module Tisp.Type where

import Control.Monad
import Control.Lens
import Data.Word
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Tisp.Tokenize
import Tisp.Value
import Tisp.Parse

data Kind = Star | KFun Kind Kind
  deriving (Eq, Ord, Show)

data TypeConstructor = TypeConstructor Symbol Kind
                     | TyBits Word32
                     | TyFloat Word32
  deriving (Eq, Show)

function :: Type
function = TyCon $ TypeConstructor "Function" (KFun Star (KFun Star Star))

data TypeVariable = TypeVariable Name Kind
  deriving (Eq, Ord, Show)

data Type = TyCon TypeConstructor
          | TyApp Type Type
          | TyVar TypeVariable
  deriving (Eq, Show)

class HasKind t where
  kind :: t -> Kind

data DataConstructor = DataConstructor [Type]
  deriving (Eq, Show)

ctorArgs :: Lens' DataConstructor [Type]
ctorArgs f (DataConstructor args) = fmap (\args' -> DataConstructor args') (f args)

data TypeDefinition = TypeDefinition Kind [TypeVariable] [(Symbol, DataConstructor)]
  deriving (Eq, Show)

instance HasKind TypeDefinition where
  kind (TypeDefinition k _ _) = k

listDef :: TypeDefinition
listDef = TypeDefinition (KFun Star Star)
                         [a]
                         [ ("nil", DataConstructor [])
                         , ("cons", DataConstructor [ TyVar a
                                                    , TyApp (TyCon (TypeConstructor "List" (KFun Star Star)))
                                                            (TyVar a)])
                         ]
  where
    a = TypeVariable "a" Star

parseTypeDef :: Map Symbol Type -> Tree -> Either (SourceLoc, Text) (Symbol, TypeDefinition)
parseTypeDef env (Tree _ (Branch ( Tree _ (Leaf (Symbol "data"))
                                 : Tree _ (Leaf (Symbol name))
                                 : Tree _ (Branch vars)
                                 : ctors))) = do
  vars' <- case symbolList vars of
             Left l -> Left (l, "expected symbol")
             Right ss -> Right (map (flip TypeVariable Star . NUser) ss)
  let k = (foldr (\_ k' -> KFun Star k') Star vars')
  ctors' <- mapM (parseCtor (M.insert name (TyCon $ TypeConstructor name k)
                                      (foldl (\e v@(TypeVariable (NUser n) _) -> M.insert n (TyVar v) e) env vars')))
                 ctors
  pure $ (name, TypeDefinition k vars' ctors')
parseTypeDef _ (Tree range _) = Left (range ^. start, "malformed type definition")

parseType :: Map Symbol Type -> Tree -> Either (SourceLoc, Text) Type
parseType env (Tree _ (Branch (t:ts))) = do
  t' <- parseType env t
  foldM (\f x -> TyApp f <$> parseType env x) t' ts
parseType _ (Tree range (Branch _)) = Left ((range ^. start), "malformed type application")
parseType env (Tree range (Leaf (Symbol s))) =
  case M.lookup s env of
    Nothing -> Left ((range ^. start), "type not in scope")
    Just t -> Right t
parseType _ (Tree range (Leaf _)) = Left ((range ^. start), "illegal literal in type")
parseType _ (Tree _ (TreeError l m)) = Left (l, m)

parseCtor :: Map Symbol Type -> Tree -> Either (SourceLoc, Text) (Symbol, DataConstructor)
parseCtor env (Tree _ (Branch (Tree _ (Leaf (Symbol name)) : argTys))) = do
  args <- mapM (parseType env) argTys
  pure $ (name, DataConstructor args)
parseCtor _ (Tree range _) = Left ((range ^. start), "malformed type constructor (should be (Name arg-types*))")

fn :: Type -> Type -> Type
fn arg ret = TyApp (TyApp function arg) ret

instance HasKind TypeConstructor where
  kind (TypeConstructor _ k) = k
  kind _ = Star

instance HasKind TypeVariable where
  kind (TypeVariable _ k) = k

instance HasKind Type where
  kind (TyCon c) = kind c
  kind (TyVar v) = kind v
  kind (TyApp t _) =
    case kind t of
      KFun _ k -> k
      _ -> error "HasKind Type kind: impossible"

newtype Subst = Subst (Map TypeVariable Type)

singleton :: TypeVariable -> Type -> Subst
singleton v t = Subst (M.singleton v t)

class Types t where
  apply :: Subst -> t -> t
  typeVars :: t -> Set TypeVariable

instance Types Type where
  apply _ t@(TyCon _) = t
  apply s (TyApp c x) = TyApp (apply s c) (apply s x)
  apply (Subst s) t@(TyVar v) = case M.lookup v s of
                                  Just t' -> t'
                                  Nothing -> t

  typeVars (TyVar v) = S.singleton v
  typeVars (TyApp c x) = typeVars c `S.union` typeVars x
  typeVars (TyCon _) = S.empty

instance Types a => Types [a] where
  apply s = map (apply s)
  typeVars = S.unions . map typeVars

instance Monoid Subst where
  mempty = Subst M.empty
  mappend l@(Subst l') (Subst r') = Subst $ M.union (M.map (apply l) r') l'

merge :: Subst -> Subst -> Maybe Subst
merge l@(Subst l') r@(Subst r') =
   if all (\v -> apply l (TyVar v) == apply r (TyVar v)) (S.toList $ M.keysSet l' `S.intersection` M.keysSet r')
     then Just . Subst $ l' `M.union` r'
     else Nothing

unifier :: Type -> Type -> Either (Text, Type, Type) Subst
unifier (TyApp c1 x1) (TyApp c2 x2) =
  do s1 <- unifier c1 c2
     s2 <- unifier (apply s1 x1) (apply s1 x2)
     pure (s2 `mappend` s1)
unifier t1@(TyVar v) t2 =
  case varBind v t2 of
    Left msg -> Left (msg, t1, t2)
    Right u -> Right u
unifier t1 t2@(TyVar v) =
  case varBind v t1 of
    Left msg -> Left (msg, t1, t2)
    Right u -> Right u
unifier (TyCon c1) (TyCon c2)
  | c1 == c2 = pure mempty
unifier t1 t2 = Left ("cannot unify", t1, t2)

varBind :: TypeVariable -> Type -> Either Text Subst
varBind v t
  | t == TyVar v            = Right mempty
  | v `S.member` typeVars t = Left "recursive type"
  | kind v /= kind t        = Left "kind mismatch"
  | otherwise               = Right (singleton v t)

primTy :: Text -> Type
primTy n = TyCon $ TypeConstructor n Star

rational :: Type
rational = primTy "Rational"

pointer :: Type
pointer = primTy "Pointer"

text :: Type
text = primTy "Text"

literalTy :: Literal -> Type
literalTy (LitNum _) = rational
literalTy (LitText _) = text
literalTy (LitForeign _) = pointer
