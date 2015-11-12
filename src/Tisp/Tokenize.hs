module Tisp.Tokenize
  ( Symbol
  , Atom(..)
  , SourceLoc(..), char, line, column
  , SourceRange(..), start, end
  , TokenValue(..)
  , HasRange, sourceRange
  , Token(..)
  , tokenize
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Char
import Data.Word (Word64)
import Data.Ratio

import Control.Monad.RWS
import Control.Lens

import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

type Symbol = Text

data Atom = Number Rational | AText Text | Symbol Symbol
  deriving (Show)

data SourceLoc = SourceLoc { _char :: Word64, _line :: Word64, _column :: Word64 }
  deriving (Show, Eq)
makeLenses ''SourceLoc
data SourceRange = SourceRange { _start :: SourceLoc, _end :: SourceLoc }
  deriving (Show, Eq)
makeLenses ''SourceRange

data TokState = TokState { _loc :: SourceLoc, _remaining :: Text }
  deriving (Show)
makeLenses ''TokState

class HasRange a where
  sourceRange :: Lens' a SourceRange

instance HasRange SourceRange where
  sourceRange = id

data Token = Token SourceRange TokenValue
  deriving (Show)

instance HasRange Token where
  sourceRange f (Token r v) = fmap (\r' -> Token r' v) (f r)

newtype Tokenization = Tokenization [Token]

instance Monoid Tokenization where
  mempty = Tokenization []
  mappend (Tokenization x) (Tokenization y) = Tokenization (mappend y x)

type Tok = RWS () Tokenization TokState

data TokenValue = TokError SourceLoc Text | Atom Atom | LParen | RParen
  deriving (Show)

instance Pretty Atom where
  pretty (Number n) = if denominator n == 1 then PP.integer (numerator n) else (PP.char '#' <> (PP.angles $ PP.rational n))
  pretty (AText t) = PP.text (show t)
  pretty (Symbol s) = PP.text (T.unpack s)

instance Pretty SourceLoc where
  pretty (SourceLoc _ l c) = (PP.integer . toInteger $ l) <> PP.colon <> (PP.integer . toInteger $ c)

instance Pretty SourceRange where
  pretty (SourceRange a b) = pretty a <> PP.char '-' <> pretty b

instance Pretty Token where
  pretty (Token _ v) = pretty v

instance Pretty TokenValue where
  pretty (TokError l t) = PP.angles $ pretty l <> PP.space <> PP.text (T.unpack t)
  pretty (Atom a) = pretty a
  pretty LParen = PP.lparen
  pretty RParen = PP.rparen

peek :: Tok (Maybe Char)
peek = do
  xs <- use remaining
  return $ if T.null xs
              then Nothing
              else Just (T.head xs)

advance :: Tok ()
advance = do
  Just x <- peek
  SourceLoc ch l c <- use loc
  loc .= case x of
           '\n' -> SourceLoc (ch+1) (l+1) 0
           _    -> SourceLoc (ch+1) l (c+1)
  remaining %= T.tail

tokenize :: Text -> [Token]
tokenize x = let (_, _, (Tokenization r)) = runRWS tokenize' () (TokState (SourceLoc 0 0 0) x) in r

tokenize' :: Tok ()
tokenize' = do
  c <- peek
  case c of
    Nothing -> return ()
    Just x -> do
      result <- case x of
                  '(' -> do pos <- use loc
                            advance
                            pos' <- use loc
                            (return [Token (SourceRange pos pos') LParen])
                  ')' -> do pos <- use loc
                            advance
                            pos' <- use loc
                            (return [Token (SourceRange pos pos') RParen])
                  '-' -> (: []) <$> number
                  '"' -> (: []) <$> string
                  _ | isSpace x -> advance >> return []
                    | isDigit x -> (: []) <$> number
                    | otherwise -> (: []) <$> symbol
      xs <- use remaining
      unless (T.null xs) $ tokenize'
      tell . Tokenization $ result

number :: Tok Token
number = do
  (range, xs) <- symbolText
  return $ case T.signed T.rational xs of
             Left e -> Token range (TokError (range ^. start) (T.concat ["illegal number literal: ", T.pack e]))
             Right (x, "") -> Token range (Atom . Number $ x)
             Right (_, garbage) ->
               let len = fromIntegral $ T.length garbage
               in Token range (TokError ((column -~ len) . (char -~ len) $ range ^. end) "garbage after number")

string :: Tok Token
string = do
  pos <- use loc
  advance
  value <- helper pos False
  pos' <- use loc
  pure $ Token (SourceRange pos pos')
               (case value of
                  Left (l, msg) -> TokError l msg
                  Right v -> (Atom . AText . T.pack $ v))
  where
    helper :: SourceLoc -> Bool -> Tok (Either (SourceLoc, Text) [Char])
    helper pos escape = do
      c <- peek
      l <- use loc
      case c of
        Nothing -> pure $ Left (pos, "missing terminating doublequote")
        Just x -> advance >>
          if escape
             then case lookup x [('\\', '\\')
                                ,('"', '"')
                                ,('n', '\n')
                                ,('r', '\r')
                                ,('t', '\t')
                                ] of
                    Nothing -> pure $ Left (l, "illegal escape character")
                    Just x' -> fmap (x':) <$> helper pos False
             else case x of
                    '"' -> pure $ Right []
                    '\\' -> helper pos True
                    _ -> fmap (x:) <$> helper pos False

symbol :: Tok Token
symbol = (\(range, x) -> Token range (Atom (Symbol x))) <$> symbolText

symbolText :: Tok (SourceRange, Text)
symbolText = do pos <- use loc
                x <- T.pack <$> helper
                pos' <- use loc
                return (SourceRange pos pos', x)
  where
    helper :: Tok [Char]
    helper  = do
      c <- peek
      case c of
        Just x | symbolChar x -> (x:) <$> (advance >> helper)
        _ -> return []

symbolChar :: Char -> Bool
symbolChar '(' = False
symbolChar ')' = False
symbolChar c
 | isSpace c = False
 | otherwise = True
