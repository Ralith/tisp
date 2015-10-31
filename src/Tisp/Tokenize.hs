module Tisp.Tokenize
  ( Symbol
  , Atom(..)
  , SourceLoc(..), char, line, column
  , SourceRange(..), start, end
  , TokenValue(..)
  , FromSource, sourceRange
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

data Atom = Number Rational | Symbol Symbol
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

class FromSource a where
  sourceRange :: Lens' a SourceRange

data Token = Token SourceRange TokenValue
  deriving (Show)

instance FromSource Token where
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
             Left e -> Token range (TokError (range ^. start) (T.pack e))
             Right (x, "") -> Token range (Atom . Number $ x)
             Right (_, garbage) ->
               let len = fromIntegral $ T.length garbage
               in Token range (TokError ((column -~ len) . (char -~ len) $ range ^. end) "garbage after number")

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
