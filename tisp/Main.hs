module Main where

import qualified Data.Text as T
import Control.Monad.IO.Class

import Text.PrettyPrint.ANSI.Leijen (pretty, putDoc)

import System.Console.Haskeline

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST
import Tisp.Expr

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "> "
      case input of
        Nothing -> pure ()
        Just text -> do
          let toks = tokenize (T.pack text)
              tree = parse toks
          case tree of
            Nothing -> outputStrLn "parse error"
            Just (t, _, _) -> do
              let expr = fromAST . fromTree $ t
              liftIO $ (putDoc . pretty . toAST . normalize Strong $ expr) >> putStrLn ""
          loop
