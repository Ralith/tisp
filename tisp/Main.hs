module Main where

import qualified Data.ByteString as BS

import Data.Text.Encoding (decodeUtf8)

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST
import Tisp.Expr

main :: IO ()
main = do
  text <- decodeUtf8 <$> BS.getContents
  let toks = tokenize text
      tree = parse toks
  case tree of
    Nothing -> return ()
    Just (t, _, _) -> do
      let expr = fromAST . fromTree $ t
      print (reify (eval expr))
