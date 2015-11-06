module Main where

import qualified Data.ByteString as BS

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Control.Monad

import Text.PrettyPrint.ANSI.Leijen (pretty, putDoc, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Tisp.Parse
import Tisp.Tokenize
import Tisp.AST
import Tisp.Expr
import Tisp.Value
import Tisp.Type

main :: IO ()
main = do
  text <- decodeUtf8 <$> BS.getContents
  let toks = tokenize text
      tree = parse toks
  -- putDoc (pretty toks)
  -- putStrLn ""
  case tree of
    Nothing -> return ()
    Just (t, _, _) -> do
      let typed = infer . fromAST primTys . fromTree $ t
      print typed
      print . exprTy $ typed
      forM_ (errors typed) $ \ (_, _, msg) -> do
        BS.putStr . encodeUtf8 . T.concat $ ["ERROR: ", msg, "\n"]
      -- case eval defaultEnv (fromAST ast) of
      --   VLiteral l -> print l
      --   VError _ _ msg -> putStrLn "ERROR:" >> BS.putStr (encodeUtf8 msg) >> putStrLn ""
      --   VFunc _ -> putStrLn "#<function>"
      -- putStrLn ""
      -- let record = buildRecord [t]
      -- forM_ (M.toList . fst $ record) $ \(name, Definition _ ty value) -> do
      --   putDoc $ (PP.text (T.unpack name)) <+> (pretty ty) <+> (pretty value)
      -- putStrLn ""
