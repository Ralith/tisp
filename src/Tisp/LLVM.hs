module Tisp.LLVM where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Word

import LLVM.General.AST
import LLVM.General.AST.DataLayout

import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A

data Target = Target { triple :: String, dataLayout :: DataLayout }

data ModuleState = ModuleState { globalNames :: Map Text Word }

initModuleState = ModuleState { globalNames = M.empty }

newtype ModuleGen = ModuleGen { unMG :: Reader Target (State ModuleGenState) }
  deriving (Functor, Applicative, Monad, MonadReader Target, MonadState ModuleGenState)

data FunctionState = FunctionState { currentBlockName :: Maybe Text
                                   , currentBlockInstructions :: [Named Instruction]
                                   , localNames :: Map Text Word
                                   }

initFunctionState = FunctionState { currentBlockName = Nothing
                                  , currentBlockInstructions = []
                                  , localNames = M.empty
                                  }

data GeneratedFunction = GeneratedFunction { allocas :: [(Name, Instruction)], blocks :: [BasicBlock] }

instance Monoid GeneratedFunction where
  mempty = FunctionGenOutput mempty mempty
  mappend (FunctionGenOutput a b) (FunctionGenOutput a' b') = FunctionGenOutput (a `mappend` a') (b `mappend` b')

newtype FunctionGen = FunctionGen { unFG :: (Writer GeneratedFunction (StateT FunctionState ModuleGen))}
  deriving (Functor, Applicative, Monad, MonadReader Target, MonadWriter FunctionGenOutput, MonadState FunctionGenState)
