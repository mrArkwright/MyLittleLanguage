module Codegen.Lib where

import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Type as LLVM

import Misc
import Typecheck.Syntax



constantOperand :: MonadError Error m => Symbol -> Type -> m LLVM.Operand
constantOperand symbol type_ = do
  type_' <- typeToLlvmType type_
  return $ LLVM.ConstantOperand $ LLVM.Constant.GlobalReference type_' (LLVM.Name $ B.toShort $ BC.pack $ show symbol)


typeToLlvmType :: MonadError Error m => Type -> m LLVM.Type
typeToLlvmType TypeUnit = return LLVM.VoidType
typeToLlvmType TypeBoolean = return $ LLVM.IntegerType 32
typeToLlvmType TypeInt = return $ LLVM.IntegerType 32
typeToLlvmType TypeFloat = return $ LLVM.FloatingPointType LLVM.DoubleFP
typeToLlvmType (TypeFunction parameterTypes resultType) = do
  parameterTypes' <- mapM typeToLlvmType parameterTypes
  resultType' <- typeToLlvmType resultType
  return $ LLVM.ptr $ LLVM.FunctionType resultType' parameterTypes' False


type SymbolTable = M.Map Symbol (Type, LLVM.Operand)
