module Codegen.Utils where

import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Type as LLVM

import Utils
import Typecheck.Syntax



constantOperand :: MonadError Error m => Symbol -> Type -> m LLVM.Operand
constantOperand symbol type_ = do
  type_' <- typeToLlvmType type_
  return $ LLVM.ConstantOperand $ LLVM.Constant.GlobalReference type_' (LLVM.Name $ B.toShort $ BC.pack $ show symbol)


constantPointerOperand :: MonadError Error m => Symbol -> Type -> m LLVM.Operand
constantPointerOperand symbol type_ = do
  type_' <- typeToLlvmType type_
  let type_'' = LLVM.PointerType type_' (LLVM.AddrSpace 0)
  return $ LLVM.ConstantOperand $ LLVM.Constant.GlobalReference type_'' (LLVM.Name $ B.toShort $ BC.pack $ show symbol)


typeToLlvmType :: MonadError Error m => Type -> m LLVM.Type
typeToLlvmType TypeUnit = return LLVM.VoidType
typeToLlvmType TypePointer = return $ LLVM.PointerType (LLVM.IntegerType 32) (LLVM.AddrSpace 0)
typeToLlvmType TypeBoolean = return $ LLVM.IntegerType 8
typeToLlvmType TypeInt = return $ LLVM.IntegerType 32
typeToLlvmType TypeInt8 = return $ LLVM.IntegerType 8
typeToLlvmType TypeFloat = return $ LLVM.FloatingPointType LLVM.DoubleFP
typeToLlvmType (TypeFunction parameterTypes resultType) = do
  parameterTypes' <- mapM typeToLlvmType parameterTypes
  resultType' <- typeToLlvmType resultType
  return $ LLVM.ptr $ LLVM.FunctionType resultType' parameterTypes' False


expressionLoc :: Expression -> Loc
expressionLoc (Unit _ loc) = loc
expressionLoc (LiteralExpression _ _ loc) = loc
expressionLoc (SymbolReference _ _ loc) = loc
expressionLoc (Call _ _ _ loc) = loc
expressionLoc (If _ _ _ _ loc) = loc
expressionLoc (Do _ _ loc) = loc


type SymbolTable = M.Map Symbol SymbolProperties


data SymbolProperties = SymbolProperties {
    symbolProperties_type_ :: Type,
    symbolProperties_operand :: LLVM.Operand,
    symbolProperties_indirectAccess :: Bool -- specifies wether the operand needs to be accesses indirectly (i.e. with a load instruction)
  } deriving Show


phase :: Phase
phase = PhaseCodegen
