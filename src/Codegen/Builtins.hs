module Codegen.Builtins where

import qualified Data.Map as M

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.FloatingPointPredicate as LLVM.FloatingPointPredicate
import qualified LLVM.AST.IntegerPredicate as LLVM.IntegerPredicate

import Typecheck.Syntax



builtins :: M.Map GlobalSymbol (Type, LLVM.Operand -> LLVM.Operand -> LLVM.Instruction)
builtins = M.fromList $ map (\(name, (type_, builtin)) -> (GlobalSymbol name [], (type_, builtin))) [
    ("store", (TypeFunction [TypePointer, TypeInt] TypeUnit, store)),
    ("+", (TypeFunction [TypeInt, TypeInt] TypeInt, add)),
    ("-", (TypeFunction [TypeInt, TypeInt] TypeInt, sub)),
    ("<", (TypeFunction [TypeInt, TypeInt] TypeBoolean, icmp LLVM.IntegerPredicate.SLT)),
    ("+.", (TypeFunction [TypeFloat, TypeFloat] TypeFloat, fadd)),
    ("-.", (TypeFunction [TypeFloat, TypeFloat] TypeFloat, fsub)),
    ("*.", (TypeFunction [TypeFloat, TypeFloat] TypeFloat, fmul)),
    ("/.", (TypeFunction [TypeFloat, TypeFloat] TypeFloat, fdiv)),
    ("<.", (TypeFunction [TypeFloat, TypeFloat] TypeBoolean, fcmp LLVM.FloatingPointPredicate.ULT))
  ]


store :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
store address value = LLVM.Store True address value Nothing 1 []


add :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
add a b = LLVM.Add False False a b []


sub :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
sub a b = LLVM.Sub False False a b []


icmp :: LLVM.IntegerPredicate.IntegerPredicate -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
icmp predicate a b = LLVM.ICmp predicate a b []


fadd :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
fadd a b = LLVM.FAdd LLVM.noFastMathFlags a b []


fsub :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
fsub a b = LLVM.FSub LLVM.noFastMathFlags a b []


fmul :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
fmul a b = LLVM.FMul LLVM.noFastMathFlags a b []


fdiv :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
fdiv a b = LLVM.FDiv LLVM.noFastMathFlags a b []


fcmp :: LLVM.FloatingPointPredicate.FloatingPointPredicate -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
fcmp predicate a b = LLVM.FCmp predicate a b []
