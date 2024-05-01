module Eval (
    evalExpr
) where

import Parse (BinaryOpType(..), Expression(..), UnaryOpType(..))

evalUnaryExpression :: UnaryOpType -> Int -> Int
evalUnaryExpression opType op1 = case opType of
    UPlus -> op1
    UMinus -> -op1

evalBinaryExpression :: Int -> BinaryOpType -> Int -> Int
evalBinaryExpression op1 opType op2 = case opType of
        Plus -> op1 + op2
        Minus -> op1 - op2
        Multiply -> op1 * op2
        Divide -> op1 `div` op2

evalExpr :: Expression -> Int
evalExpr (BinaryExpression op1 opType op2) = evalBinaryExpression (evalExpr op1) opType (evalExpr op2)
evalExpr (UnaryExpression opType op1) = evalUnaryExpression opType (evalExpr op1)
evalExpr (IntLiteral i) = i

