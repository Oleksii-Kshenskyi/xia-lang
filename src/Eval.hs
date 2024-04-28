module Eval (
    evalExpr
) where

import Parse (BinaryExpression(..), BinaryOpType(..))

evalExpr :: BinaryExpression -> Int
evalExpr (BinaryExpression op1 opType op2) = case opType of
        Plus -> op1 + op2
        Minus -> op1 - op2
        Multiply -> op1 * op2
        Divide -> op1 `div` op2
    


