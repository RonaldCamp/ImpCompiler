module tipos

%access public export

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | Id String

data BExp = Eq AExp AExp | Not BExp | LT AExp AExp | GT AExp AExp | LE AExp AExp | GE AExp AExp | And BExp BExp | OR BExp BExp | Boo Bool

data Exp = AExpR AExp | BExpR BExp

data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul | CtrlNot | CtrlLT | CtrlGT | CtrlLE | CtrlGE | CtrlAnd | CtrlOR | CtrlEq

data Ctrl = CtExp Exp | CtExpOp ExpOp

data Val = ValInt Int | ValBool Bool
