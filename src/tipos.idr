module tipos

%access public export

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int

data BExp = Eq AExp AExp | Not BExp | LT AExp AExp | GT AExp AExp | LE AExp AExp | GE AExp AExp | And BExp BExp | OR BExp BExp | Boo Bool

data Exp = AExpR AExp | BExpR BExp

data Id = ID String

data Cmd = Assign Id Exp | Loop BExp Cmd | Cond BExp Cmd Cmd | CSeq Cmd Cmd

data CmdOp = CtrlAssign | CtrlLoop | CtrlCond | CtrlCSeq

data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul | CtrlNot | CtrlLT | CtrlGT | CtrlLE | CtrlGE | CtrlAnd | CtrlOR | CtrlEq

data Ctrl = CtExp Exp | CtExpOp ExpOp | CtCmd Cmd | CtCmdOp CmdOp

data Val = ValInt Int | ValBool Bool | ValId Id
