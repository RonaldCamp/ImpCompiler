module tipos

import Data.SortedMap

%access public export

data Id = ValID String

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | ID Id

data BExp = Eq AExp AExp | Not BExp | LT AExp AExp | GT AExp AExp | LE AExp AExp | GE AExp AExp | And BExp BExp | OR BExp BExp | Boo Bool

data Exp = AExpR AExp | BExpR BExp | Ref Exp | DeRef Id | ValRef Id

data Dec = Bind Id Exp | DSeq Dec Dec

data Cmd = Assign Id Exp | Loop BExp Cmd | Cond BExp Cmd Cmd | CSeq Cmd Cmd | Blk Dec Cmd | NOP

data CmdOp = CtrlAssign | CtrlLoop | CtrlCond | CtrlCSeq | CtrlBlkDec | CtrlBlkCmd

data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul | CtrlNot | CtrlLT | CtrlGT | CtrlLE | CtrlGE | CtrlAnd | CtrlOR | CtrlEq | CtrlRef

data DecOp = CtrlBind | CtrlDSeq

data Ctrl = CtExp Exp | CtExpOp ExpOp | CtCmd Cmd | CtDec Dec | CtCmdOp CmdOp | CtDecOp DecOp

data Loc = L Int

data Val = ValInt Int | ValBool Bool | ValId String | ValCmd Cmd | ValLoc Loc | ValListLoc (List Loc) | ValEnv (SortedMap Val Loc) |ValNop



implementation Eq Loc where
  (L a) == (L b) = a == b
  (L a) /= (L b) = a /= b

implementation Ord Loc where
  compare (L a) (L b) = compare a b

implementation Eq Val where
    (ValId a) == (ValId b) = a == b

implementation Ord Val where
    compare (ValId a) (ValId b) = compare a b
