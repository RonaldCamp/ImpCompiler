module tipos

import Data.SortedMap

%access public export

data Id = ValID String

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | ID Id

data BExp = Equal AExp AExp | Not BExp | LT AExp AExp | GT AExp AExp | LE AExp AExp | GE AExp AExp | And BExp BExp | OR BExp BExp | Boo Bool

data Exp = AExpR AExp | BExpR BExp | Ref Exp | DeRef Id | ValRef Id

data Dec = Bind Id Exp | DSeq Dec Dec

data Cmd = Assign Id Exp | Loop BExp Cmd | Cond BExp Cmd Cmd | CSeq Cmd Cmd | Blk Dec Cmd | NOP

data CmdOp = CtrlAssign | CtrlLoop | CtrlCond | CtrlCSeq | CtrlBlkDec | CtrlBlkCmd

data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul | CtrlNot | CtrlLT | CtrlGT | CtrlLE | CtrlGE | CtrlAnd | CtrlOR | CtrlEqual | CtrlRef | CtrlCns

data DecOp = CtrlBind | CtrlDSeq

data Ctrl = CtExp Exp | CtExpOp ExpOp | CtCmd Cmd | CtDec Dec | CtCmdOp CmdOp | CtDecOp DecOp

data Loc = L Int

data Val = ValInt Int | ValBool Bool | ValId String | ValCmd Cmd | ValLoc Loc | ValListLoc (List Loc) | ValEnv (SortedMap Val Loc) |ValNop

--
-- implementation Equal Loc where
--   (L a) == (L b) = a == b
--   (L a) /= (L b) = a /= b
--
-- implementation Ord Loc where
--   compare (L a) (L b) = compare a b
--
--
-- implementation Equal Val where
--     (ValId a) == (ValId b) = a == b
--
-- implementation Ord Val where
--     compare (ValId a) (ValId b) = compare a b

implementation Show Id where
  show (ValID x) = "ValID " ++ show x

implementation Show AExp where
  show (Sum a b) = "Sum (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Sub a b) = "Sub (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Mul a b) = "Mul (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Div a b) = "Div (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (N a) = "N " ++ show a
  show (ID a) = "ID " ++ show a

implementation Show BExp where
  show (Equal a b) = "Equal (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (GT a b) = "GT (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (LT a b) = "LT (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (LE a b) = "LE (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (GE a b) = "GE (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (And a b) = "And (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (OR a b) = "OR (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
  show (Boo a) = "Boo " ++ show a
  show (Not a) = "Not " ++ show a

implementation Show Exp where
  show (AExpR a) = "AExpR (" ++ show a ++ ")"
  show (BExpR a) = "BExpR " ++ show a
  show (Ref a) = "Ref " ++ show a
  show (DeRef id) = "DeRef " ++ show id
  show (ValRef id) = "ValRef " ++ show id

implementation Show Dec where
  show (Bind a b) = "Bind " ++ show a ++ " " ++ show b
  show (DSeq a b) = "DSeq " ++ show a ++ " " ++ show b

implementation Show Cmd where
  show (Assign id exp) = "Assign " ++ show id ++ " " ++ show exp
  show (Loop b c) = "Loop " ++ show b ++ " " ++ show c
  show (Cond b c1 c2) = "Cond " ++ show b ++ " " ++ show c1 ++ " " ++ show c2
  show (CSeq c1 c2) = "CSeq " ++ show c1 ++ " " ++ show c2
  show (Blk d c) = "Blk " ++ show d ++ " " ++ show c
  show (NOP) = "NOP"

implementation Show CmdOp where
  show CtrlAssign = "CtrlAssign"
  show CtrlLoop = "CtrlLoop"
  show CtrlCond = "CtrlCond"
  show CtrlCSeq = "CtrlCSeq"
  show CtrlBlkDec = "CtrlBlkDec"
  show CtrlBlkCmd = "CtrlBlkCmd"

implementation Show ExpOp where
  show CtrlSum = "CtrlSum"
  show CtrlSub = "CtrlSub"
  show CtrlDiv = "CtrlDiv"
  show CtrlMul = "CtrlMul"
  show CtrlNot = "CtrlNot"
  show CtrlLT = "CtrlLT"
  show CtrlGT = "CtrlGT"
  show CtrlLE = "CtrlLE"
  show CtrlGE = "CtrlGE"
  show CtrlAnd = "CtrlAnd"
  show CtrlOR = "CtrlOR"
  show CtrlEqual = "CtrlEqual"
  show CtrlRef = "CtrlRef"
  show CtrlCns = "CtrlCns"

implementation Show DecOp where
  show CtrlBind = "CtrlBind"
  show CtrlDSeq = "CtrlDSeq"

implementation Show Ctrl where
  show (CtExp a) = "CtExp (" ++ show a ++ ")"
  show (CtExpOp a) = "CtExpOp " ++ show a
  show (CtCmd a) = "CtCmd " ++ show a
  show (CtDec a) = "CtDec " ++ show a
  show (CtCmdOp a) = "CtCmdOp " ++ show a
  show (CtDecOp a) = "CtDecOp " ++ show a

implementation Show Loc where
  show (L a) = "L " ++ show a

implementation Show Val where
  show (ValInt a) = "ValInt " ++ show a
  show (ValBool b) = "ValBool " ++ show b
  show (ValId a) = "ValId " ++ show a
  show (ValCmd a) = "ValCmd " ++ show a
  show (ValLoc a) = "ValLoc " ++ show a
  show (ValListLoc a) = "ValListLoc " ++ show a
  show (ValEnv map) = "ValEnv" ++ show (toList map)
  show ValNop = "ValNop"
