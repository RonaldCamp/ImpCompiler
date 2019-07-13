module tipos

import Data.SortedMap

%access public export

mutual
  data Id = ValID String

  data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | IdA Id

  data BExp = Equal AExp AExp | Not BExp | LT AExp AExp | GT AExp AExp | LE AExp AExp | GE AExp AExp | And BExp BExp | OR BExp BExp | Boo Bool | IdB Id

  data Exp = AExpR AExp | BExpR BExp | Ref Exp | DeRef Id | ValRef Id | Cns Exp

  data Formals = Form (List Id)

  data Actuals = Act (List Exp)

  data Cmd = Assign Id Exp | Loop BExp Cmd | Cond BExp Cmd Cmd | CSeq Cmd Cmd | Blk Dec Cmd | Call Id Actuals | NOP

  data Abst = Abstr Formals Cmd

  data Dec = Bind Id Exp | DSeq Dec Dec | BindF Id Abst | Rbnd Id Abst

  data CmdOp = CtrlAssign | CtrlLoop | CtrlCond | CtrlCSeq | CtrlBlkDec | CtrlBlkCmd | CtrlCall Id Nat

  data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul | CtrlNot | CtrlLT | CtrlGT | CtrlLE | CtrlGE | CtrlAnd | CtrlOR | CtrlEq | CtrlRef | CtrlCns

  data DecOp = CtrlBind | CtrlDSeq | CtrlBindF | CtrlRbnd

  data Ctrl = CtExp Exp | CtExpOp ExpOp | CtCmd Cmd | CtDec Dec | CtCmdOp CmdOp | CtDecOp DecOp | CtAbs Abst

  data Loc = L Int

  data Bindable = BindLoc Loc | BindInt Int | BindClos Closure

  data Val = ValInt Int | ValBool Bool | ValId String | ValCmd Cmd | ValLoc Loc | ValListLoc (List Loc) | ValEnv (SortedMap Id Bindable) | ValBindable Bindable | ValClos Closure | ValNop

  data Closure = Clos (Formals, Cmd, (SortedMap Id Bindable))

  implementation Eq Loc where
    (L a) == (L b) = a == b
    (L a) /= (L b) = a /= b

  implementation Ord Loc where
    compare (L a) (L b) = compare a b

  implementation Eq Id where
    (ValID a) == (ValID b) = a == b
    (ValID a) /= (ValID b) = a /= b

  implementation Ord Id where
    compare (ValID a) (ValID b) = compare a b


  implementation Show Id where
    show (ValID x) = "ValID " ++ show x

  implementation Show AExp where
    show (Sum a b) = "Sum (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (Sub a b) = "Sub (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (Mul a b) = "Mul (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (Div a b) = "Div (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (N a) = "N " ++ show a
    show (IdA a) = "IdA (" ++ show a ++ ")"

  implementation Show BExp where
    show (Equal a b) = "Equal (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (GT a b) = "GT (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (LT a b) = "LT (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (LE a b) = "LE (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (GE a b) = "GE (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (And a b) = "And (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (OR a b) = "OR (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (Boo a) = "Boo " ++ show a
    show (Not a) = "Not (" ++ show a ++ ")"
    show (IdB a) = "IdB (" ++ show a ++ ")"

  implementation Show Exp where
    show (AExpR a) = "AExpR (" ++ show a ++ ")"
    show (BExpR a) = "BExpR (" ++ show a ++ ")"
    show (Ref a) = "Ref (" ++ show a ++ ")"
    show (DeRef id) = "DeRef (" ++ show id ++ ")"
    show (ValRef id) = "ValRef (" ++ show id ++ ")"

  implementation Show Actuals where
    show (Act x) = "Act " ++ show x

  implementation Show Formals where
    show (Form x) = "Form " ++ show x

  implementation Show Abst where
    show (Abstr b c) = "Abstr (" ++ show b ++ ") " ++ "(" ++ show c ++ ")"

  implementation Show Dec where
    show (Bind a b) = "Bind (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (DSeq a b) = "DSeq (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (BindF a b) = "BindF (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"
    show (Rbnd a b) = "Rbnd (" ++ show a ++ ") " ++ "(" ++ show b ++ ")"

  implementation Show Cmd where
    show (Assign id exp) = "Assign (" ++ show id ++ ") " ++ "("++ show exp ++ ")"
    show (Loop b c) = "Loop (" ++ show b ++ ") " ++ "("++ show c ++ ")"
    show (Cond b c1 c2) = "Cond (" ++ show b ++ ") " ++ "("++ show c1 ++ ")" ++ " (" ++ show c2 ++ ")"
    show (CSeq c1 c2) = "CSeq (" ++ show c1 ++ ") " ++ "("++ show c2 ++ ")"
    show (Blk d c) = "Blk (" ++ show d ++ ") " ++ "("++ show c ++ ")"
    show (Call d c) = "Call (" ++ show d ++ ") " ++ "("++ show c ++ ")"
    show (NOP) = "NOP"

  implementation Show Closure where
    show (Clos (f, c, e)) = "Clos (" ++ show f ++ ", " ++ show c ++ ", " ++ show (toList e) ++ ")"

  implementation Show CmdOp where
    show CtrlAssign = "CtrlAssign"
    show CtrlLoop = "CtrlLoop"
    show CtrlCond = "CtrlCond"
    show CtrlCSeq = "CtrlCSeq"
    show CtrlBlkDec = "CtrlBlkDec"
    show CtrlBlkCmd = "CtrlBlkCmd"
    show (CtrlCall id tam) = "CtrlCall (" ++ show id ++ ") " ++ "(" ++ show tam ++ ")"

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
    show CtrlEq = "CtrlEq"
    show CtrlRef = "CtrlRef"
    show CtrlCns = "CtrlCns"

  implementation Show DecOp where
    show CtrlBind = "CtrlBind"
    show CtrlDSeq = "CtrlDSeq"
    show CtrlBindF = "CtrlBindF"
    show CtrlRbnd = "CtrlRbnd"

  implementation Show Ctrl where
    show (CtExp a) = "CtExp (" ++ show a ++ ")"
    show (CtExpOp a) = "CtExpOp (" ++ show a ++ ")"
    show (CtCmd a) = "CtCmd (" ++ show a ++ ")"
    show (CtDec a) = "CtDec " ++ show a
    show (CtAbs a) = "CtAbs " ++ show a
    show (CtCmdOp a) = "CtCmdOp (" ++ show a ++ ")"
    show (CtDecOp a) = "CtDecOp (" ++ show a ++ ")"

  implementation Show Loc where
    show (L a) = "L " ++ show a

  implementation Show Bindable where
    show (BindLoc x) = "BindLoc (" ++ show x ++ ")"
    show (BindInt x) = "BindInt (" ++ show x ++ ")"
    show (BindClos x) = "BindClos (" ++ show x ++ ")"

  implementation Show Val where
    show (ValInt a) = "ValInt (" ++ show a ++ ")"
    show (ValBool b) = "ValBool (" ++ show b ++ ")"
    show (ValId a) = "ValId (" ++ show a ++ ")"
    show (ValCmd a) = "ValCmd (" ++ show a ++ ")"
    show (ValLoc a) = "ValLoc (" ++ show a ++ ")"
    show (ValListLoc a) = "ValListLoc (" ++ show a ++ ")"
    show (ValEnv map) = "ValEnv (" ++ show (toList map) ++ ")"
    show (ValClos cls) = "ValClos (" ++ show cls ++ ")"
    show ValNop = "ValNop"
