module piAutomata

%access public export


data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int

data BExp = Eq BExp BExp

data Exp = AExpR AExp | BExpR BExp

data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul

data Ctrl = CtExp Exp | CtExpOp ExpOp

data Val = ValInt Int


calc : AExp -> Int
calc (Sum (N n1) (N n2)) = n1 + n2
calc (Sub (N n1) (N n2)) = n1 - n2
calc (Div (N n1) (N n2)) = n1 `div` n2
calc (Mul (N n1) (N n2)) = n1 * n2


process: (List Ctrl, List Val) -> (List Ctrl, List Val)
process ([],list) = ([],list)
process ( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSum :: xs)) ), listVal)
process ( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSum :: xs)) ), listVal)
process ( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSub :: xs)) ), listVal)
process ( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSub :: xs)) ), listVal)
process ( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlMul :: xs)) ), listVal)
process ( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlMul :: xs)) ), listVal)
process ( (CtExp (AExpR (Div n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlDiv :: xs)) ), listVal)
process ( (CtExp (AExpR (Div n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlDiv :: xs)) ), listVal)
process ( (CtExp (AExpR (N     n  ))) ::xs , listVal ) = process (xs , ValInt n :: listVal)
process ( (CtExpOp CtrlSum)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calc (Sum (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlSub)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calc (Sub (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlMul)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calc (Mul (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlDiv)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calc (Div (N val2) (N val1)))) ::restoLista)
