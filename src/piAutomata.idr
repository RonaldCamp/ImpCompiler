module piAutomata

import tipos

%access public export

calcAExp : AExp -> Int
calcAExp (Sum (N n1) (N n2)) = n1 + n2
calcAExp (Sub (N n1) (N n2)) = n1 - n2
calcAExp (Div (N n1) (N n2)) = n1 `div` n2
calcAExp (Mul (N n1) (N n2)) = n1 * n2

calcBExp : BExp -> Bool
calcBExp (Not (Boo b)) = not b
calcBExp (Eq (N n1) (N n2)) = n1 == n2
calcBExp (GE (N n1) (N n2)) = n1 >= n2
calcBExp (LE (N n1) (N n2)) = n1 <= n2
calcBExp (LT (N n1) (N n2)) = n1 < n2
calcBExp (GT (N n1) (N n2)) = n1 > n2
calcBExp (And (Boo b1) (Boo b2)) = (&&) b1 b2
calcBExp (OR (Boo b1) (Boo b2)) = (||) b1 b2



--TEST PROCESS--
-- process ([(CtExp (AExpR (Div (Sum (Sub (N 10) (N 2)) (Mul (N 4) (N 3)) ) (Mul (N 2) (N 5))) ) )],[])  -- > ((10-2) + (4*3))/(2*5)
-- process ([CtExp (BExpR (Not (Boo False)))], []) -> True
-- process ([CtExp (BExpR (Not (Boo True)))], []) -> False
-- process ([CtExp (BExpR (Eq (N 2) (N 4)))], [])  -> False
-- process ([CtExp (BExpR (Eq (N 2) (N 2)))], [])  -> True

process: (List Ctrl, List Val) -> (List Ctrl, List Val)

-- Stop Case
process ([],list) = ([],list)

-- Aritmetic Expression
process ( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSum :: xs)) ), listVal)
process ( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSub :: xs)) ), listVal)
process ( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlMul :: xs)) ), listVal)
process ( (CtExp (AExpR (Div n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlDiv :: xs)) ), listVal)
process ( (CtExp (AExpR (N     n  ))) ::xs , listVal ) = process (xs , ValInt n :: listVal)
process ( (CtExpOp CtrlSum)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calcAExp (Sum (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlSub)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calcAExp (Sub (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlMul)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calcAExp (Mul (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlDiv)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValInt (calcAExp (Div (N val2) (N val1)))) ::restoLista)

-- Boolean Expression
process ( (CtExp (BExpR (Not b))) ::xs , listVal ) = process ( (CtExp (BExpR b)) :: (CtExpOp CtrlNot :: xs) , listVal)
process ( (CtExp (BExpR (Eq n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlEq :: xs)) ), listVal)
process ( (CtExp (BExpR (GE n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGE :: xs)) ), listVal)
process ( (CtExp (BExpR (LE n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLE :: xs)) ), listVal)
process ( (CtExp (BExpR (LT n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLT :: xs)) ), listVal)
process ( (CtExp (BExpR (GT n1 n2))) ::xs , listVal ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGT :: xs)) ), listVal)
process ( (CtExp (BExpR (And b1 b2))) ::xs , listVal ) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlAnd :: xs)) ), listVal)
process ( (CtExp (BExpR (OR b1 b2))) ::xs , listVal ) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlOR :: xs)) ), listVal)
process ( (CtExp (BExpR (Boo b))) ::xs , listVal ) = process (xs , ValBool b :: listVal)
process ( (CtExpOp CtrlNot) :: xs , (ValBool b) :: restoLista ) = process (xs , ValBool (calcBExp (Not (Boo b))) :: restoLista )
process ( (CtExpOp CtrlEq)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (Eq (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlGE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (GE (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlLE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (LE (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlLT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (LT (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlGT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (GT (N val2) (N val1)))) ::restoLista)
process ( (CtExpOp CtrlAnd)::xs , (ValBool val1) :: (ValBool val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (And (Boo val2) (Boo val1)))) ::restoLista)
process ( (CtExpOp CtrlOR)::xs , (ValBool val1) :: (ValBool val2 :: restoLista) ) = process (xs, (ValBool (calcBExp (OR (Boo val2) (Boo val1)))) ::restoLista)

--Commands
process ( (CtCmd (Assign c1 c2)) ::xs , listVal ) = process (c2::(CtCmdOp CtrlAssign::xs), c1::listVal)
