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
-- process ([(CtExp (AExpR (Div (Sum (Sub (N 10) (N 2)) (Mul (N 4) (N 3)) ) (Mul (N 2) (N 5))) ) )],[],[],[])  -- > ((10-2)+(4*3)) / (2*5)
-- process ([CtExp (BExpR (Not (Boo False)))], [], [], []) -> True
-- process ([CtExp (BExpR (Not (Boo True)))], [], [], []) -> False
-- process ([CtExp (BExpR (Eq (N 2) (N 4)))], [], [], [])  -> False
-- process ([CtExp (BExpR (Eq (N 2) (N 2)))], [], [], [])  -> True

process: (List Ctrl, List Val, List Val, List Val) -> (List Ctrl, List Val, List Val, List Val)

-- Stop Case
process ([],list, env, stored) = ([],list, env, stored)

-- Aritmetic Expression
process ( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSum :: xs)) ), listVal, env, stored)
process ( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSub :: xs)) ), listVal, env, stored)
process ( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlMul :: xs)) ), listVal, env, stored)
process ( (CtExp (AExpR (Div n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlDiv :: xs)) ), listVal, env, stored)
process ( (CtExp (AExpR (N     n  ))) ::xs , listVal, env, stored ) = process (xs , ValInt n :: listVal, env, stored)
process ( (CtExpOp CtrlSum)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Sum (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlSub)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Sub (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlMul)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Mul (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlDiv)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Div (N val2) (N val1)))) ::restoLista, env, stored)

-- Boolean Expression
process ( (CtExp (BExpR (Not b))) ::xs , listVal, env, stored ) = process ( (CtExp (BExpR b)) :: (CtExpOp CtrlNot :: xs) , listVal, env, stored)
process ( (CtExp (BExpR (Eq n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlEq :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (GE n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGE :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (LE n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLE :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (LT n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLT :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (GT n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGT :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (And b1 b2))) ::xs , listVal, env, stored ) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlAnd :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (OR b1 b2))) ::xs , listVal, env, stored ) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlOR :: xs)) ), listVal, env, stored)
process ( (CtExp (BExpR (Boo b))) ::xs , listVal, env, stored ) = process (xs , ValBool b :: listVal, env, stored)
process ( (CtExpOp CtrlNot) :: xs , (ValBool b) :: restoLista, env, stored ) = process (xs , ValBool (calcBExp (Not (Boo b))) :: restoLista , env, stored)
process ( (CtExpOp CtrlEq)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (Eq (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlGE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (GE (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlLE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (LE (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlLT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (LT (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlGT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (GT (N val2) (N val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlAnd)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (And (Boo val2) (Boo val1)))) ::restoLista, env, stored)
process ( (CtExpOp CtrlOR)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (OR (Boo val2) (Boo val1)))) ::restoLista, env, stored)

--Commands
process ( (CtCmd (Assign c1 c2)) ::xs , listVal, env, stored) = process (CtExp c2 ::(CtCmdOp CtrlAssign::xs), ValId c1::listVal, env, stored)
