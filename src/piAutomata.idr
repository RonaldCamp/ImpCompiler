module piAutomata

import tipos
import Data.SortedMap

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


----------------SortedMaps---------------------------------
--fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)]

--fromList [ (L 1, ValId "x") , (L 2, ValId "y") , (L 3, ValId "z") ]

----------------------------------------------------------------------

--TEST PROCESS--
-- process ([(CtExp (AExpR (Div (Sum (Sub (N 10) (N 2)) (Mul (N 4) (N 3)) ) (Mul (N 2) (N 5))) ) )],[],empty,[])  -- > ((10-2)+(4*3)) / (2*5)  = 2
-- process ([CtExp (BExpR (Not (Boo False)))], [], empty, []) -> True
-- process ([CtExp (BExpR (Not (Boo True)))], [], empty, []) -> False
-- process ([CtExp (BExpR (Eq (N 2) (N 4)))], [], empty, [])  -> False
-- process ([CtExp (BExpR (Eq (N 2) (N 2)))], [], empty, [])  -> True
-- process ([CtCmd (Assign (ID "x") (AExpR (Div (Sum (Sub (N 10) (N 2)) (Mul (N 4) (N 3)) ) (Mul (N 2) (N 5))) ))] , [], fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)], empty)

-------------------------------------------------------------------------------------
-- process ( [CtCmd (CSeq (Assign (ID "x") (AExpR (N 5))) (CSeq (Assign (ID"y") (AExpR (N 3))) (Loop (GT (ID "x") (N 2)) (CSeq (Assign (ID "y") (AExpR (Sum (ID "y") (N 10)))) (Assign (ID "x") (AExpR (Sub (ID "x") (N 1))))))))],[],fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)], empty)
-- x = 5
-- y = 3
-- while x>2
--   y = y+10
--   x = x-1
--------------------------------------------------------------------------------------

-- process ([(CtCmd (CSeq (Assign (ID "y") (AExpR (N 3))) (Loop (GT (ID "y") (N 2)) ((Assign (ID "y") (AExpR (Sub (ID "y") (N 1) )) ))) ))], [], fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)], empty)
-- process ([CtCmd (CSeq (Assign (ID "x") (AExpR (N 2)) ) (Assign (ID "x") ( AExpR (Sub (ID "x") (N 1)) ) ) )], [], fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)], empty)

lookup': Val -> SortedMap Val Loc -> Maybe Loc
lookup' v map =  lookup v map

lookup'': Maybe Loc -> SortedMap Loc Val -> Val
lookup'' (Just loc) sto =  transforma (lookup loc sto) where
  transforma:Maybe Val -> Val
  transforma (Just val) = val

inserir : Maybe Loc -> Val -> SortedMap Loc Val -> SortedMap Loc Val
inserir (Just loc) v stored = insert loc v stored

print: IO ()
print = putStrLn "Hello World Idris"

process: (List Ctrl, List Val, SortedMap Val Loc , SortedMap Loc Val) -> (List Ctrl, List Val, SortedMap Val Loc , SortedMap Loc Val)

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
process ( (CtCmd (Assign (ID c1) c2)) ::xs , listVal, env, stored) = process (CtExp c2 ::(CtCmdOp CtrlAssign::xs), ValId c1::listVal, env, stored)
process ( (CtExp (AExpR (ID c1) )) ::xs , listVal, env, stored) = process (xs, (lookup'' (lookup' (ValId c1) (env)) (stored) )::listVal, env, stored)
process ( (CtCmd (Loop b c)) ::xs , listVal, env, stored) = process (CtExp (BExpR b) ::(CtCmdOp CtrlLoop::xs), ValCmd (Loop b c)::listVal, env, stored)
process ( (CtCmd (Cond b c1 c2)) ::xs , listVal, env, stored) = process (CtExp (BExpR b) ::(CtCmdOp CtrlCond::xs), ValCmd (Cond b c1 c2)::listVal, env, stored)
process ( (CtCmd (CSeq c1 c2)) ::xs , listVal, env, stored) = process (CtCmd c1::(CtCmd c2::xs), listVal, env, stored)

process ( (CtCmdOp CtrlAssign :: xs ,  v1 :: (v2 ::listVal), env, stored)) = process (xs, listVal, env, inserir (lookup' v2 env) (v1) stored)
process ( (CtCmdOp CtrlLoop :: xs , ValBool True :: (ValCmd (Loop b2 c) :: listVal), env, stored)) = process (CtCmd c ::(CtCmd (Loop b2 c)::xs), listVal, env, stored)
process ( (CtCmdOp CtrlLoop :: xs , ValBool False :: (ValCmd (Loop b2 c) :: listVal), env, stored)) = process (xs, listVal, env, stored)
process ( (CtCmdOp CtrlCond :: xs , ValBool True :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored)) = process (CtCmd c1 ::xs, listVal, env, stored)
process ( (CtCmdOp CtrlCond :: xs , ValBool False :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored)) = process (CtCmd c2 ::xs, listVal, env, stored)
