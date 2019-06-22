module piAutomata

import tipos
import Data.SortedMap
import Control.Monad.Identity
import Control.Monad.Writer

%access public export


diga : a -> Writer (List a) ()
diga a = tell [a]

calcAExp : AExp -> Int
calcAExp (Sum (N n1) (N n2)) = n1 + n2
calcAExp (Sub (N n1) (N n2)) = n1 - n2
calcAExp (Div (N n1) (N n2)) = n1 `div` n2
calcAExp (Mul (N n1) (N n2)) = n1 * n2

calcBExp : BExp -> Bool
calcBExp (Not (Boo b)) = not b
calcBExp (Equal (N n1) (N n2)) = n1 == n2
calcBExp (GE (N n1) (N n2)) = n1 >= n2
calcBExp (LE (N n1) (N n2)) = n1 <= n2
calcBExp (LT (N n1) (N n2)) = n1 < n2
calcBExp (GT (N n1) (N n2)) = n1 > n2
calcBExp (And (Boo b1) (Boo b2)) = (&&) b1 b2
calcBExp (OR (Boo b1) (Boo b2)) = (||) b1 b2

transforma:Maybe a -> a
transforma (Just v) = v


-- retorna o maior Loc da lista de Locs
maxList: List Loc -> Int
maxList [] = 0
maxList (x :: xs) = auxiliar x xs where
  auxiliar: Loc -> List Loc -> Int
  auxiliar (L x) [] = x
  auxiliar x  (l :: ls) = if x > l then auxiliar x ls  else auxiliar l ls

-- retorna o maior Loc do mapa + 1
getLocPlus1FromMap: SortedMap Loc Val -> Loc
getLocPlus1FromMap map = L ((maxList (keys map)) + 1)


-- Retorna um stored com a adição de uma nova location como chave e um valor v
extendStored: SortedMap Loc Val -> Val -> SortedMap Loc Val
extendStored map v = insert (getLocPlus1FromMap map) (v) map


-- Atualiza segundo ambiente com os valores do primeiro, e cria novas chave-valor caso nao exista
-- ex: addIntersectionNewEnv ([ (ValId "k", L 3) , (ValId "y", L 4) , (ValId "z", L 5)]) ([ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)])
addIntersectionNewEnv: List (Id,Bindable)-> List (Id,Bindable) -> SortedMap Id Bindable
addIntersectionNewEnv [] env = fromList env
addIntersectionNewEnv ( (key,value) :: e) env = addIntersectionNewEnv e (toList (insert key value (fromList env)))


-- deleta todos as chave-valor da memoria dada uma lista de chaves (Loc)
deleteLocsStore: List Loc -> SortedMap Loc Val -> SortedMap Loc Val
deleteLocsStore [] store = store
deleteLocsStore (x :: xs) store = deleteLocsStore xs (delete x store)

----------------SortedMaps---------------------------------
--fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)]

--fromList [ (L 1, ValId "x") , (L 2, ValId "y") , (L 3, ValId "z") ]

-- [L 1, L 2, L 6, L 3]

------------------------ Casos de Teste -------------------------------------------------------------
--process ( [CtCmd (Blk (Bind (ValID "x") (Ref (AExpR (N 5)))) (Blk (Bind (ValID "y") (Ref (AExpR (N 3)))) (Loop (GT (ID (ValID "x")) (N 2)) (CSeq (Assign (ValID "y") (AExpR (Sum (ID (ValID "y")) (N 10)))) (Assign (ValID "x") (AExpR (Sub (ID (ValID "x")) (N 1)))))) ))],[], empty, empty, []) []
-- let var x = 5 in
--  let var y = 3 in
--    while x>2
--      y := y+10
--      x := x-1

-- process ([CtCmd (Blk (Bind (ValID "x") (Ref (AExpR (N 5)))) (Assign (ValID "x") (AExpR (Sum (N 1) (ID (ValID "x"))))))], [], empty, empty, []) []
-- let var x := 5 in
--   x:= 1+x

-- process ([CtCmd (Blk (DSeq (Bind (ValID "x")  (AExpR (N 5))) (Bind (ValID "y")  (Ref (AExpR (N 0))))) (Assign (ValID "y") (AExpR (Sum (N 1) (ID (ValID "x"))))))], [], empty, empty, []) []
-- let cons x := 5, var y:=0 in
--   y:= 1+x
--------------------------------------------------------------------------------------

--[CtCmd (Blk (Bind (ValID "x") (Ref (AExpR (N 5)))) (Blk (Bind (ValID "y") (Ref (AExpR (N 3)))) (Loop (GT (ID (ValID "x")) (N 2)) (CSeq (Assign (ValID "y") (AExpR (Sum (ID (ValID "y")) (N 10)))) (Assign (ValID "x") (AExpR (Sub (ID (ValID "x")) (N 1)))))) ))]

printPi: (List Ctrl, List Val, SortedMap Val Loc , SortedMap Loc Val, List Loc) -> String
printPi (listCrtl, listaVal, env, sto, listLoc) = show listCrtl ++ show listaVal ++ show (toList env) ++ show (toList sto) ++ show listLoc


getLocFromValLoc: Val -> Loc
getLocFromValLoc (ValLoc l) = l


lookup': Maybe Bindable -> SortedMap Loc Val -> Val
lookup' (Just (BindLoc loc)) sto =  transforma (lookup loc sto)
lookup' Nothing sto = ValNop

inserir : Maybe Bindable -> Val -> SortedMap Loc Val -> SortedMap Loc Val
inserir (Just (BindLoc loc)) v stored = insert loc v stored
inserir Nothing v stored = stored

process: (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> List (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> ((List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc), List (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc))

-- Stop Case
process ([],[], env, stored, listLoc) list = (([],[], env, stored, listLoc), list)

-- Aritmetic Expression
process ( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSum :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal, env, stored, listLoc)::list)

process ( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSub :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlMul :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (AExpR (Div n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlDiv :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (AExpR (Div n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (AExpR (N     n  ))) ::xs , listVal, env, stored, listLoc) (list) = process (xs , ValInt n :: listVal, env, stored, listLoc) (( (CtExp (AExpR (N     n  ))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExpOp CtrlSum)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValInt (calcAExp (Sum (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlSum)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlSub)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValInt (calcAExp (Sub (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlSub)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlMul)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValInt (calcAExp (Mul (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlMul)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlDiv)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValInt (calcAExp (Div (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlDiv)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)

-- Boolean Expression
process ( (CtExp (BExpR (Not b))) ::xs , listVal, env, stored, listLoc) (list) = process ( (CtExp (BExpR b)) :: (CtExpOp CtrlNot :: xs) , listVal, env, stored, listLoc) (( (CtExp (BExpR (Not b))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (Equal n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlEq :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (Equal n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (GE n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGE :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (GE n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (LE n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLE :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (LE n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (LT n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLT :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (LT n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (GT n1 n2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGT :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (GT n1 n2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (And b1 b2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlAnd :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (And b1 b2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (OR b1 b2))) ::xs , listVal, env, stored, listLoc) (list) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlOR :: xs)) ), listVal, env, stored, listLoc) (( (CtExp (BExpR (OR b1 b2))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (BExpR (Boo b))) ::xs , listVal, env, stored, listLoc) (list) = process (xs , ValBool b :: listVal, env, stored, listLoc) (( (CtExp (BExpR (Boo b))) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExpOp CtrlNot) :: xs , (ValBool b) :: restoLista, env, stored, listLoc) (list) = process (xs , ValBool (calcBExp (Not (Boo b))) :: restoLista , env, stored, listLoc) (( (CtExpOp CtrlNot) :: xs , (ValBool b) :: restoLista, env, stored, listLoc)::list)
process ( (CtExpOp CtrlEq)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (Equal (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlEq)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlGE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (GE (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlGE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlLE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (LE (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlLE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlLT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (LT (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlLT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlGT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (GT (N val2) (N val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlGT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlAnd)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (And (Boo val2) (Boo val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlAnd)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored, listLoc)::list)
process ( (CtExpOp CtrlOR)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored, listLoc) (list) = process (xs, (ValBool (calcBExp (OR (Boo val2) (Boo val1)))) ::restoLista, env, stored, listLoc) (( (CtExpOp CtrlOR)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored, listLoc)::list)

--Commands
process ( (CtCmd (Assign (ValID c1) c2)) ::xs , listVal, env, stored, listLoc) (list) = process (CtExp c2 ::(CtCmdOp CtrlAssign::xs), ValId c1::listVal, env, stored, listLoc) (( (CtCmd (Assign (ValID c1) c2)) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtExp (AExpR (ID (ValID c1)) )) ::xs , listVal, env, stored, listLoc) (list) = let loc = lookup (ValID c1) env in case loc of
  Nothing => process ([], listVal, env, stored, listLoc) (list)
  Just (BindLoc l) => process (xs, (lookup' (Just (BindLoc l)) (stored))::listVal, env, stored, listLoc) (( (CtExp (AExpR (ID (ValID c1)) )) ::xs , listVal, env, stored, listLoc)::list)
  Just (BindInt k) => process (xs, (ValInt k)::listVal, env, stored, listLoc) (( (CtExp (AExpR (ID (ValID c1)) )) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtCmd (Loop b c)) ::xs , listVal, env, stored, listLoc) (list) = process (CtExp (BExpR b) ::(CtCmdOp CtrlLoop::xs), ValCmd (Loop b c)::listVal, env, stored, listLoc) (( (CtCmd (Loop b c)) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtCmd (Cond b c1 c2)) ::xs , listVal, env, stored, listLoc) (list) = process (CtExp (BExpR b) ::(CtCmdOp CtrlCond::xs), ValCmd (Cond b c1 c2)::listVal, env, stored, listLoc) (( (CtCmd (Cond b c1 c2)) ::xs , listVal, env, stored, listLoc)::list)
process ( (CtCmd (CSeq c1 c2)) ::xs , listVal, env, stored, listLoc) (list) = process (CtCmd c1::(CtCmd c2::xs), listVal, env, stored, listLoc) (( (CtCmd (CSeq c1 c2)) ::xs , listVal, env, stored, listLoc)::list)

process ( (CtCmdOp CtrlAssign :: xs ,  v1 :: (ValId v2 ::listVal), env, stored, listLoc)) (list) = process (xs, listVal, env, (inserir (lookup (ValID v2) env) (v1) stored), listLoc) (( (CtCmdOp CtrlAssign :: xs ,  v1 :: (ValId v2 ::listVal), env, stored, listLoc))::list)
process ( (CtCmdOp CtrlLoop :: xs , ValBool True :: (ValCmd (Loop b2 c) :: listVal), env, stored, listLoc)) (list) = process (CtCmd c ::(CtCmd (Loop b2 c)::xs), listVal, env, stored, listLoc) (( (CtCmdOp CtrlLoop :: xs , ValBool True :: (ValCmd (Loop b2 c) :: listVal), env, stored, listLoc))::list)
process ( (CtCmdOp CtrlLoop :: xs , ValBool False :: (ValCmd (Loop b2 c) :: listVal), env, stored, listLoc)) (list) = process (xs, listVal, env, stored, listLoc) (( (CtCmdOp CtrlLoop :: xs , ValBool False :: (ValCmd (Loop b2 c) :: listVal), env, stored, listLoc))::list)
process ( (CtCmdOp CtrlCond :: xs , ValBool True :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored, listLoc)) (list) = process (CtCmd c1 ::xs, listVal, env, stored, listLoc) (( (CtCmdOp CtrlCond :: xs , ValBool True :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored, listLoc))::list)
process ( (CtCmdOp CtrlCond :: xs , ValBool False :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored, listLoc)) (list) = process (CtCmd c2 ::xs, listVal, env, stored, listLoc) (( (CtCmdOp CtrlCond :: xs , ValBool False :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored, listLoc))::list)

--Declarations
process ( CtExp (Ref exp) :: xs , listVal, env, stored, listLoc) (list) = process (CtExp exp ::(CtExpOp CtrlRef::xs), listVal, env, stored, listLoc) (( CtExp (Ref exp) :: xs , listVal, env, stored, listLoc)::list)
process ( CtCmd (Blk d c) :: xs , listVal, env, stored, listLoc) (list) = process ( CtDec d :: (CtCmdOp CtrlBlkDec :: (CtCmd c :: (CtCmdOp CtrlBlkCmd::xs))), ValListLoc listLoc :: listVal, env, stored, []) (( CtCmd (Blk d c) :: xs , listVal, env, stored, listLoc)::list)
process ( CtDec (Bind (ValID x) exp) :: xs , listVal, env, stored, listLoc) (list) = process ( CtExp exp :: (CtDecOp CtrlBind::xs), ValId x ::listVal, env, stored, listLoc) (( CtDec (Bind (ValID x) exp) :: xs , listVal, env, stored, listLoc)::list)
process ( CtExp (DeRef (ValID x)) :: xs , listVal, env, stored, listLoc) (list) = process ( xs, (ValBindable (transforma (lookup (ValID x) env)))::listVal, env, stored, listLoc) (( CtExp (DeRef (ValID x)) :: xs , listVal, env, stored, listLoc)::list)
process ( CtExp (ValRef (ValID x)) :: xs , listVal, env, stored, listLoc) (list) = process ( xs, (lookup' (Just (BindLoc (getLocFromValLoc (lookup' (lookup (ValID x) env) stored))) ) stored)::listVal, env, stored, listLoc) (( CtExp (ValRef (ValID x)) :: xs , listVal, env, stored, listLoc)::list)
process ( CtDec (DSeq a b) :: xs , listVal, env, stored, listLoc) (list) = process (CtDec a :: (CtDec b :: xs), listVal, env, stored, listLoc) (( CtDec (DSeq a b) :: xs , listVal, env, stored, listLoc)::list)

process ( CtExpOp CtrlRef :: xs , v :: listVal, env, stored, listLoc) (list) = process (xs, ValLoc (getLocPlus1FromMap stored) ::listVal, env, extendStored stored v, getLocPlus1FromMap stored ::listLoc) (( CtExpOp CtrlRef :: xs , v :: listVal, env, stored, listLoc)::list)
process ( CtCmdOp CtrlBlkDec :: xs , ValEnv e :: listVal, env, stored, listLoc) (list) = process (xs, ValEnv env ::listVal, addIntersectionNewEnv (toList e) (toList env), stored, listLoc) (( CtCmdOp CtrlBlkDec :: xs , ValEnv e :: listVal, env, stored, listLoc)::list)
process ( CtCmdOp CtrlBlkCmd :: xs , ValEnv e :: (ValListLoc l :: listVal), env, stored, listLoc) (list) = process (xs, listVal, e, deleteLocsStore listLoc stored, l) (( CtCmdOp CtrlBlkCmd :: xs , ValEnv e :: (ValListLoc l :: listVal), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindLoc l) (e)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: listVal), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindLoc l) (empty)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: listVal), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindInt n) (e)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: listVal), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindInt n) (empty)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: listVal), env, stored, listLoc)::list)
