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

getLocFromValBindable: Bindable -> Val
getLocFromValBindable (BindLoc x) = ValLoc x

-- Atualiza segundo ambiente com os valores do primeiro, e cria novas chave-valor caso nao exista
-- ex: addIntersectionNewEnv ([ (ValId "k", L 3) , (ValId "y", L 4) , (ValId "z", L 5)]) ([ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)])
addIntersectionNewEnv: List (Id,Bindable)-> List (Id,Bindable) -> SortedMap Id Bindable
addIntersectionNewEnv [] env = fromList env
addIntersectionNewEnv ( (key,value) :: e) env = addIntersectionNewEnv e (toList (insert key value (fromList env)))


-- deleta todos as chave-valor da memoria dada uma lista de chaves (Loc)
deleteLocsStore: List Loc -> SortedMap Loc Val -> SortedMap Loc Val
deleteLocsStore [] store = store
deleteLocsStore (x :: xs) store = deleteLocsStore xs (delete x store)


pushExpsInCtrl : Actuals -> List Ctrl -> List Ctrl
pushExpsInCtrl (Act []) list = list
pushExpsInCtrl (Act (x::xs)) list =  pushExpsInCtrl (Act xs) ((CtExp x)::list)

getCmdFromClosure : Id -> (SortedMap Id Bindable) ->  Ctrl
getCmdFromClosure id env = let (bindClosure) = lookup id env in case bindClosure of
  Just (BindClos (Clos (f, b, e))) => CtCmd b

getFormalsFromClosure : Id -> (SortedMap Id Bindable) -> Formals
getFormalsFromClosure id env = let (bindClosure) = lookup id env in case bindClosure of
  Just (BindClos (Clos (f, b, e))) => f

getEnvFromClosure : Id -> (SortedMap Id Bindable) -> (SortedMap Id Bindable)
getEnvFromClosure id env = let (bindClosure) = lookup id env in case bindClosure of
  Just (BindClos (Clos (f, b, e))) => e

getExpsFromListVal: List Val -> Nat -> List Val -> List Val
getExpsFromListVal list Z l = reverse l
getExpsFromListVal (x::xs) (S tam) l = getExpsFromListVal xs (tam) (x::l)

getLocFromValLoc: Val -> Loc
getLocFromValLoc (ValLoc l) = l

match: Formals -> List Val -> SortedMap Id Bindable -> SortedMap Id Bindable
match (Form []) (val::vals) e = empty
match (Form (id::ids)) [] e = empty
match (Form []) [] e = e
match (Form (id::ids)) (val::vals) e = case val of
  ValInt x => let env = insert id (ValBindable (BindInt x)) e in match (Form ids) (vals) env
  ValLoc l => let env = insert id (ValBindable (BindLoc l)) e in match (Form ids) (vals) env

lookup': Maybe Bindable -> SortedMap Loc Val -> Val
lookup' (Just (BindLoc loc)) sto =  transforma (lookup loc sto)
lookup' Nothing sto = ValNop

inserir : Maybe Bindable -> Val -> SortedMap Loc Val -> SortedMap Loc Val
inserir (Just (BindLoc loc)) v stored = insert loc v stored
inserir Nothing v stored = stored

process: (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> List (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc) -> ((List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc), List (List Ctrl, List Val, SortedMap Id Bindable , SortedMap Loc Val, List Loc))

-- Stop Case
process ([],[], env, stored, listLoc) list = (([],[], env, stored, listLoc), ([],[], env, stored, listLoc)::list)

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
process ( (CtExp (AExpR (IdA (ValID c1)) )) ::xs , listVal, env, stored, listLoc) (list) = let loc = lookup (ValID c1) env in case loc of
  Nothing => process ([], listVal, env, stored, listLoc) (list)
  Just (BindLoc l) => process (xs, (lookup' (Just (BindLoc l)) (stored))::listVal, env, stored, listLoc) (( (CtExp (AExpR (IdA (ValID c1)) )) ::xs , listVal, env, stored, listLoc)::list)
  Just (BindInt k) => process (xs, (ValInt k)::listVal, env, stored, listLoc) (( (CtExp (AExpR (IdA (ValID c1)) )) ::xs , listVal, env, stored, listLoc)::list)

process ( (CtExp (BExpR (IdB (ValID c1)) )) ::xs , listVal, env, stored, listLoc) (list) = let loc = lookup (ValID c1) env in case loc of
  Nothing => process ([], listVal, env, stored, listLoc) (list)
  Just (BindLoc l) => process (xs, (lookup' (Just (BindLoc l)) (stored))::listVal, env, stored, listLoc) (( (CtExp (BExpR (IdB (ValID c1)) )) ::xs , listVal, env, stored, listLoc)::list)
  Just (BindInt k) => process (xs, (ValInt k)::listVal, env, stored, listLoc) (( (CtExp (BExpR (IdB (ValID c1)) )) ::xs , listVal, env, stored, listLoc)::list)

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
process ( CtDec (BindF (ValID x) abs) :: xs , listVal, env, stored, listLoc) (list) = process ( CtAbs abs :: (CtDecOp CtrlBindF::xs), ValId x ::listVal, env, stored, listLoc) (( CtDec (BindF (ValID x) abs) :: xs , listVal, env, stored, listLoc)::list)
process ( CtExp (DeRef (ValID x)) :: xs , listVal, env, stored, listLoc) (list) = process ( xs, ( getLocFromValBindable (transforma (lookup (ValID x) env)))::listVal, env, stored, listLoc) (( CtExp (DeRef (ValID x)) :: xs , listVal, env, stored, listLoc)::list)
process ( CtExp (ValRef (ValID x)) :: xs , listVal, env, stored, listLoc) (list) = process ( xs, (lookup' (Just (BindLoc (getLocFromValLoc (lookup' (lookup (ValID x) env) stored))) ) stored)::listVal, env, stored, listLoc) (( CtExp (ValRef (ValID x)) :: xs , listVal, env, stored, listLoc)::list)
process ( CtDec (DSeq a b) :: xs , listVal, env, stored, listLoc) (list) = process (CtDec a :: (CtDec b :: xs), listVal, env, stored, listLoc) (( CtDec (DSeq a b) :: xs , listVal, env, stored, listLoc)::list)

process ( CtExpOp CtrlRef :: xs , v :: listVal, env, stored, listLoc) (list) = process (xs, ValLoc (getLocPlus1FromMap stored) ::listVal, env, extendStored stored v, getLocPlus1FromMap stored ::listLoc) (( CtExpOp CtrlRef :: xs , v :: listVal, env, stored, listLoc)::list)
process ( CtCmdOp CtrlBlkDec :: xs , ValEnv e :: listVal, env, stored, listLoc) (list) = process (xs, ValEnv env ::listVal, addIntersectionNewEnv (toList e) (toList env), stored, listLoc) (( CtCmdOp CtrlBlkDec :: xs , ValEnv e :: listVal, env, stored, listLoc)::list)
process ( CtCmdOp CtrlBlkCmd :: xs , ValEnv e :: (ValListLoc l :: listVal), env, stored, listLoc) (list) = process (xs, listVal, e, deleteLocsStore listLoc stored, l) (( CtCmdOp CtrlBlkCmd :: xs , ValEnv e :: (ValListLoc l :: listVal), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindLoc l) (e)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: listVal), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindLoc l) (empty)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValLoc l :: (ValId w :: listVal), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindInt n) (e)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: (ValEnv e :: listVal)), env, stored, listLoc)::list)
process ( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: listVal), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindInt n) (empty)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBind :: xs , ValInt n :: (ValId w :: listVal), env, stored, listLoc)::list)

process ( CtDecOp CtrlBindF :: xs , ValClos cls :: (ValId w :: listVal), env, stored, listLoc) (list) = process (xs, ValEnv (insert (ValID w) (BindClos cls) (empty)) ::listVal, env, stored, listLoc) (( CtDecOp CtrlBindF :: xs , ValClos cls :: (ValId w :: listVal), env, stored, listLoc)::list)


--Abstractions
process ( CtAbs (Abstr f c) :: xs , listVal, env, stored, listLoc) (list) = process (xs, ValClos (Clos (f,c, env))::listVal, env, stored, listLoc) (( CtAbs (Abstr f c) :: xs , listVal, env, stored, listLoc)::list)
process ( CtCmd (Call id (Act listExp)) :: xs , listVal, env, stored, listLoc) (list) = process ((pushExpsInCtrl (Act listExp) ((CtCmdOp (CtrlCall id (Prelude.List.length listExp)))::xs)), listVal, env, stored, listLoc) (( CtCmd (Call id (Act listExp)) :: xs , listVal, env, stored, listLoc)::list)


process ( CtCmdOp (CtrlCall id tam)::xs , listVal, env, stored, listLoc) (list) = process ((getCmdFromClosure id env)::((CtCmdOp CtrlBlkCmd)::xs), ValEnv env :: listVal, addIntersectionNewEnv (match (getFormalsFromClosure id env) (getExpsFromListVal (listVal) (tam) ([]) ) ) (addIntersectionNewEnv ((getEnvFromClosure id env)) (env) ) , stored, listLoc) (( CtCmdOp (CtrlCall id tam)::xs , listVal, env, stored, listLoc)::list)
