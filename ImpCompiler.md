# ImpCompiler

Projeto Construindo um compilador para a linguagem Imp , utilizando o Pi Framework na linguagem Idris.
Feito por Caio Guimarães, Nicholas Barcelos e Ronald Campbell

## Introdução

Inicialmente definimos no arquivo tipos.idr as π IR expressions.
O tipo Exp representa uma Expressão que poderia ser AExp (Aritmética) ou BExp (Booleana). Logo, em seguida definimos o tipo AExp com os construtores para cada operação aritmética e BExp com os construtores das operações booleanas.
Ctrl e Val são tipos que foram definidos para auxiliar na representação dos tipos de controle e Opcodes das expressões e nos valores que podem ser retornados.

Em código:

```
data Id = ValID String

data AExp = Sum AExp AExp | Sub AExp AExp | Div AExp AExp | Mul AExp AExp | N Int | ID Id

data BExp = Eq AExp AExp | Not BExp | LT AExp AExp | GT AExp AExp | LE AExp AExp | GE AExp AExp | And BExp BExp | OR BExp BExp | Boo Bool

data Exp = AExpR AExp | BExpR BExp

data Cmd = Assign Id Exp | Loop BExp Cmd | Cond BExp Cmd Cmd | CSeq Cmd Cmd | NOP

data CmdOp = CtrlAssign | CtrlLoop | CtrlCond | CtrlCSeq

data ExpOp = CtrlSum | CtrlSub | CtrlDiv | CtrlMul | CtrlNot | CtrlLT | CtrlGT | CtrlLE | CtrlGE | CtrlAnd | CtrlOR | CtrlEq

data Ctrl = CtExp Exp | CtExpOp ExpOp | CtCmd Cmd | CtCmdOp CmdOp

data Val = ValInt Int | ValBool Bool | ValId String | ValCmd Cmd

data Loc = L Int

implementation Eq Loc where
  (L a) == (L b) = a == b

implementation Ord Loc where
  compare (L a) (L b) = compare a b

implementation Eq Val where
    (ValId a) == (ValId b) = a == b

implementation Ord Val where
    compare (ValId a) (ValId b) = compare a b

```
## π Automata

O pŕoximo passo foir criar o arquivo piAutomata.idr para definir o próprio π Automata e uma função de calc para calcular as expressões aritméticas e booleanas retornando um valor inteiro ou um valor booleano.

Em código:
```
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
```

A função process representa o π Automata, ela inicialmente recebe um estado do Automato que é uma lista de Ctrl, uma lista de Val e dois mapas de chave-valor representando a pilha de controle, a pilha de valores, o ambiente e a memória, respectivamente, devolvendo o novo estado do Automato.

Em código:
```
process: (List Ctrl, List Val, SortedMap Val Loc , SortedMap Loc Val) -> (List Ctrl, List Val, SortedMap Val Loc , SortedMap Loc Val)
process ([], [], env, stored) = ([], [], env, stored)
```
O π Automata para quando a pilha de controle e a pilha de valores forem vazias.

### Expressões Aritméticas
Nosso primeiro teste foi com a expressão de soma
A primeira transição:
```
δ(Sum(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #SUM :: C, V, S) corresponde à:
process ( (CtExp (AExpR (Sum n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSum :: xs)) ), listVal, env, stored)
```
Aqui o π Automata recebe a expressão aritmética de soma no topo da pilha e xs é o restante dos elementos na pilha, a pilha de valores está vazia. Na primeira transição ao encontrar o opcode Aexp sabe-se que é necessário empilhar os operandos n1 e n2 e o opcode correspondente a soma (Ctrl Sum) na pilha de controle.

Na próxima transição os operandos estarão no topo da pilha de controle e dessa forma irão casar com:
```
δ(Num(N) :: C, V, S) = δ(C, N :: V, S) corresponde à:
process ( (CtExp (AExpR (N     n  ))) ::xs , listVal, env, stored ) = process (xs , ValInt n :: listVal, env, stored)
```
Dessa forma, os operandos são empilhados na pilha de valores.
Então, a próxima transição será:

```
δ(#SUM :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ + N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlSum)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Sum (N val2) (N val1)))) ::restoLista, env, stored)
```

Ao encontrar o opcode CtrlSum no topo da pilha de controle a próxima transição realizada é a de chamar a função calc para a operação indicada com os dois valores mais acima da pilha de valores.

Seguindo o mesmo raciocinio estão definidas a seguir as outras operações aritméticas:
```
δ(Sub(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #SUB :: C, V, S) corresponde à:
process ( (CtExp (AExpR (Sub n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlSub :: xs)) ), listVal, env, stored)

δ(Mul(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #MUL :: C, V, S) corresponde à:
process ( (CtExp (AExpR (Mul n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlMul :: xs)) ), listVal, env, stored)

δ(Div(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #MUL :: C, V, S) corresponde  à:
process ( (CtExp (AExpR (Div n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlDiv :: xs)) ), listVal, env, stored)

δ(#SUB :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ - N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlSub)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Sub (N val2) (N val1)))) ::restoLista, env, stored)

δ(#MUL :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ * N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlMul)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Mul (N val2) (N val1)))) ::restoLista, env, stored)

δ(#DIV :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ / N₂ :: V, S) if N₂ ≠ 0 corresponde à:
process ( (CtExpOp CtrlDiv)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValInt (calcAExp (Div (N val2) (N val1)))) ::restoLista, env, stored)
```
### Expressões Booleanas

Para as expressões booleanas, utilizaremos o exemplo da expressão And.
Ao encontrar o opcode BExpr as parcias booleanas b1 e b2 são empilhadas na pilha de controle assim como o opcode CtrlAnd.

```
δ(And(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #AND :: C, V, S) corresponde à:
process ( (CtExp (BExpR (And b1 b2))) ::xs , listVal, env, stored ) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlAnd :: xs)) ), listVal, env, stored)
```

Ao encontrar o tipo Boo no topo da pilha de controle, dispara a próxima transição que empilha os valores das parciais booleanas na pilha de valores.
```
process ( (CtExp (BExpR (Boo b))) ::xs , listVal, env, stored ) = process (xs , ValBool b :: listVal, env, stored)
```
Ao encontrar o opcode CtrlAnd no topo da pilha de controle a próxima transição irá chamar a função calc para expressões booleanas utilizando os 2 valores do topo da pilha de valores.
```
δ(#AND :: C, Boo(B₁) :: Boo(B₂) :: V, S) = δ(C, B₁ ∧ B₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlAnd)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (And (Boo val2) (Boo val1)))) ::restoLista, env, stored)
```

As demais expressões booleanas seguem o mesmo raciocinio com ressalva que as expressões Eq, LT, LE, GT e GE recebem avaliações de expressões aritméticas.

```
δ(Eq(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #EQ :: C, V, S) corresponde à:
process ( (CtExp (BExpR (Eq n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlEq :: xs)) ), listVal, env, stored)

δ(Ge(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #GE :: C, V, S) corresponde à:
process ( (CtExp (BExpR (GE n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGE :: xs)) ), listVal, env, stored)

δ(Le(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #LE :: C, V, S) corresponde à:
process ( (CtExp (BExpR (LE n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLE :: xs)) ), listVal, env, stored)

δ(Lt(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #LT :: C, V, S)corresponde à:
process ( (CtExp (BExpR (LT n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlLT :: xs)) ), listVal, env, stored)

δ(Gt(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #GT :: C, V, S) corresponde à:
process ( (CtExp (BExpR (GT n1 n2))) ::xs , listVal, env, stored ) = process ((CtExp (AExpR n1) :: (CtExp (AExpR n2) :: (CtExpOp CtrlGT :: xs)) ), listVal, env, stored)

δ(Or(E₁, E₂) :: C, V, S) = δ(E₁ :: E₂ :: #OR :: C, V, S) corresponde à:
process ( (CtExp (BExpR (OR b1 b2))) ::xs , listVal, env, stored ) = process ((CtExp (BExpR b1) :: (CtExp (BExpR b2) :: (CtExpOp CtrlOR :: xs)) ), listVal, env, stored)

δ(#EQ :: C, Boo(B₁) :: Boo(B₂) :: V, S) = δ(C, B₁ = B₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlEq)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (Eq (N val2) (N val1)))) ::restoLista, env, stored)

δ(#GE :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ ≥ N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlGE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (GE (N val2) (N val1)))) ::restoLista, env, stored)

δ(#LE :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ ≤ N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlLE)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (LE (N val2) (N val1)))) ::restoLista, env, stored)

δ(#LT :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ < N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlLT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (LT (N val2) (N val1)))) ::restoLista, env, stored)

δ(#GT :: C, Num(N₁) :: Num(N₂) :: V, S) = δ(C, N₁ > N₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlGT)::xs , (ValInt val1) :: (ValInt val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (GT (N val2) (N val1)))) ::restoLista, env, stored)

δ(#OR :: C, Boo(B₁) :: Boo(B₂) :: V, S) = δ(C, B₁ ∨ B₂ :: V, S) corresponde à:
process ( (CtExpOp CtrlOR)::xs , (ValBool val1) :: (ValBool val2 :: restoLista), env, stored ) = process (xs, (ValBool (calcBExp (OR (Boo val2) (Boo val1)))) ::restoLista, env, stored)
```
A expressão Not é ligeiramente diferente é avaliado apenas um componente booleano.

```
δ(Not(E) :: C, V, S) = δ(E :: #NOT :: C, V, S) corresponde à:
process ( (CtExp (BExpR (Not b))) ::xs , listVal, env, stored ) = process ( (CtExp (BExpR b)) :: (CtExpOp CtrlNot :: xs) , listVal, env, stored)
```

As duas transições correspondentes são resolvidas dentro da função calcBExp
```
δ(#NOT :: C, Boo(True) :: V, S) = δ(C, False :: V, S)
δ(#NOT :: C, Boo(False) :: V, S) = δ(C, True :: V, S)
process ( (CtExpOp CtrlNot) :: xs , (ValBool b) :: restoLista, env, stored ) = process (xs , ValBool (calcBExp (Not (Boo b))) :: restoLista , env, stored)
```

### Comandos

Depois de introduzirmos os tipos Cmd, CmdOp e Id, nós definimos o tipo Loc para lidar com a memória. 

Ao encontrar um Id no topo da pilha de controle ele utiliza o Id para encontrar um Loc (location) dentro do ambiente, e com esse Loc ele vai na memória
pegar o valor associado ao Loc, empilhando o mesmo na pilha de valores. Sendo 'c1' a string do Id, o lookup uma função do SortedMap (mapa chave valor) que 
utiliza a chave como primeiro parametro e o mapa como segundo parametro para retornar o valor correspondente. O lookup' aqui tem a mesma função do lookup,
exceto que este trata a questão do retorno do lookup ser um Monad.

```
δ(Id(W) :: C, V, E, S) = δ(C, B :: V, E, S), where E[W] = l ∧ S[l] = B corresponde à:
process ( (CtExp (AExpR (ID (ValID c1)) )) ::xs , listVal, env, stored) = process (xs, (lookup' (lookup (ValId c1) (env)) (stored) )::listVal, env, stored)

```
Ao se deparar com um Assign no topo da pilha de controle, empilha-se o Id correspondente ao assign na pilha de valores e o opcode correspondente ao assign
seguido de um Exp. Sendo 'c2' nosso Exp e CtrlAssign nosso opcode.

```
δ(Assign(W, X) :: C, V, E, S) = δ(X :: #ASSIGN :: C, W :: V, E, S') corresponde à:
process ( (CtCmd (Assign (ValID c1) c2)) ::xs , listVal, env, stored) = process (CtExp c2 ::(CtCmdOp CtrlAssign::xs), ValId c1::listVal, env, stored)
```
Ao encontrar Loop no topo da pilha empilha-se o opcode correspondente ao loop e a expressão booleana ambos na pilha de controle, e empilha-se o Loop na
pilha de valores. Sendo 'b' a expressão booleana, CtrlLoop o opcode.

```
δ(Loop(X, M) :: C, V, E, S) = δ(X :: #LOOP :: C, Loop(X, M) :: V, E, S) corresponde à:
process ( (CtCmd (Loop b c)) ::xs , listVal, env, stored) = process (CtExp (BExpR b) ::(CtCmdOp CtrlLoop::xs), ValCmd (Loop b c)::listVal, env, stored)
```
Ao encontrar Cond (condicional) no topo da pilha de controle, empilha-se o Cond na pilha de valores, e empilha-se o opcode correspondente ao cond e a expressão
booleana na pilha de controle. Sendo 'b' nossa expressão booleana, CtrlCond o opcode.

```
δ(Cond(X, M₁, M₂) :: C, V, E, S) = δ(X :: #COND :: C, Cond(X, M₁, M₂) :: V, E, S) corresponde à:
process ( (CtCmd (Cond b c1 c2)) ::xs , listVal, env, stored) = process (CtExp (BExpR b) ::(CtCmdOp CtrlCond::xs), ValCmd (Cond b c1 c2)::listVal, env, stored)
```
Ao se deparar com CSeq no topo da pilha de controle, empilha-se os dois comandos na lista de controle. Sendo c1 o primeiro comando e c2 o segundo comando.
Contudo c1 é empilhado somente após c2 ser empilhado, para que c1 seja o primeiro comando a ser encontrado no topo.

```
δ(CSeq(M₁, M₂) :: C, V, E, S) = δ(M₁ :: M₂ :: C, V, E, S) corresponde à:
process ( (CtCmd (CSeq c1 c2)) ::xs , listVal, env, stored) = process (CtCmd c1::(CtCmd c2::xs), listVal, env, stored)
```
Ao encontrar o opcode de Assign (CtrlAssign) no topo da pilha de controle e dois valores na pilha de valores, inserimos na memória 'v1', utilizando o
Loc (location) encontrado fazendo-se o lookup de 'v2' no mapa do ambiente, ou seja, utilizamos o Loc encontrado pelo lookup no mapa do ambiente utilizando a chave
'v2'. Sendo 'v2'(Id) a chave para o mapa ambiente, e 'v1' o valor desse 'v2' na memória, mapeado por um location como chave.

```
δ(#ASSIGN :: C, T :: W :: V, E, S) = δ(C, V, E, S'), where E[W] = l ∧ S' = S/[l ↦ T] corresponde à:
process ( (CtCmdOp CtrlAssign :: xs ,  v1 :: (v2 ::listVal), env, stored)) = process (xs, listVal, env, inserir (lookup v2 env) (v1) stored)
```
Ao encontrar o opcode do Loop (CtrlLoop) no topo da pilha de controle, um valor booleano e outro Loop ambos na pilha de valores, então temos dois casos:

- Caso o valor booleano seja true, empilha-se o comando dentro do Loop na pilha de controle e empilha-se novamente o loop na pilha de valores.

- Caso o valor booleano seja false, retornamos a pilha de controle sem o opcode e a pilha de valores sem os valores mencionados acima. 

Sendo 'c' o comando dentro do Loop.


```
δ(#LOOP :: C, Boo(true) :: Loop(X, M) :: V, E, S) = δ(M :: Loop(X, M) :: C, V, E, S) corresponde à:
process ( (CtCmdOp CtrlLoop :: xs , ValBool True :: (ValCmd (Loop b2 c) :: listVal), env, stored)) = process (CtCmd c ::(CtCmd (Loop b2 c)::xs), listVal, env, stored)

δ(#LOOP :: C, Boo(false) :: Loop(X, M) :: V, E, S) = δ(C, V, E, S) corresponde à:
process ( (CtCmdOp CtrlLoop :: xs , ValBool False :: (ValCmd (Loop b2 c) :: listVal), env, stored)) = process (xs, listVal, env, stored)
```
Ao encotrar o opcode do Cond (CtrlCond) no topo da pilha de controle, um valor booleano e outro Cond Na pilha de valores, então temos dois casos:

- Caso o valor booleano seja true, empilha-se o primeiro comando (corresponde ao then do if then else) na pilha de controle.

- Caso o valor booleano seja false, empilha-se o segundo comando (corresponde ao else do if then else) na pilha de controle.

Sendo 'c1' o primeiro comando e 'c2' o segundo comando.

```
δ(#COND :: C, Boo(true) :: Cond(X, M₁, M₂) :: V, E, S) = δ(M₁ :: C, V, E, S) corresponde à:
process ( (CtCmdOp CtrlCond :: xs , ValBool True :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored)) = process (CtCmd c1 ::xs, listVal, env, stored)

δ(#COND :: C, Boo(false) :: Cond(X, M₁, M₂) :: V, E, S) = δ(M₂ :: C, V, E, S) corresponde à:
process ( (CtCmdOp CtrlCond :: xs , ValBool False :: (ValCmd (Cond b2 c1 c2) :: listVal), env, stored)) = process (CtCmd c2 ::xs, listVal, env, stored)
```
