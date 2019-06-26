module parser

import lexer
import tipos

%access public export

mutual

--------------------------------- Parser de Expressões Aritmeticas ---------------------------------
  num : List Token -> (Maybe AExp, List Token)
  num ((TokenInt n)::l) = (Just (N n), l)
  num ((TokenVarId id)::xs) = let (exp, r) = parseId ((TokenVarId id)::xs) in case exp of
    Nothing => (Nothing, r)
    Just k => (Just (IdA k), xs)
  num l = (Nothing, l)

  factor : List Token -> (Maybe AExp, List Token)
  factor = orParser num parenExp

  parenExp : List Token -> (Maybe AExp, List Token)
  parenExp ((TokenLParen)::xs) = let (e,r) = arithExp xs in parenExpAux e r where
    parenExpAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    parenExpAux Nothing r = (Nothing, (TokenLParen::r))
    parenExpAux (Just e) (TokenRParen::xs) = ((Just e),xs)
    parenExpAux (Just e) l = (Nothing,(TokenLParen::xs))
  parenExp l = (Nothing, l)

  mul : List Token -> (Maybe AExp, List Token)
  mul l = let (exp, r) = factor l in mulAux exp r where
    mulAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    mulAux Nothing r = (Nothing, r)
    mulAux (Just e) ((TokenTimes)::xs) = let (exp', r2) = factor xs in case exp' of
      Nothing => (Just e, (TokenTimes::xs))
      Just k => mulAux (Just (Mul e k)) r2
    mulAux (Just e) ((TokenDiv)::xs) = let (exp', r2) = factor xs in case exp' of
      Nothing => (Just e, (TokenDiv::xs))
      Just k => mulAux (Just (Div e k)) r2
    mulAux (Just e) l = ((Just e),l)

  sum : List Token -> (Maybe AExp, List Token)
  sum l = let (exp, r) = mul l in sumAux exp r where
    sumAux : Maybe AExp -> List Token -> (Maybe AExp, List Token)
    sumAux Nothing r = (Nothing, r)
    sumAux (Just e) ((TokenPlus)::xs) = let (exp', r2) = mul xs in case exp' of
      Nothing => (Just e, (TokenPlus::xs))
      Just k => sumAux (Just (Sum e k)) r2
    sumAux (Just e) ((TokenMinus)::xs) = let (exp', r2) = mul xs in case exp' of
      Nothing => (Just e, (TokenMinus::xs))
      Just k => sumAux (Just (Sub e k)) r2
    sumAux (Just e) l = ((Just e),l)

  arithExp : List Token -> (Maybe AExp, List Token)
  arithExp = sum

--------------------------------- Parser de Expressões Booleanas ---------------------------------
  bool : List Token -> (Maybe BExp, List Token)
  bool (TokenTrue::xs) = (Just (Boo True), xs)
  bool (TokenFalse::xs) = (Just (Boo False), xs)
  bool l = (Nothing, l)

  idBExp : List Token -> (Maybe BExp, List Token)
  idBExp ((TokenVarId id)::xs) = let (exp, r) = parseId ((TokenVarId id)::xs) in idAux exp r where
    idAux : Maybe Id -> List Token -> (Maybe BExp, List Token)
    idAux (Just e) (TokenPlus::xs) = (Nothing, (TokenPlus::xs))
    idAux (Just e) (TokenMinus::xs) = (Nothing, (TokenMinus::xs))
    idAux (Just e) (TokenTimes::xs) = (Nothing, (TokenTimes::xs))
    idAux (Just e) (TokenDiv::xs) = (Nothing, (TokenDiv::xs))
    idAux (Just e) l = (Just (IdB e), l)

  factorBExp : List Token -> (Maybe BExp, List Token)
  factorBExp ((TokenVarId id)::xs) = idBExp ((TokenVarId id)::xs)
  factorBExp (TokenNot::xs) = let (exp, r) = conditExp xs in case exp of
    Nothing => (Nothing, (TokenNot::xs))
    Just k => (Just (Not k), r)
  factorBExp l = orParser bool parenBExp l

  parenBExp : List Token -> (Maybe BExp, List Token)
  parenBExp ((TokenLParen)::xs) = let (e,r) = boolExp xs in parenBExpAux e r where
    parenBExpAux : Maybe BExp -> List Token -> (Maybe BExp, List Token)
    parenBExpAux Nothing r = (Nothing, (TokenLParen::r))
    parenBExpAux (Just e) (TokenRParen::xs) = ((Just e),xs)
    parenBExpAux (Just e) l = (Nothing,(TokenLParen::xs))
  parenBExp l = (Nothing, l)

  conditExp : List Token -> (Maybe BExp, List Token)
  conditExp = orParser aux factorBExp

  aux : List Token -> (Maybe BExp, List Token)
  aux l = let (e,r) = arithExp l in conditExpAux e r where
    conditExpAux : Maybe AExp -> List Token -> (Maybe BExp, List Token)
    conditExpAux Nothing r = (Nothing, r)
    conditExpAux (Just e) ((TokenEqual)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenEqual::xs))
      Just k => ((Just (Equal e k)), r2)
    conditExpAux (Just e) ((TokenMaior)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMaior::xs))
      Just k => ((Just (GT e k)), r2)
    conditExpAux (Just e) ((TokenMenor)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMenor::xs))
      Just k => ((Just (LT e k)), r2)
    conditExpAux (Just e) ((TokenMaiorIgual)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMaiorIgual::xs))
      Just k => ((Just (GE e k)), r2)
    conditExpAux (Just e) ((TokenMenorIgual)::xs) = let (exp', r2) = arithExp xs in case exp' of
      Nothing => (Nothing, (TokenMenorIgual::xs))
      Just k => ((Just (LE e k)), r2)
    conditExpAux (Just e) l' = (Nothing,l)

  logic : List Token -> (Maybe BExp, List Token)
  logic l = let (exp, r) = conditExp l in logicAux exp r where
    logicAux : Maybe BExp -> List Token -> (Maybe BExp, List Token)
    logicAux Nothing r = (Nothing, r)
    logicAux (Just e) ((TokenOr)::xs) = let (exp', r2) = conditExp xs in case exp' of
      Nothing => (Just e, (TokenOr::xs))
      Just k => logicAux (Just (OR e k)) r2
    logicAux (Just e) ((TokenAnd)::xs) = let (exp', r2) = conditExp xs in case exp' of
      Nothing => (Just e, (TokenAnd::xs))
      Just k => logicAux (Just (And e k)) r2
    logicAux (Just e) l = ((Just e),l)

  boolExp : List Token -> (Maybe BExp, List Token)
  boolExp = logic

--------------------------------- Ajustes para Exp ---------------------------------
  parseBExp : List Token -> (Maybe Exp, List Token)
  parseBExp l = let (exp, r) = logic l in case exp of
    Nothing => (Nothing, l)
    Just k => ((Just (BExpR k)), r)

  parseAExp : List Token -> (Maybe Exp, List Token)
  parseAExp l = let (exp, r) = arithExp l in case exp of
    Nothing => (Nothing, l)
    Just k => ((Just (AExpR k)), r)

  parseExp : List Token -> (Maybe Exp, List Token)
  parseExp = orParser parseBExp parseAExp

--------------------------------- Parser de Comandos ---------------------------------
  parseId : List Token -> (Maybe Id, List Token)
  parseId ((TokenVarId id)::xs) = (Just (ValID id), xs)
  parseId l = (Nothing, l)

  assign : List Token -> (Maybe Cmd, List Token)
  assign l = let (exp, r) = parseId l in assignAux exp r where
    assignAux : Maybe Id -> List Token -> (Maybe Cmd, List Token)
    assignAux Nothing r = (Nothing, r)
    assignAux (Just e) (TokenAssign::xs) = let (exp', r2) = parseExp xs in case exp' of
      Nothing => (Nothing, l)
      Just k => ((Just (Assign e k)), r2)
    assignAux (Just e) l = (Nothing, l)

  loop : List Token -> (Maybe Cmd, List Token)
  loop (TokenWhile::xs) = let (exp, r) = boolExp xs in loopAux exp r where
    loopAux : Maybe BExp -> List Token -> (Maybe Cmd, List Token)
    loopAux Nothing l = (Nothing, (TokenWhile::xs))
    loopAux (Just e) (TokenDo::xs) = let (exp', r2) = seq xs in case exp' of
      Nothing => (Nothing, (TokenDo::xs))
      Just k => (Just (Loop e k), r2)
    loopAux (Just e) l = (Nothing, l)
  loop l = (Nothing, l)

  partials : List Token -> (Maybe Cmd, List Token)
  partials (TokenElse::xs) = seq xs
  partials l = (Nothing, l)

  auxComand : List Token -> (Maybe Cmd, List Token)
  auxComand = orParser loop bloc

  comandUnityAux : List Token -> (Maybe Cmd, List Token)
  comandUnityAux = orParser assign auxComand

  comandUnity : List Token -> (Maybe Cmd, List Token)
  comandUnity = orParser cond comandUnityAux

  cond : List Token -> (Maybe Cmd, List Token)
  cond (TokenIf::xs) = let (exp, r) = boolExp xs in condAux exp r where
    condAux : Maybe BExp -> List Token -> (Maybe Cmd, List Token)
    condAux Nothing l = (Nothing, (TokenIf::xs))
    condAux (Just e) (TokenThen::xs) = let (c1, r2) = seq xs in case c1 of
      Nothing => (Nothing, (TokenThen::xs))
      Just k => let (c2, r3) = partials r2 in case c2 of
        Nothing => (Just (Cond e k NOP), r3)
        Just p => (Just (Cond e k p), r3)
    condAux (Just e) l = (Nothing, l)
  cond l = (Nothing, l)

  seq : List Token -> (Maybe Cmd, List Token)
  seq l = let (exp, r) = comandUnity l in seqAux exp r where
    seqAux : Maybe Cmd -> List Token -> (Maybe Cmd, List Token)
    seqAux Nothing l' = (Nothing, l')
    seqAux (Just e) l' = let (c2, r2) = seq l' in case c2 of
      Nothing => ((Just e), l')
      Just k => (Just (CSeq e k), r2)

  comands : List Token -> (Maybe Cmd, List Token)
  comands = seq

  declaration :List Token -> (Maybe Dec, List Token)
  declaration (TokenCons::xs) = const (TokenCons::xs)
  declaration (TokenVar::xs) = let (exp, r) = parseId xs in declarationAux exp r where
    declarationAux : Maybe Id -> List Token -> (Maybe Dec, List Token)
    declarationAux Nothing l' = (Nothing, l')
    declarationAux (Just e) (TokenAssign::l') = let (exp', r2) = parseExp l' in case exp' of
      Nothing => (Nothing, l')
      Just k => (Just (Bind e (Ref k)), r2)
    declarationAux (Just e) l' = (Nothing, l')
  declaration l = (Nothing, l)

  const :List Token -> (Maybe Dec, List Token)
  const (TokenCons::xs) = let (exp, r) = parseId xs in constAux exp r where
    constAux : Maybe Id -> List Token -> (Maybe Dec, List Token)
    constAux Nothing l' = (Nothing, l')
    constAux (Just e) (TokenAssign::l') = let (exp', r2) = parseExp l' in case exp' of
      Nothing => (Nothing, l')
      Just k => (Just (Bind e k), r2)
    constAux (Just e) l' = (Nothing, l')
  const l = (Nothing, l)

  bloc : List Token -> (Maybe Cmd, List Token)
  bloc (TokenLet::xs) = let (exp, r) = seqDec xs in case exp of
    Nothing => (Nothing, xs)
    Just e => let (c1, r2) = comands r in case c1 of
      Nothing => (Nothing, r)
      Just k => (Just (Blk e k), r2)
  bloc l = (Nothing, l)

  seqDec : List Token -> (Maybe Dec, List Token)
  seqDec l = let (exp, r) = declaration l in seqDecAux exp r where
    seqDecAux : Maybe Dec -> List Token -> (Maybe Dec, List Token)
    seqDecAux Nothing l' = (Nothing, l')
    seqDecAux (Just e) l' = let (c2, r2) = seqDec l' in case c2 of
      Nothing => ((Just e), l')
      Just k => (Just (DSeq e k), r2)

  ctrlParser : List Token -> (Maybe Ctrl, List Token)
  ctrlParser l = let (exp, r) = bloc l in case exp of
    Nothing => (Nothing, l)
    Just k => ((Just (CtCmd k)), r)

-- funçao para tentar parser de AExp e BExp
-- tenta aplicar o parser1 a lista
  orParser : (List Token -> (Maybe a, List Token)) -> (List Token -> (Maybe a, List Token)) -> (List Token -> (Maybe a, List Token))
  orParser parser1 parser2 list = let (exp,r) = parser1 list in orParserAux exp r where
-- orParserAux verifica se o parser1 falhou (Nothing) ou retornou com sucesso um (exp, resto da lista)
    orParserAux : Maybe a -> List Token -> (Maybe a, List Token)
    orParserAux Nothing l = parser2 l
    orParserAux (Just exp') l = (Just exp', l)

-- Ajustes para o piAutomata
transformPi : Exp -> Ctrl
transformPi exp = (CtExp exp)
