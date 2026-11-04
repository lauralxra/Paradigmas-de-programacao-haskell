-- stack ghci --package silently --package containers --package HUnit interpretador-25-04-26.hs

module Interp where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit
import System.IO.Silently (capture_)

-- Map é usado para armazenar os identificadores do programa
-- Funcões utilizadas: Map.empty, Map.insert e Map.lookup

-- ============================================================
-- Tipos da arvore sintatica 
-- ============================================================

data Type
  = TInt
  | TBool
  | TVoid
  | TFloat
  | TArray Type -- representa um tipo array , como array de Int ou Bool, mas podemos colocar float , por exemplo TArray TFloat
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Gt  | Le | Ge
  | And | Or
  deriving (Show, Eq)

data Expr
  = Lit   Int
  | BLit  Bool
  | Var   String
  | BinE  BinOp Expr Expr
  | Not   Expr
  | Call  String [Expr]
  | FLit Float
  | Index String Expr --Acesso a uma posição: numeros[2]
  -- ainda sobre a adição dessa expressação do indíce do array , o primeiro argumento é o nome do array; o segundo é a expressão que calcula o índice.
  deriving (Show, Eq)

data Stmt
  = Decl   Type String Expr    -- int x = 10; 
  | Assign String Expr         -- x = 20 + 10
  | If     Expr Stmt Stmt
  | While  Expr Stmt
  | Block  [Stmt]
  | Return Expr                -- return x + y
  | Print  Expr                -- printf (10)
  | Skip
  | DeclArray Type String Int -- Declara um array: tipo, nomee tamanho (por isso ta Int , porque é um array unidimensional)
  -- por exemplo em de acordo com a IA - int numeros[3] → DeclArray TInt "numeros" 3 (usando a linguagem c como referencia)
  | AssignIndex String Expr Expr -- Altera uma posição para um outro valor : nome, indece e novo valor 
  -- outro exemplo para explicar como funciona essa expressão, numeros[1] = 10 → AssignIndex "numeros" (Lit 1) (Lit 10)
  deriving (Show, Eq)

data FunDecl = FunDecl
  { funName   :: String
  , funParams :: [(Type, String)]-- [(TInt, "x"), (TInt,"y")]
  , funBody   :: Stmt
  } deriving (Show, Eq)

-- ============================================================
-- Valores e estado do programa (ambiente)
-- ============================================================

-- programas lidam com inteiros e booleanos
data Value
  = VInt  Int
  | VBool Bool
  | VVoid
  | VFloat Float
  | VArray [Value] --representa os elementos do Arrays
  deriving (Show, Eq)

-- variaveis no escopo do programa
type Env    = Map String Value

-- funções no escopo do programa
type FunEnv = Map String FunDecl

--Adição de funções auxiliares (essas funções ajudam a inicializar , consultar e modificar os arrays) --
-- Retorna o valor inicial de cada tipo de elemento. pararecido com : int nome[5] =0;
-- Int começa em 0, Bool em False e Float em 0.0.
defaultArrayValue :: Type -> Value
defaultArrayValue TInt   = VInt 0
defaultArrayValue TBool  = VBool False
defaultArrayValue TFloat = VFloat 0.0
defaultArrayValue _      = error "Tipo invalido para elemento de array"

--Nessa função o objetivo é para buscar um elemnto em uma lista usando seu indice , ou seja, recebe o índice e a lista, devolvendo o valor daquela posição.
-- Além de verificar se o indice é negativo ou ultrapassa o tamanho
getArrayElement :: Int -> [Value] -> Value
getArrayElement index values
  | index < 0 = error "Indice negativo em array"
  | otherwise =
      case drop index values of
        value : _ -> value
        []        -> error "Indice fora dos limites do array"


--Essa função substitui um elemento sem alterar as demais posições.
--divide a lista antes e depois do índice e monta uma nova lista com o valor atualizado.(essa foi mais dificil de entender como ficaria)
setArrayElement :: Int -> Value -> [Value] -> [Value]
setArrayElement index newValue values
  | index < 0 = error "Indice negativo em array"
  | otherwise =
      case splitAt index values of    -- parecido com switch em c.
        (before, _ : after) -> before ++ (newValue : after)
        _                   -> error "Indice fora dos limites do array"

-- ============================================================
-- Resultado da execução de um Stmt
-- ============================================================

data StmtResult
  = Next     Env
  | Returned Value Env
  deriving (Show)

-- ============================================================
-- Avaliação de Expressões 
-- ============================================================

evalExpr :: FunEnv -> Env -> Expr -> IO Value
evalExpr fenv env expr = case expr of

  Lit  n -> return (VInt n)
  FLit n -> return (VFloat n)
  BLit b -> return (VBool b)

  Var x  -> case Map.lookup x env of
               Nothing -> error ("Variavel nao declarada: " ++ x)
               Just v  -> return v

  Not e  -> do
    v <- evalExpr fenv env e
    case v of
      VBool b -> return (VBool (not b))
      _       -> error "Not aplicado a nao-booleano"

  BinE op l r -> do
    vl <- evalExpr fenv env l
    vr <- evalExpr fenv env r
    return (evalBinOp op vl vr)

  Call fname args ->
    case Map.lookup fname fenv of
      Nothing  -> error ("Funcao nao declarada: " ++ fname)
      Just fun -> do
        let nparams = length (funParams fun)
            nargs   = length args
        if nparams /= nargs
          then error ("Funcao " ++ fname ++ " espera " ++
                      show nparams ++ " argumentos, recebeu " ++
                      show nargs)
          else do
            vals <- mapM (evalExpr fenv env) args
            let params   = map snd (funParams fun)
                envLocal = Map.fromList (zip params vals)
            result <- execStmt fenv envLocal (funBody fun)
            case result of
              Returned v _ -> return v
              Next _       -> return VVoid
  -- a entrada para essa consulta é Index "numeros" (Lit 1)
  Index name indexExpr -> do -- essa expressão é para acessa uma posição de um array: numeros[1]
    indexValue <- evalExpr fenv env indexExpr
    case indexValue of 
      VInt i -> 
        case Map.lookup name env of 
          Nothing ->
            error ("Array não declarado: " ++ name)
          Just (VArray values) ->
            return (getArrayElement i values)
          Just _ ->
            error "Variável não é um array"
      _ -> 
        error "Indice do array deve ser inteiro"



-- ============================================================
-- evalBinOp — permanece pura
-- ============================================================

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Add (VInt  a) (VInt  b) = VInt  (a + b)
evalBinOp Add (VFloat a) (VFloat b) = VFloat (a + b)
evalBinOp Add (VInt a) (VFloat b) =  VFloat (fromIntegral a + b)
evalBinOp Add (VFloat a) (VInt b) = VFloat (a + fromIntegral b)
evalBinOp Sub (VInt  a) (VInt  b) = VInt  (a - b)
evalBinOp Sub (VFloat a) (VFloat b) = VFloat (a - b)
evalBinOp Sub (VFloat a) (VInt b) = VFloat (a - fromIntegral b)
evalBinOp Sub (VInt a) (VFloat b) = VFloat (fromIntegral a - b)
evalBinOp Mul (VInt  a) (VInt  b) = VInt  (a * b)
evalBinOp Mul (VFloat a) (VFloat b) = VFloat (a * b)
evalBinOp Mul (VFloat a) (VInt b) = VFloat (a * fromIntegral b)
evalBinOp Mul (VInt a) (VFloat b) = VFloat (fromIntegral a * b)
evalBinOp Div (VInt  a) (VInt  b) = VInt  (a `div` b)
evalBinOp Div (VFloat a) (VFloat b) = VFloat (a / b)
evalBinOp Div (VFloat a) (VInt b) = VFloat (a / fromIntegral b)
evalBinOp Div (VInt a) (VFloat b) = VFloat (fromIntegral a / b)
evalBinOp Mod (VInt  a) (VInt  b) = VInt  (a `mod` b)
evalBinOp Eq  (VInt a) (VFloat b) = VBool (fromIntegral a == b)
evalBinOp Eq  (VFloat a) (VInt b) = VBool (a == fromIntegral b)
evalBinOp Eq  va        vb        = VBool (va == vb)
evalBinOp Neq (VInt a) (VFloat b) = VBool (fromIntegral a /= b)
evalBinOp Neq (VFloat a) (VInt b) = VBool (a /= fromIntegral b)
evalBinOp Neq va        vb        = VBool (va /= vb)
evalBinOp Lt  (VInt  a) (VInt  b) = VBool (a <  b)
evalBinOp Lt  (VInt a) (VFloat b) = VBool (fromIntegral a < b)
evalBinOp Lt  (VFloat a) (VInt b) = VBool (a < fromIntegral b)
evalBinOp Lt  (VFloat a) (VFloat b) = VBool (a < b)
evalBinOp Gt  (VInt a) (VInt b)   = VBool (a >  b)
evalBinOp Gt  (VInt a) (VFloat b)  = VBool (fromIntegral a > b)
evalBinOp Gt  (VFloat a) (VInt b) = VBool (a > fromIntegral b)
evalBinOp Gt  (VFloat a) (VFloat b) = VBool (a > b)
evalBinOp Le  (VInt  a) (VInt  b) = VBool (a <= b)
evalBinOp Le  (VInt a) (VFloat b) = VBool (fromIntegral a <= b)
evalBinOp Le  (VFloat a) (VInt b) = VBool (a <= fromIntegral b)
evalBinOp Le  (VFloat a) (VFloat b) = VBool (a <= b)
evalBinOp Ge  (VInt  a) (VInt  b) = VBool (a >= b)
evalBinOp Ge  (VInt a) (VFloat b) = VBool (fromIntegral a >= b)
evalBinOp Ge  (VFloat a) (VInt b) = VBool (a >= fromIntegral b)
evalBinOp Ge  (VFloat a) (VFloat b) = VBool (a >= b)
evalBinOp And (VBool a) (VBool b) = VBool (a && b)
evalBinOp Or  (VBool a) (VBool b) = VBool (a || b)
evalBinOp op  _         _         =
  error ("Operacao invalida ou tipos incompativeis: " ++ show op)

-- ============================================================
-- Execução de Statements
-- ============================================================

execStmt :: FunEnv -> Env -> Stmt -> IO StmtResult
execStmt fenv env stmt = case stmt of

  Skip ->
    return (Next env)

  Print e -> do
    v <- evalExpr fenv env e
    putStrLn (showValue v)
    return (Next env)

  Decl t x e -> do
    v <- evalExpr fenv env e
    let v'   = ajustaValor t v
        env' = Map.insert x v' env
    -- seq obriga a conversao a acontecer agora: o Map eh preguicoso e o erro
    -- de tipo nunca apareceria se a variavel nao fosse lida depois
    v' `seq` return (Next env')

  Assign x e ->
    case Map.lookup x env of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just valorAtual -> do
        v <- evalExpr fenv env e
        -- o Env so guarda valores: o tipo da variavel vem do valor que ela ja tem
        let v'   = ajustaValor (tipoDoValor valorAtual) v
            env' = Map.insert x v' env
        v' `seq` return (Next env')

  If cond t f -> do
    v <- evalExpr fenv env cond
    case v of
      VBool True  -> execStmt fenv env t
      VBool False -> execStmt fenv env f
      _           -> error "Condicao do If nao eh booleana"

  Return e -> do
    v <- evalExpr fenv env e
    return (Returned v env)

  Block stmts ->
    execBlock fenv env stmts

  While cond body -> do
    v <- evalExpr fenv env cond
    case v of
      VBool False -> return (Next env)
      VBool True  -> do
        r <- execStmt fenv env body
        case r of
          Returned v' env' -> return (Returned v' env')
          Next        env' -> execStmt fenv env' (While cond body)
      _ -> error "Condicao do While nao eh booleana"

    -- Declara um array e inicializa todas as posições.(Laura)
  DeclArray ty name size ->
    if size < 0
      then error "Tamanho negativo para array"
      else do

        let initialValues = replicate size (defaultArrayValue ty)
        let env' = Map.insert name (VArray initialValues) env

        return (Next env')


  -- Modifica uma posição específica do array. (Laura)
  AssignIndex name indexExpr valueExpr ->
    case Map.lookup name env of
      Nothing ->
        error ("Array nao declarado: " ++ name)
      Just (VArray values) -> do
        indexValue <- evalExpr fenv env indexExpr
        newValue <- evalExpr fenv env valueExpr
        case indexValue of
          VInt index -> do
            -- o tipo do elemento vem do valor que ja esta na posicao
            let atual = getArrayElement index values
                v'    = ajustaValor (tipoDoValor atual) newValue
            let updatedValues =
                  setArrayElement index v' values
            let env' =
                  Map.insert name (VArray updatedValues) env
            v' `seq` return (Next env')
          _ ->
            error "Indice do array deve ser inteiro"
      Just _ ->
        error "Variavel nao eh um array"

-- ============================================================
-- Execução de um bloco
-- ============================================================

execBlock :: FunEnv -> Env -> [Stmt] -> IO StmtResult
execBlock _    env []     = return (Next env)
execBlock fenv env (s:ss) = do
  r <- execStmt fenv env s
  case r of
    Returned v env' -> return (Returned v env')
    Next       env' -> execBlock fenv env' ss

-- ============================================================
-- Verificação de tipos
-- ============================================================

-- Descobre o tipo de um valor
tipoDoValor :: Value -> Type
tipoDoValor (VInt _)   = TInt
tipoDoValor (VBool _)  = TBool
tipoDoValor (VFloat _) = TFloat
tipoDoValor VVoid      = TVoid
-- o tipo do array vem dos elementos (todos sao do mesmo tipo)
tipoDoValor (VArray (v:_)) = TArray (tipoDoValor v)
tipoDoValor (VArray [])    = TArray TVoid

-- Ajusta o valor ao tipo da variavel, convertendo Int <-> Float como em C
ajustaValor :: Type -> Value -> Value
ajustaValor TInt   (VInt n)   = VInt n
ajustaValor TInt   (VFloat n) = VInt (truncate n)
ajustaValor TFloat (VInt n)   = VFloat (fromIntegral n)
ajustaValor TFloat (VFloat n) = VFloat n
ajustaValor TBool  (VBool b)  = VBool b
ajustaValor t      v          =
  error ("Tipo incompativel: variavel do tipo " ++ show t ++
         " recebeu valor do tipo " ++ show (tipoDoValor v))

-- ============================================================
-- Utilitários
-- ============================================================

showValue :: Value -> String
showValue (VInt  n) = show n
showValue (VFloat n) = show n
showValue (VBool b) = if b then "verdadeiro" else "falso"
showValue VVoid     = "()"
showValue (VArray values) = -- laura
  "[" ++ joinValues values ++ "]"
joinValues :: [Value] -> String
joinValues [] = ""
joinValues [value] = showValue value
joinValues (value : rest) =
  showValue value ++ ", " ++ joinValues rest

-- ============================================================
-- Auxiliares para testes
-- ============================================================

-- Executa um programa e extrai o valor retornado via Return
runProg :: FunEnv -> Stmt -> IO Value
runProg fenv prog = do
  result <- execStmt fenv Map.empty prog
  case result of
    Returned v _ -> return v
    Next _       -> return VVoid

-- Executa um programa e captura tudo que foi impresso via Print
runProgIO :: FunEnv -> Stmt -> IO String
runProgIO fenv prog =
  capture_ (execStmt fenv Map.empty prog)

-- ============================================================
-- Funções de exemplo
-- ============================================================

-- int dobro(int x) { retorna x * 2; }
funDobro :: FunDecl
funDobro = FunDecl
  { funName   = "dobro"
  , funParams = [(TInt, "x")]
  , funBody   = Return (BinE Mul (Var "x") (Lit 2))
  }

-- int fatorial(int n) {
--   se (n == 0) retorna 1
--   senao retorna n * fatorial(n - 1)
-- }
funFatorial :: FunDecl
funFatorial = FunDecl
  { funName   = "fatorial"
  , funParams = [(TInt, "n")]
  , funBody   =
      If (BinE Eq (Var "n") (Lit 0))
         (Return (Lit 1))
         (Return (BinE Mul
                    (Var "n")
                    (Call "fatorial" [BinE Sub (Var "n") (Lit 1)])))
  }

-- int soma(int a, int b) { retorna a + b; }
funSoma :: FunDecl
funSoma = FunDecl
  { funName   = "soma"
  , funParams = [(TInt, "a"), (TInt, "b")]
  , funBody   = Return (BinE Add (Var "a") (Var "b"))
  }

fenvExemplo :: FunEnv
fenvExemplo = Map.fromList
  [ ("dobro",    funDobro)
  , ("fatorial", funFatorial)
  , ("soma",     funSoma)
  ]

-- ============================================================
-- Programas de teste
-- ============================================================

-- int x = 10; int y = 3; retorna x + y;
prog1 :: Stmt
prog1 = Block
  [ Decl TInt "x" (Lit 10)
  , Decl TInt "y" (Lit 3)
  , Return (BinE Add (Var "x") (Var "y"))
  ]

-- int n = 7; se (n > 5) retorna verdadeiro senao retorna falso
prog2 :: Stmt
prog2 = Block
  [ Decl TInt "n" (Lit 7)
  , If (BinE Gt (Var "n") (Lit 5))
       (Return (BLit True))
       (Return (BLit False))
  ]

-- int i = 0; enquanto (i < 3) { i = i + 1; } retorna i;
prog3 :: Stmt
prog3 = Block
  [ Decl TInt "i" (Lit 0)
  , While (BinE Lt (Var "i") (Lit 3))
      (Assign "i" (BinE Add (Var "i") (Lit 1)))
  , Return (Var "i")
  ]

-- retorna dobro(5)
prog4 :: Stmt
prog4 = Return (Call "dobro" [Lit 5])

-- retorna fatorial(5)
prog5 :: Stmt
prog5 = Return (Call "fatorial" [Lit 5])

-- int x = 4; retorna soma(dobro(x), 3)
prog6 :: Stmt
prog6 = Block
  [ Decl TInt "x" (Lit 4)
  , Return (Call "soma" [Call "dobro" [Var "x"], Lit 3])
  ]

-- int i = 0; enquanto (i < 3) { imprime(i); i = i + 1; }
prog7 :: Stmt
prog7 = Block
  [ Decl TInt "i" (Lit 0)
  , While (BinE Lt (Var "i") (Lit 3))
      (Block
        [ Print (Var "i")
        , Assign "i" (BinE Add (Var "i") (Lit 1))
        ])
  ]

-- imprime(dobro(4))
prog8 :: Stmt
prog8 = Print (Call "dobro" [Lit 4])

--retorna (2.5)
prog9 :: Stmt
prog9 = Return (FLit 2.5)

-- soma float float
prog10 :: Stmt
prog10 = Return (BinE Add (FLit 1.5) (FLit 2.0))

-- soma int e float
prog11 :: Stmt
prog11 = Return (BinE Add (Lit 2) (FLit 1.5))

--soma float e int
prog12 :: Stmt
prog12 = Return (BinE Add (FLit 1.5) (Lit 2))

-- subtracao float float
prog13 :: Stmt
prog13 = Return (BinE Sub (FLit 3.5) (FLit 1.5))

-- subtracao float int
prog14 :: Stmt
prog14 = Return (BinE Sub (FLit 3.5) (Lit 2))

-- subtracao int float
prog15 :: Stmt
prog15 = Return (BinE Sub (Lit 5) (FLit 2.5))

-- multiplicacao float float
prog16 :: Stmt
prog16 = Return (BinE Mul (FLit 2.5) (FLit 2.0))

-- multiplicacao float int
prog17 :: Stmt
prog17 = Return (BinE Mul (FLit 2.5) (Lit 2))

-- multiplicacao int float
prog18 :: Stmt
prog18 = Return (BinE Mul (Lit 2) (FLit 2.5))

-- divisao float float
prog19 :: Stmt
prog19 = Return (BinE Div (FLit 5.0) (FLit 2.0))

-- divisao float int
prog20 :: Stmt
prog20 = Return (BinE Div (FLit 5.0) (Lit 2))

-- divisao int float
prog21 :: Stmt
prog21 = Return (BinE Div (Lit 5) (FLit 2.0))

-- comparacao Lt int float
prog22 :: Stmt
prog22 = Return (BinE Lt (Lit 2) (FLit 2.5))

-- comparacao Lt float int
prog23 :: Stmt
prog23 = Return (BinE Lt (FLit 2.5) (Lit 3))

-- comparacao Lt float float
prog24 :: Stmt
prog24 = Return (BinE Lt (FLit 2.0) (FLit 2.5))

-- comparacao Gt int float
prog25 :: Stmt
prog25 = Return (BinE Gt (Lit 3) (FLit 2.5))

-- comparacao Gt float int
prog26 :: Stmt
prog26 = Return (BinE Gt (FLit 2.5) (Lit 2))

-- comparacao Gt float float
prog27 :: Stmt
prog27 = Return (BinE Gt (FLit 2.5) (FLit 2.0))

-- comparacao Le int float
prog28 :: Stmt
prog28 = Return (BinE Le (Lit 2) (FLit 2.5))

-- comparacao Le float int
prog29 :: Stmt
prog29 = Return (BinE Le (FLit 2.5) (Lit 3))

-- comparacao Le float float
prog30 :: Stmt
prog30 = Return (BinE Le (FLit 2.0) (FLit 2.5))

-- comparacao Ge int float
prog31 :: Stmt
prog31 = Return (BinE Ge (Lit 3) (FLit 2.5))

-- comparacao Ge float int
prog32 :: Stmt
prog32 = Return (BinE Ge (FLit 2.5) (Lit 2))

-- comparacao Ge float float
prog33 :: Stmt
prog33 = Return (BinE Ge (FLit 2.5) (FLit 2.0))

-- impressao float literal
prog34 :: Stmt
prog34 = Print (FLit 2.5)

-- impressao divisao float
prog35 :: Stmt
prog35 = Print (BinE Div (Lit 5) (FLit 2.0))

-- igualdade no Le (2.5 <= 2.5) 
prog36 :: Stmt
prog36 = Return (BinE Le (FLit 2.5) (FLit 2.5))

-- igualdade no Ge (2.0 >= 2)
prog37 :: Stmt
prog37 = Return (BinE Ge (FLit 2.0) (Lit 2))

-- Lt com Resultado False (3 < 2.5)
prog38 :: Stmt
prog38 = Return (BinE Lt (Lit 3) (FLit 2.5))

-- Gt com Resultado False (2.0 > 2.5)
prog39 :: Stmt
prog39 = Return (BinE Gt (FLit 2.0) (FLit 2.5))

-- Le com Resultado False (3.5 <= 3)
prog40 :: Stmt
prog40 = Return (BinE Le (FLit 3.5) (Lit 3))

-- Ge com Resultado False (2 >= 2.5)
prog41 :: Stmt
prog41 = Return (BinE Ge (Lit 2) (FLit 2.5))

-- igualdade mista True (2 == 2.0)
prog42 :: Stmt
prog42 = Return (BinE Eq (Lit 2) (FLit 2.0))

-- desigualdade mista False
prog43 :: Stmt
prog43 = Return (BinE Neq (Lit 2) (FLit 2.0))

-- protecao da divisao inteira
prog44 :: Stmt
prog44 = Return (BinE Div (Lit 7) (Lit 2))

-- protecao da soma simples entre inteiros
prog45 :: Stmt
prog45 = Return (BinE Add (Lit 2) (Lit 3) )
-- int x = 5; retorna x;
prog46 :: Stmt
prog46 = Block
  [ Decl TInt "x" (Lit 5)
  , Return (Var "x")
  ]

-- bool b = verdadeiro; retorna b;
prog47 :: Stmt
prog47 = Block
  [ Decl TBool "b" (BLit True)
  , Return (Var "b")
  ]

-- float f = 1.5; retorna f;
prog48 :: Stmt
prog48 = Block
  [ Decl TFloat "f" (FLit 1.5)
  , Return (Var "f")
  ]

-- float f = 2; retorna f;
prog49 :: Stmt
prog49 = Block
  [ Decl TFloat "f" (Lit 2)
  , Return (Var "f")
  ]

-- int x = 3.9; retorna x;
prog50 :: Stmt
prog50 = Block
  [ Decl TInt "x" (FLit 3.9)
  , Return (Var "x")
  ]

-- int x = -3.9; retorna x;
prog51 :: Stmt
prog51 = Block
  [ Decl TInt "x" (FLit (-3.9))
  , Return (Var "x")
  ]

-- int x = 1; x = 7; retorna x;
prog52 :: Stmt
prog52 = Block
  [ Decl TInt "x" (Lit 1)
  , Assign "x" (Lit 7)
  , Return (Var "x")
  ]

-- float f = 1.0; f = 2; retorna f;
prog53 :: Stmt
prog53 = Block
  [ Decl TFloat "f" (FLit 1.0)
  , Assign "f" (Lit 2)
  , Return (Var "f")
  ]

-- int x = 0; x = 3.9; retorna x;
prog54 :: Stmt
prog54 = Block
  [ Decl TInt "x" (Lit 0)
  , Assign "x" (FLit 3.9)
  , Return (Var "x")
  ]

-- int x = 3.9; imprime(x);
prog55 :: Stmt
prog55 = Block
  [ Decl TInt "x" (FLit 3.9)
  , Print (Var "x")
  ]

-- int x = 2 + 1.5; retorna x;
prog56 :: Stmt
prog56 = Block
  [ Decl TInt "x" (BinE Add (Lit 2) (FLit 1.5))
  , Return (Var "x")
  ]

-- int x = 7 / 2.0; retorna x;
prog57 :: Stmt
prog57 = Block
  [ Decl TInt "x" (BinE Div (Lit 7) (FLit 2.0))
  , Return (Var "x")
  ]

-- float f = 5 / 2; retorna f;
prog58 :: Stmt
prog58 = Block
  [ Decl TFloat "f" (BinE Div (Lit 5) (Lit 2))
  , Return (Var "f")
  ]

-- float f = 0.0; f = 1 + 2; retorna f;
prog59 :: Stmt
prog59 = Block
  [ Decl TFloat "f" (FLit 0.0)
  , Assign "f" (BinE Add (Lit 1) (Lit 2))
  , Return (Var "f")
  ]

-- float notas[3]; notas[0] = 2; return notas[0];
prog60 :: Stmt
prog60 = Block
  [ DeclArray TFloat "notas" 3
  , AssignIndex "notas" (Lit 0) (Lit 2)
  , Return (Index "notas" (Lit 0))
  ]

-- int numeros[3]; numeros[1] = 3.9; return numeros[1];
prog61 :: Stmt
prog61 = Block
  [ DeclArray TInt "numeros" 3
  , AssignIndex "numeros" (Lit 1) (FLit 3.9)
  , Return (Index "numeros" (Lit 1))
  ]

-- int numeros[3]; numeros[2] = -3.9; return numeros[2];
prog62 :: Stmt
prog62 = Block
  [ DeclArray TInt "numeros" 3
  , AssignIndex "numeros" (Lit 2) (FLit (-3.9))
  , Return (Index "numeros" (Lit 2))
  ]

-- float notas[2]; notas[0] = 2.5; int x = notas[0]; return x;
prog63 :: Stmt
prog63 = Block
  [ DeclArray TFloat "notas" 2
  , AssignIndex "notas" (Lit 0) (FLit 2.5)
  , Decl TInt "x" (Index "notas" (Lit 0))
  , Return (Var "x")
  ]

-- float notas[3]; notas[0] = 1; notas[1] = 2; imprime(notas);
prog64 :: Stmt
prog64 = Block
  [ DeclArray TFloat "notas" 3
  , AssignIndex "notas" (Lit 0) (Lit 1)
  , AssignIndex "notas" (Lit 1) (Lit 2)
  , Print (Var "notas")
  ]

-- arrays 
progArrayInt :: Stmt
progArrayInt = Block
  [ DeclArray TInt "numeros" 3
  , Return (Index "numeros" (Lit 1))
  ]

progArrayBool :: Stmt
progArrayBool = Block
 [ DeclArray TBool "respostas" 2
  , Return (Index "respostas" (Lit 0))
  ]

progArrayFloat :: Stmt
progArrayFloat = Block
 [ DeclArray TFloat "notas" 3
  , Return (Index "notas" (Lit 2))
  ]

progArrayAssign :: Stmt
progArrayAssign = Block
  [ DeclArray TInt "numeros" 3
  , AssignIndex "numeros" (Lit 1) (Lit 99)
  , Return (Index "numeros" (Lit 1))
  ]

progArrayPrint :: Stmt
progArrayPrint = Block
 [ DeclArray TInt "numeros" 3
  , AssignIndex "numeros" (Lit 0) (Lit 10)
  , AssignIndex "numeros" (Lit 1) (Lit 20)
  , AssignIndex "numeros" (Lit 2) (Lit 30)
  , Print (Var "numeros")
  ]
-- ============================================================
-- Suite de testes HUnit
-- ============================================================

-- testes de valor retornado (usam runProg)

testSomaSimples :: Test
testSomaSimples = TestCase $ do
  v <- runProg Map.empty prog1
  assertEqual "soma 10 + 3" (VInt 13) v

testCondicionalVerdade :: Test
testCondicionalVerdade = TestCase $ do
  v <- runProg Map.empty prog2
  assertEqual "7 > 5 eh verdadeiro" (VBool True) v

testWhileContaAte3 :: Test
testWhileContaAte3 = TestCase $ do
  v <- runProg Map.empty prog3
  assertEqual "while incrementa i ate 3" (VInt 3) v

testDobro :: Test
testDobro = TestCase $ do
  v <- runProg fenvExemplo prog4
  assertEqual "dobro de 5 eh 10" (VInt 10) v

testFatorial :: Test
testFatorial = TestCase $ do
  v <- runProg fenvExemplo prog5
  assertEqual "fatorial de 5 eh 120" (VInt 120) v

testChamadaAninhada :: Test
testChamadaAninhada = TestCase $ do
  v <- runProg fenvExemplo prog6
  assertEqual "soma(dobro(4), 3) eh 11" (VInt 11) v

testFloatLiteral :: Test 
testFloatLiteral = TestCase $ do
  v <- runProg Map.empty prog9
  assertEqual "retorna 2.5" (VFloat 2.5) v

testSomaFloatFloat :: Test
testSomaFloatFloat = TestCase $ do
  v <- runProg Map.empty prog10
  assertEqual "retorna 3.5" (VFloat 3.5) v

testSomaIntFloat :: Test
testSomaIntFloat = TestCase $ do
  v <- runProg Map.empty prog11
  assertEqual "soma int e float" (VFloat 3.5) v

testSomaFloatInt :: Test
testSomaFloatInt = TestCase $ do 
  v <- runProg Map.empty prog12
  assertEqual "soma float e int" (VFloat 3.5) v  

testSubFloatFloat :: Test
testSubFloatFloat = TestCase $ do
  v <- runProg Map.empty prog13
  assertEqual "subtracao float e float" (VFloat 2.0) v  

testSubFloatInt :: Test  
testSubFloatInt = TestCase $ do
  v <- runProg Map.empty prog14
  assertEqual "subtracao float e int" (VFloat 1.5) v

testSubIntFloat :: Test
testSubIntFloat = TestCase $ do 
  v <- runProg Map.empty prog15
  assertEqual "subtracao int e float" (VFloat 2.5) v

testMulFloatFloat :: Test
testMulFloatFloat = TestCase $ do
  v <- runProg Map.empty prog16
  assertEqual "multiplicacao float e float" (VFloat 5.0) v  

testMulFloatInt :: Test
testMulFloatInt = TestCase $ do 
  v <- runProg Map.empty prog17
  assertEqual "multiplicacao float e int" (VFloat 5.0) v  

testMulIntFloat :: Test
testMulIntFloat = TestCase $ do 
  v <- runProg Map.empty prog18
  assertEqual "multiplicacao int e float" (VFloat 5.0) v 

testDivFloatFloat :: Test
testDivFloatFloat = TestCase $ do
  v <- runProg Map.empty prog19
  assertEqual "divisao float e float" (VFloat 2.5) v  

testDivFloatInt :: Test
testDivFloatInt = TestCase $ do
  v <- runProg Map.empty prog20
  assertEqual "divisao float e int" (VFloat 2.5) v

testDivIntFloat :: Test
testDivIntFloat = TestCase $ do
  v <- runProg Map.empty prog21  
  assertEqual "divisao int e float" (VFloat 2.5) v  

testLtIntFloat :: Test
testLtIntFloat = TestCase $ do 
  v <- runProg Map.empty prog22
  assertEqual "comparacao lt int e float" (VBool True) v 

testLtFloatInt :: Test
testLtFloatInt = TestCase $ do
  v <- runProg Map.empty prog23
  assertEqual "comparacao lt float e int" (VBool True) v   

testLtFloatFloat :: Test
testLtFloatFloat = TestCase $ do
  v <- runProg Map.empty prog24
  assertEqual "comparacao lt float e float" (VBool True) v

testGtIntFloat :: Test
testGtIntFloat = TestCase $ do
  v <- runProg Map.empty prog25
  assertEqual "comparacao gt int e float" (VBool True) v  

testGtFloatInt :: Test
testGtFloatInt = TestCase $ do 
  v <- runProg Map.empty prog26
  assertEqual "comparacao gt float e int" (VBool True) v    

testGtFloatFloat :: Test
testGtFloatFloat = TestCase $ do 
  v <- runProg Map.empty prog27
  assertEqual "comparacao gt float e float" (VBool True) v  

testLeIntFloat :: Test
testLeIntFloat = TestCase $ do
  v <- runProg Map.empty prog28
  assertEqual "comparacao le int e float" (VBool True) v 

testLeFloatInt :: Test
testLeFloatInt = TestCase $ do 
  v <- runProg Map.empty prog29
  assertEqual "comparacao le float e int" (VBool True) v  

testLeFloatFloat :: Test
testLeFloatFloat = TestCase $ do
  v <- runProg Map.empty prog30
  assertEqual "comparacao le float e float" (VBool True) v 

testGeIntFloat :: Test
testGeIntFloat = TestCase $ do
  v <- runProg Map.empty prog31
  assertEqual "comparacao ge int e float" (VBool True) v    

testGeFloatInt :: Test
testGeFloatInt = TestCase $ do
  v <- runProg Map.empty prog32
  assertEqual "comparacao ge float e int" (VBool True) v  

testGeFloatFloat :: Test
testGeFloatFloat = TestCase $ do 
  v <- runProg Map.empty prog33
  assertEqual "comparacao ge float e float" (VBool True) v  

testLeFloatFloatEq :: Test
testLeFloatFloatEq = TestCase $ do 
  v <- runProg Map.empty prog36
  assertEqual "reforco le float float igualdade" (VBool True) v 

testGeFloatIntEq :: Test
testGeFloatIntEq = TestCase $ do 
  v <- runProg Map.empty prog37
  assertEqual "reforco ge float int igualdade" (VBool True) v  

testLtIntFloatFalse :: Test
testLtIntFloatFalse = TestCase $ do 
  v <- runProg Map.empty prog38
  assertEqual "reforco lt int float falso" (VBool False) v  

testGtFloatFloatFalse :: Test
testGtFloatFloatFalse = TestCase $ do 
  v <- runProg Map.empty prog39
  assertEqual "reforco gt float e float falso" (VBool False) v  

testLeFloatIntFalse :: Test
testLeFloatIntFalse = TestCase $ do 
  v <- runProg Map.empty prog40
  assertEqual "reforco le float e int falso" (VBool False) v  

testGeIntFloatFalse :: Test
testGeIntFloatFalse = TestCase $ do 
  v <- runProg Map.empty prog41
  assertEqual "reforco ge int e float falso" (VBool False) v   

testEqIntFloatTrue :: Test
testEqIntFloatTrue = TestCase $ do
  v <- runProg Map.empty prog42
  assertEqual "igualdade mista int float" (VBool True) v  

testNeqIntFloatFalse :: Test
testNeqIntFloatFalse = TestCase $ do 
  v <- runProg Map.empty prog43
  assertEqual "desigualdade mista int float" (VBool False) v

testDivIntProtecao :: Test
testDivIntProtecao = TestCase $ do
  v <- runProg Map.empty prog44
  assertEqual "protecao divisao inteira" (VInt 3) v

testAddIntProtecao :: Test
testAddIntProtecao = TestCase $ do
  v <- runProg Map.empty prog45
  assertEqual "protecao soma inteira" (VInt 5) v
testDeclIntInt :: Test
testDeclIntInt = TestCase $ do
  v <- runProg Map.empty prog46
  assertEqual "int x = 5" (VInt 5) v

testDeclBoolBool :: Test
testDeclBoolBool = TestCase $ do
  v <- runProg Map.empty prog47
  assertEqual "bool b = verdadeiro" (VBool True) v

testDeclFloatFloat :: Test
testDeclFloatFloat = TestCase $ do
  v <- runProg Map.empty prog48
  assertEqual "float f = 1.5" (VFloat 1.5) v

testDeclFloatInt :: Test
testDeclFloatInt = TestCase $ do
  v <- runProg Map.empty prog49
  assertEqual "float f = 2 vira 2.0" (VFloat 2.0) v

testDeclIntFloat :: Test
testDeclIntFloat = TestCase $ do
  v <- runProg Map.empty prog50
  assertEqual "int x = 3.9 vira 3" (VInt 3) v

testDeclIntFloatNegativo :: Test
testDeclIntFloatNegativo = TestCase $ do
  v <- runProg Map.empty prog51
  assertEqual "int x = -3.9 vira -3" (VInt (-3)) v

testAssignIntInt :: Test
testAssignIntInt = TestCase $ do
  v <- runProg Map.empty prog52
  assertEqual "x = 7" (VInt 7) v

testAssignFloatInt :: Test
testAssignFloatInt = TestCase $ do
  v <- runProg Map.empty prog53
  assertEqual "f = 2 vira 2.0" (VFloat 2.0) v

testAssignIntFloat :: Test
testAssignIntFloat = TestCase $ do
  v <- runProg Map.empty prog54
  assertEqual "x = 3.9 vira 3" (VInt 3) v

testDeclIntSomaMista :: Test
testDeclIntSomaMista = TestCase $ do
  v <- runProg Map.empty prog56
  assertEqual "int x = 2 + 1.5 vira 3" (VInt 3) v

testDeclIntDivMista :: Test
testDeclIntDivMista = TestCase $ do
  v <- runProg Map.empty prog57
  assertEqual "int x = 7 / 2.0 vira 3" (VInt 3) v

testDeclFloatDivInteira :: Test
testDeclFloatDivInteira = TestCase $ do
  v <- runProg Map.empty prog58
  assertEqual "float f = 5 / 2 vira 2.0" (VFloat 2.0) v

testAssignFloatSomaInteira :: Test
testAssignFloatSomaInteira = TestCase $ do
  v <- runProg Map.empty prog59
  assertEqual "f = 1 + 2 vira 3.0" (VFloat 3.0) v

testArrayFloatRecebeInt :: Test
testArrayFloatRecebeInt = TestCase $ do
  v <- runProg Map.empty prog60
  assertEqual "notas[0] = 2 vira 2.0" (VFloat 2.0) v

testArrayIntRecebeFloat :: Test
testArrayIntRecebeFloat = TestCase $ do
  v <- runProg Map.empty prog61
  assertEqual "numeros[1] = 3.9 vira 3" (VInt 3) v

testArrayIntRecebeFloatNegativo :: Test
testArrayIntRecebeFloatNegativo = TestCase $ do
  v <- runProg Map.empty prog62
  assertEqual "numeros[2] = -3.9 vira -3" (VInt (-3)) v

testDeclIntComElementoFloat :: Test
testDeclIntComElementoFloat = TestCase $ do
  v <- runProg Map.empty prog63
  assertEqual "int x = notas[0] (2.5) vira 2" (VInt 2) v

testImprimeArrayFloatConvertido :: Test
testImprimeArrayFloatConvertido = TestCase $ do
  output <- runProgIO Map.empty prog64
  assertEqual "imprime array float apos conversao" "[1.0, 2.0, 0.0]\n" output

-- arrays
---- Testa a inicialização de um array de inteiros.
testArrayInt :: Test
testArrayInt = TestCase $ do
  v <- runProg Map.empty progArrayInt
  assertEqual "array Int inicia com zero" (VInt 0) v

-- Testa a inicialização de um array de booleanos.
testArrayBool :: Test
testArrayBool = TestCase $ do
  v <- runProg Map.empty progArrayBool
  assertEqual "array Bool inicia com False" (VBool False) v

-- Testa a inicialização de um array de floats.
testArrayFloat :: Test
testArrayFloat = TestCase $ do
  v <- runProg Map.empty progArrayFloat
  assertEqual "array Float inicia com 0.0" (VFloat 0.0) v

-- Testa a alteração de um elemento.
testArrayAssign :: Test
testArrayAssign = TestCase $ do
  v <- runProg Map.empty progArrayAssign
  assertEqual "altera a posicao 1" (VInt 99) v

-- Testa a impressão do array completo.
testArrayPrint :: Test
testArrayPrint = TestCase $ do
  output <- runProgIO Map.empty progArrayPrint
  assertEqual "imprime os elementos do array"
    "[10, 20, 30]\n" output

-- testes de saída impressa (usam runProgIO)

testWhileImprime :: Test
testWhileImprime = TestCase $ do
  output <- runProgIO Map.empty prog7
  assertEqual "while imprime 0, 1, 2" "0\n1\n2\n" output

testImprimeCallDobro :: Test
testImprimeCallDobro = TestCase $ do
  output <- runProgIO fenvExemplo prog8
  assertEqual "imprime dobro(4)" "8\n" output

testImprimeFloatLiteral :: Test
testImprimeFloatLiteral = TestCase $ do 
  output <- runProgIO Map.empty prog34
  assertEqual "impressao float literal" "2.5\n" output

testImprimeDivFloat :: Test
testImprimeDivFloat = TestCase $ do 
  output <- runProgIO Map.empty prog35
  assertEqual "impressao divisao float" "2.5\n" output
testImprimeIntConvertido :: Test
testImprimeIntConvertido = TestCase $ do
  output <- runProgIO Map.empty prog55
  assertEqual "imprime 3 apos conversao" "3\n" output

-- Casos de erro (aguardando autorização do professor para testar com Control.Exception):
-- int x = verdadeiro;
-- bool b = 5;
-- float f = verdadeiro;
-- bool b = 2.5;
-- int x = 1; x = verdadeiro;
-- bool r[2]; r[0] = 5;
-- int a[2]; a = 5;

-- suite completa

suite :: Test
suite = TestList
  [ TestLabel "soma simples"         testSomaSimples
  , TestLabel "condicional"          testCondicionalVerdade
  , TestLabel "while retorna"        testWhileContaAte3
  , TestLabel "chamada dobro"        testDobro
  , TestLabel "fatorial recursivo"   testFatorial
  , TestLabel "chamada aninhada"     testChamadaAninhada
  , TestLabel "while imprime"        testWhileImprime
  , TestLabel "imprime call dobro"   testImprimeCallDobro
  , TestLabel "float literal"        testFloatLiteral
  , TestLabel "soma float float"     testSomaFloatFloat
  , TestLabel "soma int float"       testSomaIntFloat
  , TestLabel "soma float int"       testSomaFloatInt
  , TestLabel "sub float float"      testSubFloatFloat
  , TestLabel "sub float int"        testSubFloatInt
  , TestLabel "sub int float"        testSubIntFloat
  , TestLabel "mul float float"      testMulFloatFloat
  , TestLabel "mul float int"        testMulFloatInt
  , TestLabel "mul int float"        testMulIntFloat
  , TestLabel "div float float"      testDivFloatFloat
  , TestLabel "div float int"        testDivFloatInt
  , TestLabel "div int float"        testDivIntFloat
  , TestLabel "compa lt int float"   testLtIntFloat
  , TestLabel "compa lt float int"   testLtFloatInt
  , TestLabel "compa lt float float" testLtFloatFloat
  , TestLabel "compa gt int float"   testGtIntFloat
  , TestLabel "compa gt float int"   testGtFloatInt
  , TestLabel "compa gt float float" testGtFloatFloat
  , TestLabel "compa le int float"   testLeIntFloat
  , TestLabel "compa le float int"   testLeFloatInt
  , TestLabel "compa le float float" testLeFloatFloat
  , TestLabel "compa ge int float"   testGeIntFloat
  , TestLabel "compa ge float int"   testGeFloatInt
  , TestLabel "compa ge float float" testGeFloatFloat
  , TestLabel "imprime float literal" testImprimeFloatLiteral
  , TestLabel "imprime div float"    testImprimeDivFloat
  , TestLabel "reforco le igualdade" testLeFloatFloatEq
  , TestLabel "reforco ge igualdade" testGeFloatIntEq
  , TestLabel "reforco lt falso"     testLtIntFloatFalse
  , TestLabel "reforco gt falso"     testGtFloatFloatFalse
  , TestLabel "reforco le falso"     testLeFloatIntFalse
  , TestLabel "reforco ge falso"     testGeIntFloatFalse
  , TestLabel "igual mista int float" testEqIntFloatTrue
  , TestLabel "desig mista int float" testNeqIntFloatFalse
  , TestLabel "protecao div int"     testDivIntProtecao
  , TestLabel "protecao soma int"    testAddIntProtecao
  , TestLabel "decl int int"         testDeclIntInt
  , TestLabel "decl bool bool"       testDeclBoolBool
  , TestLabel "decl float float"     testDeclFloatFloat
  , TestLabel "decl float int"       testDeclFloatInt
  , TestLabel "decl int float"       testDeclIntFloat
  , TestLabel "decl int float neg"   testDeclIntFloatNegativo
  , TestLabel "assign int int"       testAssignIntInt
  , TestLabel "assign float int"     testAssignFloatInt
  , TestLabel "assign int float"     testAssignIntFloat
  , TestLabel "imprime convertido"   testImprimeIntConvertido
  , TestLabel "decl int soma mista"  testDeclIntSomaMista
  , TestLabel "decl int div mista"   testDeclIntDivMista
  , TestLabel "decl float div int"   testDeclFloatDivInteira
  , TestLabel "assign float soma int" testAssignFloatSomaInteira
  , TestLabel "array float recebe int" testArrayFloatRecebeInt
  , TestLabel "array int recebe float" testArrayIntRecebeFloat
  , TestLabel "array int float neg"   testArrayIntRecebeFloatNegativo
  , TestLabel "decl int elemento float" testDeclIntComElementoFloat
  , TestLabel "imprime array convertido" testImprimeArrayFloatConvertido
  , TestLabel "array Int" testArrayInt
  , TestLabel "array Bool" testArrayBool
  , TestLabel "array Float" testArrayFloat
  , TestLabel "array atribuicao" testArrayAssign
  , TestLabel "array impressao" testArrayPrint
  ]

main :: IO ()
main = runTestTTAndExit suite
