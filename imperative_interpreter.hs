--ADRIAN ESTEVEZ GALLEGO

--definiciones mas basicas de tipos
type Variable = (String, Int) --las variables seran simplemente el identificador y su valor
type State = [Variable] --el estado del programa es su lista de variables, con sus respectivos valores
type Program = [Instruction] --un programas es simplemente una lista de instrucciones

--tenemos dos tipos de isntrucciones, la asignacion que iguala al identificador dado el valor de la expresion
--y la expresion condicional, que ejecutara sus instrucciones mientras su condicion sea cierta
data Instruction = Asign String Expression | While CondExpression Program
instance Show Instruction where
    show (Asign s e) = show s ++ " = " ++ show e

infix 1 =:
(=:) :: String -> Expression -> Instruction
s =: e = Asign s e



data Expression = I Int | V String | Add Expression Expression | Sub Expression Expression | Mul Expression Expression
instance Show Expression where
    show (I x) = show x
    show (V s) = show s
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2

infixl 6 +:
(+:) :: Expression -> Expression -> Expression 
e1 +: e2 = Add e1 e2

infixl 6 -:
(-:) :: Expression -> Expression -> Expression
e1 -: e2 = Sub e1 e2

infixl 7 *:
(*:) :: Expression -> Expression -> Expression
e1 *: e2 = Mul e1 e2



data CondExpression = B Bool | Eq Expression Expression | UnEq Expression Expression | Less Expression Expression | Bigger Expression Expression | LesEq Expression Expression | BigEq Expression Expression | And CondExpression CondExpression | Or CondExpression CondExpression | Not CondExpression deriving Show

infix 4 ==:
e1 ==: e2 = Eq e1 e2

infix 4 !=:
e1 !=: e2 = UnEq e1 e2

infix 4 <:
e1 <: e2 = Less e1 e2

infix 4 <=:
e1 <=: e2 = LesEq e1 e2

infix 4 >:
e1 >: e2 = Bigger e1 e2

infix 4 >=:
e1 >=: e2 = BigEq e1 e2

infixl 2 &&:
e1 &&: e2 = And e1 e2

infixl 3 ||:
e1 ||: e2 = Or e1 e2

infixl 9 !:
(!:) :: CondExpression -> CondExpression
(!:) = Not



value :: Variable -> Int
value (_, v) = v

evalue :: Expression -> State -> Int
evalue (I v) _ = v
evalue (V s) state = value $ head $ filter (\(n,_) -> n == s) state
evalue (Add e1 e2) state = (evalue e1 state) + (evalue e2 state)
evalue (Sub e1 e2) state = (evalue e1 state) - (evalue e2 state)
evalue (Mul e1 e2) state = (evalue e1 state) * (evalue e2 state)

evalueCond :: CondExpression -> State -> Bool
evalueCond (B b) _ = b
evalueCond (Eq e1 e2) state = (evalue e1 state) == (evalue e2 state)
evalueCond (UnEq e1 e2) state = (evalue e1 state) /= (evalue e2 state)
evalueCond (Less e1 e2) state = (evalue e1 state) < (evalue e2 state)
evalueCond (Bigger e1 e2) state = (evalue e1 state) > (evalue e2 state)
evalueCond (LesEq e1 e2) state = (evalue e1 state) <= (evalue e2 state)
evalueCond (BigEq e1 e2) state = (evalue e1 state) >= (evalue e2 state)
evalueCond (Not cond) state = not $ evalueCond cond state
evalueCond (And c1 c2) state = (evalueCond c1 state) && (evalueCond c2 state)
evalueCond (Or c1 c2) state = (evalueCond c1 state) || (evalueCond c2 state)


findAndReplace :: Variable -> State -> State
findAndReplace var [] = [var]
findAndReplace (s,v) ((sv,vv):xs) = if s==sv then (s,v):xs else (sv,vv):(findAndReplace (s,v) xs)

executeInstruction :: Instruction -> State -> State
executeInstruction (Asign var expr) state = findAndReplace (var,evalue expr state) state
executeInstruction (While cond ins) state = if (evalueCond cond state) then executeInstruction (While cond ins) (execute ins state) else state

execute :: Program -> State -> State
execute (ins) s0 = foldl (\x y -> executeInstruction y x) s0 ins

ejecuta=execute

factorial :: Program
factorial = ["Y" =: V "X","R" =: I 1, While (I 0 <: V "Y") ["R" =: V "R" *: V "Y", "Y" =: V "Y" -: I 1]]

s0 :: State
s0 = [("X",20)]