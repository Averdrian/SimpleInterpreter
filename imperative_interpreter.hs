--ADRIAN ESTEVEZ GALLEGO

--definiciones mas basicas de tipos
type Variable = (String, Int) --las variables seran simplemente el identificador y su valor
type State = [Variable] --el estado del programa es su lista de variables, con sus respectivos valores
type Program = [Instruction] --un programas es simplemente una lista de instrucciones

--tenemos dos tipos de isntrucciones, la asignacion que iguala al identificador dado el valor de la expresion
--y la expresion condicional, que ejecutara sus instrucciones mientras su condicion sea cierta
data Instruction = Asign String Expression | While Expression Program
instance Show Instruction where
    show (Asign s e) = show s ++ " = " ++ show e

data Expression = I Int | V String | Suma Expression Expression | Resta Expression Expression | Mult Expression Expression
instance Show Expression where
    show (I x) = show x
    show (V s) = show s
    show (Suma e1 e2) = show e1 ++ " + " ++ show e2
    show (Resta e1 e2) = show e1 ++ " - " ++ show e2
    show (Mult e1 e2) = show e1 ++ " * " ++ show e2



evalue :: Expression -> State -> Int
evalue (I v) _ = v
evalue (V s) state = value $ head $ filter (\(n,_) -> n == s) state
evalue (Suma e1 e2) state = (evalue e1 state) + (evalue e2 state)
evalue (Resta e1 e2) state = (evalue e1 state) - (evalue e2 state)
evalue (Mult e1 e2) state = (evalue e1 state) * (evalue e2 state)


value :: Variable -> Int
value (_, v) = v

infixl 6 +:
(+:) :: Expression -> Expression -> Expression 
e1 +: e2 = Suma e1 e2

infixl 6 -:
(-:) :: Expression -> Expression -> Expression
e1 -: e2 = Resta e1 e2

infixl 7 *:
(*:) :: Expression -> Expression -> Expression
e1 *: e2 = Mult e1 e2

infixl 1 =:
(=:) :: String -> Expression -> Instruction
s =: e = Asign s e


findAndReplace :: Variable -> State -> State
findAndReplace var [] = [var]
findAndReplace (s,v) ((sv,vv):xs) = if s==sv then (s,v):xs else (sv,vv):(findAndReplace (s,v) xs)

executeInstruction :: Instruction -> State -> State
executeInstruction (Asign var expr) state = findAndReplace (var,evalue expr state) state

execute :: Program -> State -> State
execute (ins) s0 = foldl (\x y -> executeInstruction y x) s0 ins

ejecuta=execute