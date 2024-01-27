--ADRIAN ESTEVEZ GALLEGO

--definiciones mas basicas de tipos
type Variable = (String, Int) --las variables son el identificador y su valor
type State = [Variable] --el estado del programa es su lista de variables
type Program = [Instruction] --un programas es simplemente una lista de instrucciones

--tenemos dos tipos de isntruciones, la asignacion que iguala al identificador dado el valor de la expresion
--y la expresion condicional, que ejecutara sus instrucciones mientras su condicion sea cierta
data Instruction = Asign String Expression | While CondExpression Program | Cond CondExpression Program Program deriving Show
-- instance Show Instruction where #TODO: Mirar los show de esto
--     show (Asign s e) = show s ++ " = " ++ show e

-- instance Read Instruction where
--     readsPrec _ str = [(readInstruction str, "")]

-- readInstruction :: String -> Instruction
-- readInstruction str = case words str of
--     [var, "=:", expStr] -> Asign var (read expStr)
--     ["While", condStr, insStr] -> While (read condStr) (read insStr)
--     ["Cond", condStr, trueStr, falseStr] -> Cond (read condStr) (read trueStr) (read falseStr)
--     _ -> error "Formato de instrucción no válido"



infix 1 =:
(=:) :: String -> Expression -> Instruction
s =: e = Asign s e


--expressiones basicas del programa, todas se reduccen a un entero (ver la funcion evalue)
data Expression = I Int | V String | Add Expression Expression | Sub Expression Expression | Mul Expression Expression deriving Read
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


--expresiones condicionales, todas se reduccen en ultima instancia a un valor booleano (ver la funcion evalueCond), se usaran para las instrucciones condicionales y la instruccion while
data CondExpression = B Bool | Eq Expression Expression | UnEq Expression Expression | Less Expression Expression | Bigger Expression Expression | LesEq Expression Expression | BigEq Expression Expression | And CondExpression CondExpression | Or CondExpression CondExpression | Not CondExpression deriving (Show, Read)

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


--valor de una variable
value :: Variable -> Int
value (_, v) = v

--evaluar las expresiones basicas
evalue :: Expression -> State -> Int
evalue (I v) _ = v
evalue (V s) state = value $ head $ filter (\(n,_) -> n == s) state
evalue (Add e1 e2) state = (evalue e1 state) + (evalue e2 state)
evalue (Sub e1 e2) state = (evalue e1 state) - (evalue e2 state)
evalue (Mul e1 e2) state = (evalue e1 state) * (evalue e2 state)

--evaluar una expresion condicional
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

--al asignar un valor a una variable, hay que modificarlo en el estado del programa, esta funcion recursiva realiza esa tarea
findAndReplace :: Variable -> State -> State
findAndReplace var [] = [var] --caso base, aqui llegara si no encuentra la variable en el estado, en ese caso devuelve un estado con esa variable
findAndReplace (s,v) ((sv,vv):xs) = if s==sv then (s,v):xs else (sv,vv):(findAndReplace (s,v) xs) --si la variable no esta en la cabeza del estado, se llama recursivamente

--el resultado de ejecutar una unica instruccion, dada una intruccion y el estado del programa devuelve el estado despues de ejecutar dicha instruccion
executeInstruction :: Instruction -> State -> State
executeInstruction (Asign var expr) state = findAndReplace (var,evalue expr state) state
executeInstruction (While cond ins) state = if (evalueCond cond state) then executeInstruction (While cond ins) (execute ins state) else state
executeInstruction (Cond cond true_ins false_ins) state = if (evalueCond cond state) then execute true_ins state else execute false_ins state

--dado un programa y un estado inicial, devuelve el estado del programa al terminar
execute :: Program -> State -> State
execute ins s0 = foldl (\x y -> executeInstruction y x) s0 ins

--segun los requisitos de la practica, el resultado de la ejecucion no es el estado completo del programa, sino solamente el valor de la variable R
--esta funcion simplemente devuelve del estado del programa el valor de R
getResult :: State -> Int
getResult state = (\(n,v) -> v) $ head $ filter (\(n,v) -> n == "R") state

--devuelve el resultado de R al ejecutar el programa dado
ejecuta :: Program -> State -> Int
ejecuta prog st = getResult $ execute prog st

--programa de prueba
factorial :: Program
factorial = ["Y" =: V "X","R" =: I 1, While (I 0 <: V "Y") ["R" =: V "R" *: V "Y", "Y" =: V "Y" -: I 1]]

s0 :: State
s0 = [("X",3)]


-- loadFile =
--     do 
--         putStr "Escribe el nombre del fichero a cargar: "
--         input <- getLine
--         content <- readFile input
--         -- let lineas = parseFile content
--         let programa = read content :: Program
--         return programa


-- -- parseFile :: [Char] ->[Instruction]
-- -- parseFile xs = lines xs