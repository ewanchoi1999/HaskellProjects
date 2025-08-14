AFP Coursework 2 - Monadic Compiler

Viren Vadhvana, Ewan Choi 
psyvv2@nottingham.ac.uk, psycc9@nottingham.ac.uk

--------------------------------------------------------------------------------


Imports:

> import Data.List
> import Control.Monad.Writer 
> import Control.Monad.Trans
> import Control.Monad.Reader


Imperative language:

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seq [Prog]
>             deriving Show
>
> data Expr = Val Int | Var Name | App Op Expr Expr
>             deriving Show
>
> type Name = Char
>
> data Op   = Add | Sub | Mul | Div
>             deriving Show

Factorial example:
Test cases to test out the function

> fac :: Int -> Prog
> fac n = Seq [Assign 'A' (Val 1),
>              Assign 'B' (Val n),
>              While (Var 'B') (Seq
>                 [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>                  Assign 'B' (App Sub (Var 'B') (Val 1))])]



Virtual machine:

> type Stack = [Int]
>
> type Mem   = [(Name,Int)]
>
> type Code  = [Inst]
> 
> data Inst  = PUSH Int
>            | PUSHV Name
>            | POP Name
>            | DO Op
>            | JUMP Label
>            | JUMPZ Label
>            | LABEL Label
>              deriving Show
> 
> type Label = Int

State monad:

> type State = Label
>
> newtype ST a = S (State -> (a, State))
>
> app :: ST a -> State -> (a,State)
> app (S st) x 	=  st x
>
> instance Functor ST where
>    -- fmap :: (a -> b) -> ST a -> ST b
>    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
>
> instance Applicative ST where
>    -- pure :: a -> ST a
>    pure x = S (\s -> (x,s))
>
>    -- (<*>) :: ST (a -> b) -> ST a -> ST b
>    stf <*> stx = S (\s ->
>       let (f,s')  = app stf s
>           (x,s'') = app stx s' in (f x, s''))
>
> instance Monad ST where
>    -- return :: a -> ST a
>    return = pure
>
>    -- (>>=) :: ST a -> (a -> ST b) -> ST b
>    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


% EXCERCISE 1

% Returns the code (list of instructions) that are needed to perform the operation 
% E.g. for (1+2)*(3+4)

% INPUT: compexpr (App Mul (App Add (Val 1) (Val 2)) (App Add (Val 3) (Val 4) ) )
% OUTPUT: [PUSH 1,PUSH 2,DO Add,PUSH 3,PUSH 4,DO Add,DO Mul]

% > compexpr :: Expr -> Code
% > compexpr (Val i) = [PUSH i]
% > compexpr (Var x) = [PUSHV x]
% > compexpr (App op x y) = compexpr x ++ compexpr y ++ [DO op]

% Uses compprog' function to compile a program and returns the code a ST monad

% > compprog :: Prog -> ST Code
% > compprog prog = do
% >   code <- compprog' prog
% >   return code

% When the type of data is Assign, it returns code that pops the expression into the name. Use return due to the monadic type

% > compprog' :: Prog -> ST Code
% > compprog' (Assign name expr) =
% >   return (compexpr expr ++ [POP name])


% When the type fo data is "If", recursively compile the subprograms. Combine them and jump to new labels if needed

% > compprog' (If expr prog1 prog2) = do
% >   l1 <- freshLabel
% >   l2 <- freshLabel
% >   p1 <- compprog' prog1 
% >   p2 <- compprog' prog2
% >   let code = compexpr expr ++ [JUMPZ l1] ++ p1 ++ [JUMP l2, LABEL l1] ++ p2 ++ [LABEL l2]
% >   return code

% When the type of data is "While", concatenate the instructions to keep executing them and only jump to the end when evaluating false

% > compprog' (While expr prog)= do
% >   l1 <- freshLabel
% >   l2 <- freshLabel
% >   p <- compprog' prog
% >   let code = [LABEL l1] ++ compexpr expr ++ [JUMPZ l2] ++ p ++ [JUMP l1, LABEL l2]
% >   return code

% Recursively generate code for each program in the sequence. Base case is empty list which returns empty. Otherwise, concatenation of resulting programs is returned

% > compprog' (Seq []) = return []
% > compprog' (Seq (prog:progs)) = do 
% >   p <- compprog' prog 
% >   ps <- compprog' (Seq progs)
% >   return $ p ++ ps

% Generates new label by updating state monad with new counter value and returning previous value 

% > freshLabel :: ST Label
% > freshLabel = S (\n -> (n, n+1))

% Takes a compiled program from the st monad

% > comp :: Prog -> Code 
% > comp p = fst (app (compprog (p)) 0) 

> subToNum :: Int -> Int -> Prog
> subToNum start goal = Seq [Assign 'A' (Val 0),
>              Assign 'B' (Val goal),
>              Assign 'C' (Val 1),
>              Assign 'D' (Val 1),
>              Assign 'E' (Val start),
>              Assign 'F' (Val 1),
>              While (Var 'D') (Seq
>                 [If (Var 'C') 
>                  (Assign 'A' (App Add (Var 'A') (Val 1))) 
>                  (Assign 'D' (Val 0)), 
>                   Assign 'F' (App Add (Var 'A') (Var 'E')),
>                  Assign 'C' (App Sub (Var 'B') (Var 'F') )])]

EXCERCISE 2 


Initialises an empty memory and stack and takes in a copy of the code

> exec :: Code -> Mem
> exec c = m 
>          where
>              (m,_,_) = exec' ([],[],c) c


The base case of the helper function. When there is no more code to execute the memory and stack are returned with empty list of code

> exec' :: (Mem, Stack, Code) -> Code -> (Mem, Stack, Code)
> exec' (m, s, []) c = (m, s, [])

Pushes the specified integer onto the stack and removes it from the code list 

> exec' (m, s, (PUSH i):is) c  = exec' (m, i:s, is) c

Pushes the value at the specified location to the stack if it exists, otherwise moves onto the next instruction. 

> exec' (m, s, (PUSHV n):is) c = case lookup n m of
>                                Just v -> exec' (m, v:s, is) c
>                                Nothing -> exec' (m, s, is) c


Pops a value from the stack into a memory location using the helper function storeMem 
StoreMem stores the value in a memory location if it exists or creates it and stores it in the new location.

> exec' (m, h:s, (POP n):is) c = exec' (storeMem n h m, s, is) c


Pops two values from the stack, performs an operation on it and pushes the value back on the stack 

> exec' (m, y : x : s, DO op : is) c = exec' (m, op' x y : s, is) c
>   where
>     op' = case op of
>             Add -> (+)
>             Sub -> (-)
>             Mul -> (*)
>             Div -> div

Drops all instructions until label is found 

> exec' (m, s, JUMP l : is) c = exec' (m, s, dropWhile (not . isLabel l) c) c
>   where
>     isLabel l (LABEL i) | i == l = True
>     isLabel _ _                   = False

If the head of the stack is zero, jumps, otherwise does the same as label 
(which just moves onto the next instruction by removing current one and preserving memory and stack)

> exec' (m, h:s, (JUMPZ l):is) c 
>   | h == 0 = exec' (m, s, (JUMP l):is) c
>   | otherwise = exec' (m, s, (LABEL l):is) c
> exec' (m, s, (LABEL l):is) c = exec' (m, s, is) c

> storeMem :: Name -> Int -> Mem -> Mem
> storeMem n i mem = case mem of
>     [] -> [(n, i)]
>     ((n', i'):vs) -> if n == n'
>                         then (n', i) : vs
>                         else (n', i') : storeMem n i vs

Try program with other examples than just fac

BONUS EXCERCISE


Takes expression and generates code.
If a value is passed as input, it is pushed onto the stack 
If the input is a variable, the value at the variable in memory is pushed onto the stack 
If the input is an operation then the two values are evaluated.
The tell function tells the writer monad what the output is. 

> compexpr :: Expr -> WriterT Code ST ()
> compexpr (Val i) = tell [PUSH i]
> compexpr (Var n) = tell [PUSHV n]
> compexpr (App op x y) = do 
>   compexpr x
>   compexpr y
>   tell [DO op]


Generates new label by updating state monad with new counter value and returning previous value 

> freshLabel :: WriterT Code ST (Label)
> freshLabel = do
>   l <- lift(S (\n -> (n, n+1)))
>   return l


Takes a variable name and expression and generates code to assign value to variable.
Then it pops the value from the stack 

> compprog :: Prog -> WriterT Code ST ()
> compprog (Assign n e) = do 
>   compexpr e
>   tell [POP n]

Takes expression and two programs and then executes the first program if the expression is True
If the expression is true it executes the second program. Labels are used to control flow. 

> compprog (If e p1 p2) = do
>   l1 <- freshLabel
>   l2 <- freshLabel
>   compexpr e
>   tell [JUMPZ l1] 
>   compprog p1
>   tell [JUMP l2, LABEL l1] 
>   compprog p2
>   tell [LABEL l2]

takes expression and program and continuously executes code while the expression is true. 

> compprog (While e p) = do
>   l1 <- freshLabel
>   l2 <- freshLabel
>   tell [LABEL l1] 
>   compexpr e 
>   tell [JUMPZ l2] 
>   compprog p 
>   tell [JUMP l1, LABEL l2]


Recursively generate code for each program in the sequence. 
Base case is empty list which returns empty. 
Otherwise, concatenation of resulting programs is returned

> compprog (Seq []) = tell []
> compprog (Seq (prog : progs)) = do
>   compprog prog
>   compprog (Seq progs)


This function returns the compiled code of a program.

> comp :: Prog -> Code
> comp p = fst (app (execWriterT (compprog p)) 0)

--------------------------------------------------------------------------------