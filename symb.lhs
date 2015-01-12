*** A Symbolic Haskel Experiment ***

>module Symbolic 
>        ( Expr(..)
>        , x,y,z,a,b,c
>        , simplify
>        , diff
>        , eval)
>        where
>
>import Data.Maybe

Here we explore some of the basics of symbolic maths in Haskell. This
is by no means a complete code, fit for anything other than
experimentation.

First we start off by defining a recursive data type to represent our
expression. This data type defines our AST that will represent
symbolic expression.

>data Expr a = Atom a
>            | Sum  (Expr a) (Expr a)
>            | Prod (Expr a) (Expr a)
>            | Exp  (Expr a) (Expr a)
>            | Log  (Expr a)
>            | Sin  (Expr a)
>            | Cos  (Expr a)
>            | Rec  (Expr a)
>            | Neg  (Expr a)
>            | E
>            | Symbol String
>            deriving(Eq)

We want to be able to build ASTs out of simple expressions
automatically (like (sin x / x)). To be able to do so, we must make
our newly created type Expr an instance of Num (+,-,*,etc), Fractional
(/), Floating (trig, exp, etc).

>instance (Num a) => Num (Expr a) where
>  (+) = Sum
>  a - b = Sum a (Neg b)
>  (*) = Prod
>  negate = Neg
>  signum = undefined
>  abs    = undefined
>  fromInteger a = Atom (fromInteger a)
>
>instance (Floating a) => Fractional (Expr a) where
>  a / b = Prod a (Rec b)
>  fromRational a = Atom (fromRational a)
>
>instance (Floating a) => Floating (Expr a) where
>  pi    = Atom pi
>  exp   = Exp E
>  sqrt  = flip Exp (1/2)
>  log   = Log
>  sin   = Sin
>  tan a = Prod (Sin a) (Rec (Cos a))
>  cos   = Cos
>  (**)  = Exp
>  logBase a b = Prod (Log a) (Rec (Log b))
>  asin  = undefined
>  atan  = undefined
>  acos  = undefined
>  sinh  = undefined
>  tanh  = undefined
>  cosh  = undefined
>  asinh = undefined
>  atanh = undefined
>  acosh = undefined

We could derive Show, but it is much nicer to display similar stuff to
what we have typed in when defining expression. TODO: use diff lists.

>instance (Show a) => Show (Expr a) where
>  show (Atom a)   = show a
>  show (Symbol a) = a
>  show (Prod a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
>  show (Sum  a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
>  show (Neg a)    = '-' : show a
>  show (Rec a)    = "1/" ++ show a
>  show (Sin a)    = "sin(" ++ show a ++ ")"
>  show (Cos a)    = "cos(" ++ show a ++ ")"
>  show (Log a)    = "log(" ++show a++ ")"
>  show (Exp a b)  = "(" ++ show a ++ "**" ++ show b ++ ")"
>  show (E)        = "e"

We define some useful symbols.

>x = Symbol "x"
>y = Symbol "y"
>z = Symbol "z"
>a = Symbol "a"
>b = Symbol "b"
>c = Symbol "c"

And we are ready to do some symbolic differentiation. Here we present
two auxiliary functions that will be used for computing partial
derivatives of functions. s simplifies an expression, d calculates its
derivative. Both work by simple pattern matching over our Expr AST.

>s :: (Fractional a, Eq a) => Expr a -> Expr a
>s (Sum (Atom a) (Atom b))  = Atom (a+b)
>s (Prod (Atom a) (Atom b)) = Atom (a*b)
>s (Sum (Atom 0) a)  = s a
>s (Sum a (Atom 0))  = s a
>s (Prod (Atom 0) a) = Atom 0
>s (Prod a (Atom 0)) = Atom 0
>s (Prod (Atom 1) a) = s a
>s (Prod a (Atom 1)) = s a
>s (Prod (Atom a) (Prod (Atom b) c)) = Prod (Atom $ a*b) c
>s (Sum  a b) | a == b    = 2 * s a
>             | otherwise = s a + s b
>s (Prod a b) = s a * s b
>s a = a


>d :: (Floating a) => String -> Expr a -> Expr a
>d x (Atom a)   = 0
>d x (Sum a b)  = d x a + d x b
>d x (Prod a b) = (d x a * b) + (a * d x b)
>d x (Rec a)    = - (d x a)/(a * a)
>d x (Neg a)    = -(d x a)
>d x (Log a)    = 1/a * (d x a)
>d x (Sin a)    = (cos a)*(d x a)
>d x (Cos a)    = -(sin a)*(d x a)
>d x (Exp E b)  = (exp b)*(d x b)
>d x (Exp (Symbol a) (Atom b)) 
>               | a == x    = (Atom b) * ((Symbol a) ** (Atom (b-1)))
>               | otherwise = 0
>d x (Exp a b)  = (a ** b)*((d x a) * b/a + (d x b) * log a)
>d x (Symbol y) | x == y    = 1
>               | otherwise = 0


>simplify a = f a
>  where
>    b = s a
>    f a | b == a = a
>        | otherwise  = simplify b

>diff (Symbol x) a = simplify $ d x a
>diff _ _ = error "First argument must be a symbol"

We will need an evaluator to evaluate our expressions for given
environment.

>eval :: (Floating a) => [(String, a)] -> Expr a -> a
>eval _ (Atom a)     = a
>eval env (Sum a b)  = eval env a + eval env b
>eval env (Prod a b) = eval env a * eval env b
>eval env (Neg a)    = -(eval env a)
>eval env (Rec a)    = recip (eval env a)
>eval env (Symbol a) = fromMaybe (error ("No "++ a ++" in env")) (lookup a env)
>eval env (Exp E b)  = exp (eval env b)
>eval env (E)        = exp 1
>eval env (Exp a b)  = eval env a ** eval env b
>eval env (Sin a)    = sin (eval env a)
>eval env (Cos a)    = cos (eval env a)
>eval env (Log a)    = log (eval env a)


Now we can do a test run:

-->f = (x**3 + 2*x**6)
-->diff x f

will output (3.0 * (x**2.0)) + (12.0 * (x**5.0))