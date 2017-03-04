{-# LANGUAGE DeriveFunctor #-}
module SimpleCalc where

data Expr a
  = Lit Int
  | Var a
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Show, Functor)

eval :: Expr Int -> Int
eval (Lit n) = n
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith def vars = eval . fmap valueOf
  where
    valueOf var = case lookup var vars of
      Just n  -> n
      Nothing -> def

display :: Expr String -> String
display (Lit n) = show n
display (Var s) = s
display (Add e1 e2) = display e1 ++ " + " ++ display e2
display (Mul e1 e2) = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"

displayWith :: (var -> String) -> Expr var -> String
displayWith displayVar = display . fmap displayVar


