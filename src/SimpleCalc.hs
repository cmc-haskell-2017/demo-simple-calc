{-# LANGUAGE DeriveFunctor #-}
module SimpleCalc where

data Expr a
  = Lit Int
  | Var a
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  deriving (Show, Functor)

-- | Класс 'Num' позволяет записывать выражения типа 'Expr' удобным образом.
--
-- Например, мы можем использовать целочисленные литералы:
--
-- >>> 1 + 2 :: Expr a
-- Add (Lit 1) (Lit 2)
--
-- Или работать с переменными прямо в интерпретаторе:
--
-- >>> let x = Var "x"
-- >>> 2 * x
-- Mul (Lit 2) (Var "x")
--
-- Мы можем также использовать операции, которые используют класс типов 'Num':
--
-- >>> (x + y) ^ 2
-- Mul (Add (Var "x") (Var "y")) (Add (Var "x") (Var "y"))
instance Num (Expr a) where
  e1 + e2 = Add e1 e2
  e1 * e2 = Mul e1 e2
  fromInteger n = Lit (fromInteger n)

  abs    = error "not implemented"
  signum = error "not implemented"
  negate = error "not implemented"

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


