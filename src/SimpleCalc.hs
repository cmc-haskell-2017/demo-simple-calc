{-# LANGUAGE DeriveFunctor #-}
module SimpleCalc where

-- $setup
--
-- >>> let x = Var "x"
-- >>> let y = Var "y"
-- >>> let z = Var "z"

-- | Простое арифметическое выражение с переменными.
data Expr a
  = Lit Int                 -- ^ Целочисленный литерал.
  | Var a                   -- ^ Переменная.
  | Add (Expr a) (Expr a)   -- ^ Сложение.
  | Mul (Expr a) (Expr a)   -- ^ Умножение.
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
-- >>> let y = Var "y"
-- >>> let z = Var "z"
-- >>> 2 * x + y * z
-- Add (Mul (Lit 2) (Var "x")) (Mul (Var "y") (Var "z"))
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

-- | Вычислить выражение с подставленными значениями переменных.
--
-- >>> eval (Add (Var 1) (Var 2))
-- 3
--
-- >>> let x = Var 2
-- >>> let y = Var 3
-- >>> x + y
-- Add (Var 2) (Var 3)
-- >>> eval (x + y)
-- 5
eval :: Expr Int -> Int
eval (Lit n) = n
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- | Вычислить выражение, используя заданные значения переменных.
--
-- >>> let vars = [("x", 2), ("y", 3)]
-- >>> evalWith 0 vars (Add (Var "x") (Var "y"))
-- 5
--
-- >>> let x = Var "x"
-- >>> let y = Var "y"
-- >>> let z = Var "z"
-- >>> evalWith 0 vars ((x + y)^2 + z)
-- 25
evalWith :: Eq var => Int -> [(var, Int)] -> Expr var -> Int
evalWith def vars = eval . fmap valueOf
  where
    valueOf var = case lookup var vars of
      Just n  -> n
      Nothing -> def

-- | Перевести выражение в строковое представление.
--
-- >>> display (Mul (Var "x") (Add (Lit 1) (Var "y")))
-- "(x) * (1 + y)"
--
-- >>> display ((x + y)^2 + z)
-- "(x + y) * (x + y) + z"
display :: Expr String -> String
display (Lit n) = show n
display (Var s) = s
display (Add e1 e2) = display e1 ++ " + " ++ display e2
display (Mul e1 e2) = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"

-- | Перевести выражение в строковое представление,
-- используя заданную функцию отображения переменных.
--
-- >>> displayWith show (Mul (Var "x") (Add (Lit 2) (Var "y")))
-- "(\"x\") * (2 + \"y\")"
--
-- >>> displayWith display (Mul (Var (x + y)) (2 + Var (y^2)))
-- "(x + y) * (2 + (y) * (y))"
displayWith :: (var -> String) -> Expr var -> String
displayWith displayVar = display . fmap displayVar

-- | Подставить подвыражения вместе переменных в исходном выражении,
-- используя заданные значения переменных.
--
-- >>> let unknown = Var "<unknown>"
-- >>> let vars = [("x", y + z), ("y", x + 3)]
--
-- >>> display (expandVars unknown vars (x * y))
-- "(y + z) * (x + 3)"
--
-- >>> display (expandVars unknown vars ((y + z) * (x + 3)))
-- "(x + 3 + <unknown>) * (y + z + 3)"
--
-- expandVars :: ?
-- expandVars = ?
