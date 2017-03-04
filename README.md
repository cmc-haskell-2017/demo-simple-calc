# demo-simple-calc

Калькулятор простых выражений с переменными.

```
>>> display ((x + 3)^2 + y)
"(x + 3) * (x + 3) + y"
```

## Сборка и запуск

Клонируйте репозиторий:

```
git clone https://github.com/cmc-haskell-2017/demo-simple-calc.git
cd demo-simple-calc
```

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Взаимодействуйте с калькулятором, используя интерпретатор GHCi:

```
stack ghci
```

## Задание

В качестве задания к [лекции «Функторы»](https://youtu.be/k0nltRK0MUE) требуется определить [функцию `expandVars`](https://github.com/cmc-haskell-2017/demo-simple-calc/blob/master/src/SimpleCalc.hs#L105-L118).

Чтобы проверить реализацию, запустите тесты, проверяющие примеры из документации:

```
stack test
```
