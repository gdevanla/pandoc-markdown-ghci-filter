## my header

``` haskell
>> putStrLn "This string should show up in the output"

```
## Example 1

``` haskell
testFunc:: Integer -> Integer
testFunc x = x + 1

>> testFunc 13

anotherFunc:: Integer -> Integer
anotherFunc x = x * 2

>> (testFunc 10) + (anotherFunc 20)

```

## Example 2

``` haskell
testFunc:: Integer -> Integer
testFunc x = x + 1

>> testFunc 13

testFunc:: Integer -> Integer
testFunc x = x + 1
```

## Example 3

``` haskell
testFunc1:: Integer -> Integer
testFunc1 x = x + 1

>> testFunc1 13

testFunc2:: Integer -> Integer
testFunc2 x = x + 1

>> testFunc2 5
```

## Example 4

``` haskell

let x = ("string with a new line character\n\n\n")::String

>> x

```
