# Part 2: Deciphering the Moon Language

In this section we explore the brave new world of Haskell's exotic syntax.

Rather than it being scary, we show ways to take it apart and make it
delightful to the eye and heart.

## Expressions and their types

Values and expressions are the same as in most languages. We have numbers,
strings, characters, bools, ...

```
ghci> 2
2
ghci> 2 + 2
4
ghci> "Hello"
"Hello"
ghci> a = 2
ghci> a
2
```

Every value and every expression has a type, which we can examine with `:t`:

```
ghci> :t "Hello"
"Hello" :: String
ghci> :t True
True :: Bool
ghci> :t 2
2 :: Num p => p
```

Num is a typeclass. We'll tackle those next time.

Notice that all the types are capitalised. Everything that isn't a type is
camel-cased.

Expressions also have types:

```
ghci> x = 2 + 2
ghci> :t x
a :: Num a => a
```

These types exist so we can't do incorrect stuff.

```
ghci> 2 + "Hello"

<interactive>:1:3: error:
    • No instance for (Num String) arising from a use of ‘+’
    • In the expression: 2 + "Hello"
```

GHC is complaining that we can't do `Num + String`. In some languages like
Javascript, we would expect "2Hello", but in Haskell this type of conversion is
never automatic.
We have to do it manually.

Oh, and the string concatenation operator is `++`, not just `+`.

```
ghci> show 2 ++ "Hello"
"2Hello"
```

`show` is a function that converts most things to string. Kind of like
`toString` in Java, or `str()` in Python.

## Functions and their types

A function is created the same way as a constant, there is no special keyword or
syntax:

```
ghci> a = 2
ghci> addTwo n = n + 2
```

Note that the parameter to the function goes before `=`, and there are no
paretheses.

We can also take two parameters:

```
ghci> add a b = a + b
```

Function calls similarly have no paretheses:

```
ghci> add 2 3
5
```

Each function has a type, just like expressions do:

```
ghci> :t addTwo
addTwo :: Num a => a -> a
```

The way to read this is:
- ignore the `Num a =>` part for now, we'll go there next time
- let's simplify to ints: `addTwo :: Int -> Int`
- the `a -> a` part is a chain of parameters types separated by `->`
- the last type in that chain is the return type

A function can only return one thing, so it can only have one return type!

To return more things, we have to choose a list or a tuple, e.g. `Int -> (Int, Bool)`

Notice that despite everything having types, we haven't written any types
manually. Haskell is very strict about types, but we never have to specify
them.

That's because the compiler already knows the real type of the function, based
on what it does.

But the human coder can still specify the type above the function. For this we
need to break out of GHCi and start writing a new Haskell program:


{ data-filename="hello.hs" }
```haskell
addTwo :: Int -> Int
addTwo n = n + 2
```

A Haskell program needs to have a `main` as well:


```haskell
addTwo :: Int -> Int
addTwo n = n + 2

main = print (addTwo 3)
```

Now we can either compile the program:
```bash
ghc hello.hs
./hello
```

Or better yet, interpret it like Python:
```bash
runhaskell hello.hs
```

Let's look at the function again:


```haskell
addTwo :: Int -> Int
addTwo n = n + 2
```

Remember that the type declaration is optional, the compiler already knows the
type. So why write it?

It's there for the human, not for the compiler. The human writes out their
assumptions about how this function behaves, before writing it. The compiler
then checks if those assumptions are correct, and complains if they aren't.

```haskell
addTwo :: Int -> Int -> Int
addTwo n = n + 2
```

```
hello.hs:2:12: error:
    • Couldn't match expected type ‘Int -> Int’ with actual type ‘Int’
    • In the expression: n + 2
      In an equation for ‘addTwo’: addTwo n = n + 2
  |
2 | addTwo n = n + 2
  |            ^^^^^
```

Here we said that the function takes two parameters and returns an int, but it
actually takes just one parameter!

Let's look at the `main` function again:


```haskell
main = print (addTwo 3)
```

This function also has a type: `IO ()`. We'll explain it next time.

More interestingly, why did we put `addTwo 3` in paretheses?

If we didn't, the `print` function would take 2 parameters: a function and an
int:

```haskell
main = print addTwo 3
```

```
hello.hs:4:8: error:
    • Couldn't match expected type: t0 -> t
                  with actual type: IO ()
    • The function ‘print’ is applied to two value arguments,
        but its type ‘(Int -> Int) -> IO ()’ has only one
      In the expression: print addTwo 3
      In an equation for ‘main’: main = print addTwo 3
    • Relevant bindings include main :: t (bound at hello2.hs:4:1)
  |
4 | main = print addTwo 3
  |        ^^^^^^^^^^^^^^
```

It doesn't like that. The type of `print` is `Show a => a -> IO ()`. It only
accepts a single parameter to print. We separate the function call by
introducing explicit precedence with paretheses.

Another example of this is string concatenation:

```haskell
main = print "Hello," ++ " Idaho"
```

```
hello.hs:4:1: error:
    • Couldn't match expected type: IO t0
                  with actual type: [Char]
    • When checking the type of the IO action ‘main’
  |
4 | main = print "Hello," ++ " Idaho"
  | ^^^^

hello.hs:4:8: error:
    • Couldn't match expected type: [Char]
                  with actual type: IO ()
    • In the first argument of ‘(++)’, namely ‘print "Hello,"’
      In the expression: print "Hello," ++ " Idaho"
      In an equation for ‘main’: main = print "Hello," ++ " Idaho"
  |
4 | main = print "Hello," ++ " Idaho"
  |        ^^^^^^^^^^^^^^
```

What happened there? Function application binds stronger than `++`, so our
expression really looks more like this:

```haskell
main = (print "Hello,") ++ " Idaho"
```

which is _certainly_ not what we want.

## Lists and ... you guessed it, their types

A list of things is specified with brackets:

```haskell
first3Numbers = [1, 2, 3]
```

What is the type of this list?

Haskell has homogenous lists, which means the values inside them all have to be
the same type. That type is also written in brackets:


```haskell
first3Numbers :: [Int]
first3Numbers = [1, 2, 3]
```

We can't cheat this rule.


```haskell
first4Numbers :: [Int]
first4Numbers = [1, 2, 3, "sneaky"]
```

```
hello.hs:2:27: error:
    • Couldn't match type ‘[Char]’ with ‘Int’
      Expected: Int
        Actual: String
    • In the expression: "sneaky"
      In the expression: [1, 2, 3, "sneaky"]
      In an equation for ‘first4Numbers’: first4Numbers = [1, 2, 3, ....]
  |
2 | first4Numbers = [1, 2, 3, "sneaky"]
  |                           ^^^^^^^^
```

Even without specifying the type above, the compiler will deduce the type of the
list, and demand all the values are the same type.

An easier way to specify a list is by using comprehension:

```haskell
first100Numbers :: [Int]
first100Numbers = [1..100]
```

And we can even have arithmetic sequences:

```haskell
oddNumbersUpTo100 :: [Int]
oddNumbersUpTo100 = [1,3..100]
```

All these lists are **inclusive**, but the list of odd numbers skips the 100.

Lists can also be infinite:

```haskell
allTheNumbers :: [Int]
allTheNumbers = [1..]

allTheOddNumbers :: [Int]
allTheOddNumbers = [1,3..]
```

If we try to print an infinite list, the program never ends:

```haskell
main = print allTheOddNumbers
```

Instead, we have to only take a piece of the list:

```haskell
main = print (take 10 allTheOddNumbers)
```

```
[1,3,5,7,9,11,13,15,17,19]
```

Notice that the program completes instantly, it doesn't get stuck computing that
infinite list. That's because Haskell is _lazy_. It knows how to compute the
list, but doesn't actually do it until you ask for it.

Printing the list asked for all of it. `take 10` asks for only the first 10
elements.

Haskell has no loops. You heard that right, you can't loop over lists. Instead,
we _map_ over them. Consider that a loop is also a sort of map:

```
for <list> { do stuff }
```
becomes
```
map { do stuff } <list>
```

The first parameter to `map` is a function that accepts one parameter and
returns a new value. The second parameter to `map` is the list of input values.
The result of `map` is the list of output values:

```haskell
map f [x1, x2, x3, ...] = [f x1, f x2, f x3, ...]
```

Another useful function is `filter`. It takes a predicate function and a list,
and keeps just the values in the list that pass the predicate:

```haskell
filter isPrime [1..] = [2, 3, 5, 7, 11, ...]
```
