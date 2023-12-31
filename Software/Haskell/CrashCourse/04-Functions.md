---
order: 4
---

# 4. Functions

A function is created the same way as a constant, there is no special keyword or
syntax:

```
ghci> two = 2
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
- ignore the `Num a =>` part for now, we'll cover that in another section.
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

```haskell
addTwo :: Int -> Int
addTwo n = n + 2
```

A Haskell program needs to have a `main` as well:

{ data-filename="hello.hs" }
```haskell
addTwo :: Int -> Int
addTwo n = n + 2

main = print (addTwo 3)
```

Now we can either compile the program:
```bash
$ ghc hello.hs
./hello
```

Or better yet, interpret it like Python:
```bash
$ runhaskell hello.hs
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

{ data-filename="GHC output" }
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

{ data-filename="hello.hs" }
```haskell
main = print (addTwo 3)
```

This function also has a type: `IO ()`. We'll explain it next time.

More interestingly, why did we put `addTwo 3` in paretheses?

If we didn't, the `print` function would take 2 parameters: a function and an
int:

{ data-filename="hello.hs" }
```haskell
main = print addTwo 3
```

{ data-filename="GHC output" }
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

{ data-filename="hello.hs" }
```haskell
main = print "Hello," ++ " Idaho"
```

{ data-filename="GHC output" }
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
