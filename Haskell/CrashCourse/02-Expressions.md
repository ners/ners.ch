---
order: 2
---

# 2. Express yourself

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

Every value and every expression has a type. We can examine these types in GHCi with `:t`:

```
ghci> :t "Hello"
"Hello" :: String
ghci> :t True
True :: Bool
ghci> :t 2
2 :: Num p => p
```

Num is a typeclass that describes many number types. We'll tackle those in
another section.

Notice that all the types are capitalised. Everything that isn't a type is
camel-cased.

Expressions also have types:

```
ghci> :t (2 + 2)
(2 + 2) :: Num a => a
```

These types exist so we can't do incorrect stuff.

```
ghci> 2 + "Hello"

<interactive>:1:3: error:
    • No instance for (Num String) arising from a use of ‘+’
    • In the expression: 2 + "Hello"
```

GHC is complaining that we can't do `Num + String`. In some languages like
Javascript, we would expect `"2Hello"`, but in Haskell this type of conversion
is never automatic. We have to do it manually.

Oh, and the string concatenation operator is `++`, not just `+`.

```
ghci> show 2 ++ "Hello"
"2Hello"
```

`show` is a function that converts most things to string. Think of it as
`toString` in Java, or `str()` in Python and C++.
