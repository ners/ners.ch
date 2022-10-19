---
order: 3
---

# 3. Lists

A list of things is specified with brackets:

```haskell
ghci> first3Numbers = [1, 2, 3]
```

What is the type of this list?

```
ghci> :t first3Numbers
first3Numbers :: Num a => [a]
```

Haskell has homogenous lists, which means the values inside them all have to be
the same type. That type is also written in brackets:


```haskell
ghci> first3Numbers = [1, 2, 3] :: [Int]
```

We can't cheat this rule.

```haskell
ghci> first4Numbers = [1, 2, 3, "sneaky"] :: [Int]

<interactive>:4:27: error:
    • Couldn't match type ‘[Char]’ with ‘Int’
      Expected: Int
        Actual: String
    • In the expression: "sneaky"
      In the expression: [1, 2, 3, "sneaky"] :: [Int]
      In an equation for ‘first4Numbers’:
          first4Numbers = [1, 2, 3, ....] :: [Int]
```

Even without specifying the type above, the compiler will deduce the type of the
list, and demand all the values are the same type.

An easier way to specify a list is by using comprehension:

```haskell
ghci> first100Numbers = [1..100] :: [Int]
```

And we can even have arithmetic sequences:

```haskell
ghci> oddNumbersUpTo100 = [1,3..100] :: [Int]
```

All these lists are **inclusive**, but the list of odd numbers skips the 100.

Lists can also be infinite:

```haskell
ghci> allTheNumbers = [1..] :: [Int]
ghci> allTheOddNumbers = [1,3..]
```

If we try to print an infinite list, the program never ends:

```haskell
ghci> print allTheOddNumbers
[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,127,129,131,133,135,137,139,141,143,145,147,149,151,153,155,157,159,161,163,165,167,169,171,173,175,177,179,181,183,185,187,189,191,193,195,197,199Interrupted
```

The only way to stop that program was to kill it with Ctrl+C.

Instead, we have to only take a piece of the list:

```haskell
ghci> print (take 10 allTheOddNumbers)
[1,3,5,7,9,11,13,15,17,19]
```

Notice that the program completes instantly, it doesn't get stuck computing that
infinite list. That's because Haskell is _lazy_. It knows how to compute the
list, but doesn't actually do it until you ask for it.

Printing the list asked for all of it. `take 10` asks for only the first 10
elements.

## Iterating over lists

Haskell has no loops. You heard that right, you can't loop over lists. Instead,
we _map_ over them. Consider that a loop is also a sort of map:

```
for (item in list) { do stuff on item }
```
becomes
```
map (do stuff) list
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
