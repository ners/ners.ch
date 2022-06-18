---
order: 3
---

# 3. Lists

A list of things is specified with brackets:

```haskell
first3Numbers = [1, 2, 3]
```

What is the type of this list?

```
ghci> :t first3Numbers
first3Numbers :: Num a => [a]
```

Haskell has homogenous lists, which means the values inside them all have to be
the same type. That type is also written in brackets:


```haskell
first3Numbers = [1, 2, 3] :: [Int]
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
print allTheOddNumbers
```

Instead, we have to only take a piece of the list:

```haskell
print (take 10 allTheOddNumbers)
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
