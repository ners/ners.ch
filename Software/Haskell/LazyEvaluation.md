# Lazy evaluation

<img class="header-logo" src="./static/sloth.svg"/>

> Laziness was undoubtedly the single theme that united the various groups that contributed to Haskell's design.
> Once we were committed to a *lazy* language, a pure one was inescapable.
> :::cite
> Hudak, P., Hughes, J., Peyton Jones, S., & Wadler, P. (2007). [A History of Haskell: Being Lazy With Class][ahoh]

[ahoh]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf

Laziness = [non-strict semantics](./LazyEvaluation.md#non-strict-semantics) + [sharing](./LazyEvaluation.md#sharing)

Laziness is a common implementation technique for non-strict languages, but it is not the only possible one.

## Non-strict semantics

Strict semantics:
 - reduce expressions from inside out, e.g. evaluating `*` first in `a+(b*c)`
 - a subexpression that evaluates to [⊥] (i.e. an error or endless loop) will be found and propagated outwards

Non-strict semantics:
 - reduce expressions from outside in, e.g. evaluating `+` first in `a+(b*c)`
 - a subexpression that evaluates to ⊥ may be eliminated by outer reductions

With non-strict semantics we can define elegant control flow abstractions:
```haskell
unless :: Applicative f => Bool -> f () -> f ()
unless True x = x
unless False _ = pure ()

any :: (Foldable f, Functor f) => (a -> Bool) -> f a -> Bool
any p = or . fmap p

unless (any isPrime [0,1..]) $ fail "No prime numbers found in ℕ!"
```

Haskell is one of the few modern languages to have non-strict semantics by default.

In practice, Haskell is not a purely lazy language: for instance pattern matching is usually strict.[^1]

## Sharing

An easy way to improve performance is to [call something fewer times](http://neilmitchell.blogspot.com/2010/01/optimising-hlint.html).

Sharing stores subexpression results so that they can be reused instead of evaluated multiple times.
One way to do so is using `let`.

In the following example, `sqrt` is called twice in `result` but only once in `result'`:

```haskell
f x y = sqrt x + y
result = f 1 2 + f 1 4

f' x = let sqrt_x = sqrt x in \y -> sqrt_x + y
result' = let f_1 = f 1 in f_1 2 + f_1 4
```

In the following example, `fibSlow` is slower than `fibFast` because it redefines `fib'` for every value of `x`:

```haskell
fibSlow x =
    let fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n - 1) + fib (n - 2)
    in  map fib' [0 ..] !! x

fibFast =
    let fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n - 1) + fib (n - 2)
    in  (map fib' [0 ..] !!)
```

You can also use `where` for sharing, but the performance impact of the eta reduction becomes harder to see:

```haskell
fibSlow x = map fib' [0 ..] !! x
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)


-- This eta reduction is non-trivial!
fibFast = (map fib' [0 ..] !!)
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)
```

- [Lazy vs. non-strict - Haskell Wiki](https://wiki.haskell.org/Lazy_vs._non-strict)
- [Performance/Laziness - Haskell Wiki](https://wiki.haskell.org/Performance/Laziness)
- [Let vs. Where - Haskell Wiki](https://wiki.haskell.org/Let_vs._Where)
- Mitchell, N. (2011). [Sharing in Haskell](http://neilmitchell.blogspot.com/2011/09/sharing-in-haskell.html)

[History of Haskell]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
[non-strict]: https://wiki.haskell.org/Non-strict_semantics
[lazy]: https://wiki.haskell.org/Lazy_evaluation
[⊥]: https://wiki.haskell.org/Bottom

[^1]: Pattern matches can be made lazy by prepending them with `~`.
