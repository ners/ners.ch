# Programming without computers

> Programs must be written for people to read, and only incidentally for machines to execute.
> 
> — Harold Abelson, [Structure and Interpretation of Computer Programs][sicp]

[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-7.html

Programming has as little to do with computers as astronomy has to do with
telescopes. Computers allow us to evaluate some aspects of a program, but the
process of building and reasoning about a program happens outside a computer.

Programming primarily happens in our minds, on paper or whiteboard, and in
conversation.

## Code is a tool for communication

A majority of code is written by humans, for humans, not for computers. It may
happen that at some point our code will be evaluated by a computer, but that is
secondary to its true purpose of _communicating intent_.

This is why we don't delete source code after compiling it, but track it with
version control, keep redundant backups, protect it against tampering, and
generally treat it with the greatest of care.

One could argue that the only output of source code is its machine
representation; the stream of instructions that the computer will run, produced
either by a compiler or an interpreter. But that's not the programmer's intent,
either. One writes a program to express computation, not the means by which it
will be evaluated.

Consider the following program:

{ data-filename="primes.hs" }
```haskell
main :: Monad m => m Integer
main = return $ primes !! 99999

primes :: [Integer]
primes = 2 : 3 : filter isPrime [5,7..]

isPrime :: Integer -> Bool
isPrime = null . primeDivisors

primeDivisors :: Integer -> [Integer]
primeDivisors n = [ p | p <- takeWhile ((<=n) . (^2)) primes, n `mod` p == 0 ]
```

The purpose of this program is to compute the 100&#x202F;000th prime number
$p_{10^5}$. The program does not concern itself with anything else; it consumes
no input from the user or the environment, makes no assumptions on where or how
it is being run, and does not alter the state of the world around it.

Crucially, it does not even print the result, as [observing the result may
change it][observer]. In fact it has no means to do so; we have used the type
system to strip the program of the ability to perform IO.[^1]

[observer]: https://en.wikipedia.org/wiki/Observer_effect_(physics)

After code review to ensure its purity and correctness, we may now reasonbly
conclude that this program is _a valid representation of_ $p_{10^5}$.

There is practical benefit to this realisation. The numeric value of
$p_{10^5}$, as produced by evaluating the program on my computer, is
1&#x202F;299&#x202F;709. One cannot reasonably verify this claim through
review. The value of the number gives no hints to its provenance, which in this
case has only been established through fiat.

Reviewing the code that produced it lends confidence in the result's
correctness, even when the result does not.

It just so happens that the code for the program is longer than its result.
Given a large enough number to compute, the output may conceivably exceed the
size of the code. This principle is leveraged by [self-extracting
archives][archives].

[archives]: https://en.wikipedia.org/wiki/Self-extracting_archive

[^1]: We can evaluate such programs using GHCi.
