---
order: 1
---

# 1. Hello, Haskell!

In this step we'll get a Haskell environment up and running.

## Preparation

We can run Haskell programs in the browser without installing anything:

https://play.haskell.org/

To run Haskell on our computer, we have to install the GHC[^1].

If you are on Linux, macOS, or Windows with WSL, then the recommended method of
getting a development environment going is Nix with flakes:
1. [Install Nix][nix-install]
2. [Enable flakes][nix-flakes]
3. Run `nix shell nixpkgs#ghc`

To proceed without Nix, follow the instructions [here](https://www.haskell.org/ghcup/install/#installation).

[nix-install]: https://nixos.org/download.html
[nix-flakes]: https://nixos.wiki/wiki/Flakes

Verify that you have the GHC in your environment:

```
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.0.2
```

[^1]: Short for _Glasgow Haskell Compiler_. [GHC Homepage](https://www.haskell.org/ghc/)

## GHCi: the Haskell playground

In either Replit or local installation of the GHC, we can enter an interactive environment with GHCi:

```
$ ghci
GHCi, version 9.0.2: https://www.haskell.org/ghc/  :? for help
ghci>
```

If using Nix, here is a quick one-liner:

```
$ nix shell nixpkgs#ghc --command ghci
```

## Running our first program

Create the file `hello.hs` with the following contents:

{ data-filename="hello.hs" }
```haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```

We can run the program directly with the following command:

```
$ runhaskell hello.hs
Hello, Haskell!
```

We can also compile the program to an executable, named the same as the source file:

```
$ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...

$ ./hello
Hello, Haskell!
```

On Linux and macOS, the executable will have no extension. On Windows it will have the extension `.exe`.
