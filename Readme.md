% Types and Typos
% Martin Heuschober; [CC-BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/)
% 4. Jan 2015

<!--
<link rel="stylesheet" href="highligh.js/styles/solarized_light.css">
<link rel="stylesheet" href="highlight.js/styles/monokai_sublime.css">-->
<link rel="stylesheet" href="highlight.js/styles/github.css">
<link rel="stylesheet" href="reveal.js/css/reveal.css"/>
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>

<!--!pandoc -t revealjs -s % -o index.html -->

Haskell
=======

Installation
------------

> goto [haskell.org](https://haskell.org/platform)
> and follow the installation instructions

Usage
-----

```
$> ghci myfile.hs
$> ghc --make myfile.hs
$> cabal run/ghci mybin/mylib
```

--------------------------------------------------------------------------------

### demotime

Documentation
-------------

- [hackage (haskell's CPAN)](http://hackage.haskell.org)
- [Prelude (standard library)](http://hackage.haskell.org/package/base)
- [hoogle](https://www.haskell.org/hoogle/?hoogle=%5Ba%5D+-%3E+a)


Buzzwords
---------

- functional
- pure
- lazy/non-strict
- strongly typed
- statically typed

Syntax
======

Commons
-------

```haskell
    if … then … else
    let a = 4 -- in ghci/local in functions
    [1..3] -- list ranges
```

Uncommons
---------

Function application by 'space'

```
max(1,2)
```

vs.

```
max 1 2
```

--------------------------------------------------------------------------------

Purity (except in ghci)

### `AssignmentError.hs`

```haskell
a :: Int
a = 3

a :: Int
a = 4
```

Missing something?
------------------

### No loop constructs! Instead recursion

```haskell
basicRecursion = do putStrLn "Guess my word:"
                    word <- getLine
                    if word == "up" then putStrLn "Word up!"
                                    else basicRecursion
```

Other peculiarities
-------------------

- binding via `=` and `<-`
- `guards`
- `case-statements`
- `do-blocks`
- `types`
- `type signatures`
- function names start lower case
- type names start with a capital letter

Questions
---------

If there are any questions please do ask a.s.a.p. if you didn't understand
something

1. maybe someone else has the same problem
2. if you need time to think about it - you won't be able follow the talk

Types
=====

Notation
--------

`a :: Int`

Means "the variable `a` is of type `Int`" (in older versions of ghc `Int` =
`Int32` on 32-bit machines and `Int64` on 64-bit machines, to my knowledge
nowadays it has been fixed to be `Int64`.

Strict and static
-----------------

Types are strict in the sense that one needs to explicitly convert types. There
is no such thing as auto-converting, the compiler defaults to certain types if
no type signature is given, but if it is - the type is enforced.

--------------------------------------------------------------------------------

### Demo-time

Built in
--------

- `Int`, `Integer`
- `Float` `Double`
- `Char`
- `Bool`
- `()` … unit (similar to void in C-like languages)

Type variable
-------------

Expressions starting with a lower case letter in type signatures act as
a type variable that gets bound to a specific type as soon as the type
checker/inferencer can figure it out.

- `a`
- `a0`
- `mightbesomethingreallylongaswell`

Containers
----------

- Lists `[a]` are always homogenous
- Tuples `(a,b)`, `(a,b,c)` … (up to 15 entries or so)
- Maybes `Maybe a`
- Sequences `Seq a`
- Set `Set a`
- Trees `Tree a`
- …

Containers can have more than one type variable
-----------------------------------------------

Associative arrays `Map k v` (with the restriction that `k` has an order) have two type
parameters, and come in the flavours

- lazy
- strict

Which always means lazy/strict in the key; the value is always lazy.

--------------------------------------------------------------------------------

Also there is `Either a b = Left a | Right b`

Uncommon types
--------------

- `Random a` … random things
- `IO a` … things coming from the outside world a.k.a. user/network/disk input

Functions
=========

Functions
---------

Functions have types too - the signature is a 'bit' uncommon - so let's get used
to it.

- `head :: [a] -> a`

This means `head` is a function that takes a list of any type and returns a
single element of the same type.

--------------------------------------------------------------------------------

So what about functions that take more than one argument.

--------------------------------------------------------------------------------


- `timesN :: Char -> Int -> String`

Would translate to `C`-like syntax to something

- `char[] timesN (char c, int n)`

The "translation scheme" is every parameter but the last is an input parameter,
and the last one is the output parameter. But more about this in the 3rd part.

A few more functions
--------------------

Let us take a brief look at the [base system](http://hackage.haskell.org/package/base).


Building your own types
=======================

Data-Declarations
-----------------

With the use of the keyword `data` we can define our own (algebraic) data types,
or ADTs for short.

--------------------------------------------------------------------------------

```haskell

data TypeWithoutParams = Type1 | Type2 | Type3
data TypeWithParams = TypeW1 Int Int | TypeW2 String
data Container a b c = C1 (a,c) [b] | C2 b (Container a b c)
data Container a b c = C1 (a,c) [b] | C2 b String
data Recursife a = Rec1 a | Rec2 a [Recursife a]

```

Explanation
-----------

- left hand side
- right hand side
- Capital initials
- type-variables
- when used on rhs => have to be on lhs
- but not vice versa

Real World Examples
-------------------

```haskell

data Bool = True | False

data JSValue = JSNull
             | JSBool     !Bool
             | JSRational Bool !Rational
             | JSString   JSString
             | JSArray    [JSValue]
             | JSObject   (JSObject JSValue)

data Tree a = Empty | Node a [Tree a]
```

--------------------------------------------------------------------------------

demo-time

Type-Aliases
------------

The keyword `type` makes it possible to create duplicates to existing types for
convenience reasons.

```haskell
type String = [Char]
type Degree = Double
```

Newtype
-------

The keyword `newtype` makes a type alias with exactly one constructor which has
the special properties of hiding all properties of the inner type.

```haskell
newtype Age = MkAge Int
newtype Temperature = T {celsius :: Double}
```

Deriving
--------

After a `data` or a `newtype` declaration one can put the keyword `deriving` to
automatically generate properties with functions.

`data Fish = Shark | Trout deriving (Eq,Ord,Enum,Show,Read)`

More about this next time






