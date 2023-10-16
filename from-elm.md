# PureScript from Elm

Based on information picked from:
- https://github.com/purescript/documentation
- https://book.purescript.org
- https://github.com/alpacaaa/elm-to-purescript-cheatsheet
- https://learnxinyminutes.com/docs/purescript/
- https://medium.com/@terezk_a/haskell-in-elm-terms-type-classes-415f1612b335
- https://gist.github.com/justinwoo/0118be3e4a0d7394a99debbde2515f9b
- https://www.tobyhobson.com/posts/cats/
- https://jordanmartinez.github.io/purescript-jordans-reference-site/
- https://github.com/alpacaaa/zero-bs-haskell

## Common packages

| **Elm**                                  | **Purescript**                           | **Notes**                                |
| ---------------------------------------- | ---------------------------------------- | ---------------------------------------- |
| [String](http://package.elm-lang.org/packages/elm-lang/core/latest/String) | [Data.String](https://pursuit.purescript.org/packages/purescript-strings) |                                          |
| [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) | [Data.Maybe](https://pursuit.purescript.org/packages/purescript-maybe) |                                          |
| [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) | [Data.Either](https://pursuit.purescript.org/packages/purescript-either) | `Err` is `Left` and `Ok` is `Right`      |
| [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array) | [Data.Array](https://pursuit.purescript.org/packages/purescript-arrays) | `[]` is the empty array |
| [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List) | [Data.List](https://pursuit.purescript.org/packages/purescript-lists) | `Nil` is the empty list|
| [Tuple](http://package.elm-lang.org/packages/elm-lang/core/latest/Tuple) | [Data.Tuple](https://pursuit.purescript.org/packages/purescript-tuples) |                                    |
| [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) | [Data.Map](https://pursuit.purescript.org/packages/purescript-maps) |                                          |
| [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set) | [Data.Set](https://pursuit.purescript.org/packages/purescript-sets) |                                          |
| () | [Data.Unit](https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Data.Unit) | `()` is the empty [`Row` type](https://github.com/purescript/documentation/blob/master/language/Types.md#rows) in PureScript |
| [Debug](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug) | [Debug.Trace](https://pursuit.purescript.org/packages/purescript-debug) | `Debug.spy` is the closest thing to `Debug.log` |

## Common Functions

| **Elm**                                  | **Purescript**                           | **Notes** |
| ---------------------------------------- | ---------------------------------------- | --------- |
| [identity](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#identity) | [id](https://pursuit.purescript.org/packages/purescript-prelude) |
| [always](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#always) | [const](https://pursuit.purescript.org/packages/purescript-prelude) |
| [toString](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#toString) | [show](https://pursuit.purescript.org/packages/purescript-prelude) |
| `>>`                                     | `>>>`                                    |           |
| `<<`                                     | `<<<`                                    |           |
| \|>                                      | `#`                                      |           |
| <\|                                      | `$`                                      |           |
| ++                                       | <>                                       | `String` concatenation |

## Type signatures

Type signatures are separated with double colons.

```purs
sum :: Int -> Int -> Int
```

Polymorphic functions in PureScript require an explicit `forall` to declare type variables before using them.

```purs
map :: forall a b. (a -> b) -> Maybe a -> Maybe b
```

## Type holes

```purs
runApp :: Foo -> Bar Baz String Int Unit -> ?x
```

If you’re not sure of a type in a type signature, you can write a type "hole" consisting of a question mark followed by a lowercase name. The compiler will generate an error and tell you what type it inferred. Note that to use type holes there must be no other compiler errors.

You can use type holes everywhere:
```purs
foo :: Int
foo = 1 + ?what_could_this_be
```

## Arrays

<!-- TODO make this just like the Lists section -->

```purs
myArray = [2,4,3]

-- Cons (prepend)
myNewArray =  1 : [2,4,3] -- [1,2,4,3]

head [1,2,3,4] -- (Just 1)
tail [1,2,3,4] -- (Just [2,3,4])
init [1,2,3,4] -- (Just [1,2,3])
last [1,2,3,4] -- (Just 4)

-- Array access by index starting at 0
[3,4,5,6,7] !! 2 -- (Just 5)

-- Range
1..5 -- [1,2,3,4,5]

length [2,2,2] -- 3
drop 3 [1,2,3,4,5] -- [4,5]
take 3 [1,2,3,4,5] -- [1,2,3]
append [1,2,3] [4,5,6] -- [1,2,3,4,5,6]
```

### Destructuring

```purs
case xs of
  [] -> ... -- empty array
  x : rest -> ... -- head and tail
```

## Lists

**Be careful!** `[1,2,3]` is syntactic sugar for `List Int` in Elm but `Array Int` in Purescript.

In PureScript, the quickest way to create a list is from a Foldable structure (an Array in this case):

```purs
myList = List.fromFoldable [2,4,3]

-- Cons (prepend)
myNewList = 1 : myList
```

### Destructuring

```purs
case xs of
  Nil -> ... -- empty list
  x : rest -> ... -- head and tail
```

## Non empty arrays/lists

The they are `NonEmpty` modules for [Array](https://pursuit.purescript.org/packages/purescript-arrays/4.4.0/docs/Data.Array.NonEmpty) and [List](https://pursuit.purescript.org/packages/purescript-lists/4.0.1/docs/Data.List.NonEmpty). They are quite useful to flatten cases as described in the famous ["Parse, don’t validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) blog post.

```purs
data NonEmpty a = a :| [a]

-- no Maybe when getting the head
getHead :: NonEmpty a -> a
getHead (x:|_) = x
```

## Tuples

Just a data type in Purescript. Use records when possible.

```purs
import Data.Tuple

coords = Tuple 10 20
```

### Destructuring

```purs
Tuple x y = coords
```

## Records

```purs
type Person = { name :: String, age :: Int }

myPerson :: Person
myPerson = { name: "Bob", age: 30 }

edited :: Person
edited = { myPerson | age = 31 }
```

### Property Accessors

```purs
_.age myPerson -- 30
_.address.street myPerson -- "Main Street"

```

### Destructuring

```purs
personName :: Person -> String
personName { name } = name

bumpAge :: Person -> Person
bumpAge p@{ age } =
	p { age = age + 1 }
```

### Row Polymorphism

Row Polymorphism is the equivalent of the extensible records concept in Elm.

```elm
-- Elm
getAge : { a | age : Int } -> Int
getAge { age } = age
```

```purs
-- PureScript
getAge :: forall r. { age :: Int | r } -> Int
getAge { age } = age
```
In the above example, the type variable `r` has kind [`Row Type`](https://github.com/purescript/documentation/blob/master/language/Types.md#rows) (an unordered collection of named types, with duplicates).


## `where` clause

The `where` clause is "syntactic sugar" for let bindings. Functions defined below the `where` keyword can be used in the function scope and in the `where` scope.

```purs
foo :: String -> String -> String
foo arg1 arg2 =
  bar arg1 arg2 "Welcome to PureScript!"

  where
    bar :: String -> String -> String -> String
    bar s1 s2 s3 =
      (baz s1) <> (baz s2) <> s3

    baz :: String -> String
    baz s = "Hi " <> s <> "! "
```

## Underscore in lambdas

The underscore symbol `_` can be used in functions when you don't want to give the parameter a name.

`(_ + 10)` is the same as `(\n -> n + 10)`

## Guards

When using patterns to define a function at the top level, guards appear after all patterns:

```purs
greater x y
  | x > y = true
  otherwise = false
```

Exhaustibility of patterns is checked by the compiler. To be considered exhaustive, guards must clearly include a case that is always true. `otherwise` is a synonym for `true` and is commonly used in guards.

Guards may also be used within case expressions, which allow for inline expressions. For example, these are equivalent:

```purs
fb :: Int -> Effect Unit
fb = log <<< case _ of
  n
    | 0 == mod n 15 -> "FizzBuzz"
    | 0 == mod n 3 -> "Fizz"
    | 0 == mod n 5 -> "Buzz"
    | otherwise -> show n
```

```purs
fb :: Int -> Effect Unit
fb n = log x
  where
  x
    | 0 == mod n 15 = "FizzBuzz"
    | 0 == mod n 3 = "Fizz"
    | 0 == mod n 5 = "Buzz"
    | otherwise = show n
```

## Data types

Instead of Elm’s `type`, PureScript uses `data`.
Instead of Elm’s `type alias`, PureScript uses `type`.

```elm
-- Elm
type Direction  = Up | Down
type alias Time = Int
```

```purs
-- PureScript
data Direction  = Up | Down
type Time       = Int
```

## Newtypes

Newtypes are like data types (which are introduced with the `data` keyword), but are restricted to a single constructor which contains a single argument.

```purs
newtype Password = Password String
```

Newtypes are especially useful when dealing with raw data as you can write a "validation" function (*smart constructor*) and not expose the type constructor itself in exports.
This is known as the [opaque type](https://sporto.github.io/elm-patterns/advanced/opaque-types.html) pattern in Elm.

```purs
module Password
  ( Password -- not Password(..) to prevent exposing the constructor Password
  , fromString
  ) where

newtype Password = Password String

fromString :: String -> Either Error Password
fromString =
  if length password >= 6 then
    Right password
  else
    Left "Size should be at least 6"

myPassword = fromString "123456"

```

There are many useful methods available for working with newtypes using the [Newtype](https://pursuit.purescript.org/packages/purescript-newtype/) package.

## Type classes

<!-- TODO make it work in PureScript (not Haskell Number) for Sandwich type  -->

```purs
class Number a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  (/) :: a -> a -> a
  abs :: a -> a
  negate :: a -> a
```

It tells the compiler that any instance of the Number type class must have these functions. Once you have defined your type class, you can add any type to this class, as long as you define the required functions, too.

```purs
type Sandwich =
  { ingredients :: List String
  , price :: Int
  }
instance Number Sandwich where
  (+) :: Sandwich -> Sandwich -> Sandwich
  (+) a b =
    { ingredients: concat a.ingredients b.ingredients, price: a.price + b.price }
  (-) :: Sandwich -> Sandwich -> Sandwich
  (-) a b =
    -- ... more silly implementations
```

With Sandwich defined as an instance of Number we can now use the arithmetic operators on people.

```purs
example :: Sandwich
example =
  Sandwich "Tereza" 23 + Sandwich "Evan" 29 -- Sandwich "Terevan" 52
```

Instead of defining a different `map` for each type (Maybe, Result etc.) like in Elm, PureScript uses type classes.

For instance, `map` is defined once for all with the [`Functor`](https://pursuit.purescript.org/packages/purescript-prelude/3.0.0/docs/Data.Functor) type class. A `Functor` is a type constructor which supports a mapping operation `map`.

```purs
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

## Constraints

You can constraint the usage of a function so that a type parameter must satisfy certain conditions. Constraints are declared on the left of the fat arrow (`=>`).

Given our Number type class in Haskell, one could add a sum function like so:

```purs
sum :: (Number a) => List a -> a
sum as =
  foldl (+) 0 as
```

A sum function does not make sense unless its arguments can be treated like numbers. Thus, by adding `(Number a) =>` in the beginning of the signature, we constrain all mentions of a in the rest of the signature to be instances of the Number type class. This is important because we need to use the (+) operator in the definition, which is only accessible to instances of the Number type class!

## Applicatives

`<*>` is the infix alias of the *apply* operator defined in the [Applicative](https://pursuit.purescript.org/packages/purescript-prelude/3.0.0/docs/Control.Applicative) type class, and is equivalent to [`|> andMap`](https://thoughtbot.com/blog/running-out-of-maps#one-liner-to-rule-them-all) in Elm.

```purs
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Applicative lets us perform N operations independently, then it aggregates the results for us.
You are in an applicative context when using [`Decoder`](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#map2) in Elm.

### Map alias `<$>`

Often used with `<*>`, the `<$>` operator is alias for `map` but in infix position, meaning that the two are equivalent:

```purs
map (\n -> n + 1) (Just 5)
(\n -> n + 1) <$> (Just 5)
```

## Monads

`>>=` is the infix alias of the *bind* operator defined in the [Monad](https://pursuit.purescript.org/packages/purescript-prelude/3.0.0/docs/Control.Monad) type class, and is is equivalent to [`|> andThen`](https://package.elm-lang.org/packages/elm/core/latest/Maybe#andThen) in Elm.
```purs
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

 The signature is the same as that of Elm’s `Maybe.andThen`, except the arguments are flipped. They are flipped in PureScript because their version is an infix operator:

```elm
-- Elm
example : Maybe Int
example =
  Just "23" |> Maybe.andThen String.toInt
```

```purs
-- PureScript
example :: Maybe Int
example =
  Just "23" >>= readMaybe
```

Monadic operations operate sequentially not concurrently. That’s great when we have a dependency between the operations e.g. lookup user_id based on email then fetch the inbox based on the user_id. But for independent operations monadic calls are very inefficient as they are inherently sequential. Monads fail fast which makes them poor for form validation and similar use cases. Once something “fails” the operation aborts.
You are in a monadic context when using [`Task`](https://package.elm-lang.org/packages/elm/core/latest/Task#andThen) in Elm.

## `do` notation
`do` Notation is "syntactic sugar" for chained `>>=`. It removes the need for indentations.

```purs
foo :: Box Unit
foo =
  -- only call `(\x -> ...)` if `getMyInt` actually produces something
  getMyInt >>= (\x ->
      let y = x + 4
      in toString y >>= (\z ->
        print z
      )
    )
```

is the same as

```purs
foo :: Box Unit
foo = do
  x <- getMyInt
  let y = x + 4 -- no need to have a corresponding `in` statement
  z <- toString y
  print z -- last line must not end with `value <- computation` but just `computation`
```

## Type Class Deriving

The compiler can derive type class instances to spare you the tedium of writing boilerplate. There are a few ways to do this depending on the specific type and class being derived.

### Classes with built-in compiler support

Some classes have special built-in compiler support, and their instances can be derived from all types.

For example, if you you'd like to be able to remove duplicates from an array of an ADT using `nub`, you need an `Eq` and `Ord` instance. Rather than writing these manually, let the compiler do the work.

```purs
import Data.Array (nub)

data MyADT
  = Some
  | Arbitrary Int
  | Contents Number String

derive instance eqMyADT :: Eq MyADT
derive instance ordMyADT :: Ord MyADT

nub [Some, Arbitrary 1, Some, Some] == [Some, Arbitrary 1]
```

Currently, instances for the following classes can be derived by the compiler:
- Data.Generic.Rep (class Generic) [see below](../guides/Type-Class-Deriving.md#deriving-from-generic)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Ord#t:Ord)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor#t:Functor)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype)

### Derive from `newtype`

If you would like your newtype to defer to the instance that the underlying type uses for a given class, then you can use newtype deriving via the `derive newtype` keywords.

For example, let's say you want to add two `Score` values using the `Semiring` instance of the wrapped `Int`.

```purs
newtype Score = Score Int

derive newtype instance semiringScore :: Semiring Score

tenPoints :: Score
tenPoints = (Score 4) + (Score 6)
```

That `derive` line replaced all this code:

```purs
-- No need to write this
instance semiringScore :: Semiring Score where
  zero = Score 0
  add (Score a) (Score b) = Score (a + b)
  mul (Score a) (Score b) = Score (a * b)
  one = Score 1
```

Note that we can use either of these options to derive an `Eq` instance for a `newtype`, since `Eq` has built-in compiler support. They are equivalent in this case.

```purs
derive instance eqScore :: Eq Score
derive newtype instance eqScore :: Eq Score
```

### Deriving from `Generic`

See the [official guide](https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#deriving-from-generic).

## The Warn type class

There is a type class in `Prim` called `Warn`. When the compiler solves a `Warn` constraint it will trivially solve the instance and print out the message as a user defined warning.

```purescript
meaningOfLife :: Warn (Text "`meaningOfLife` result is hardcoded, for now.) => Int
meaningOfLife = 42
```

## Foreign Function Interface (FFI)

### Calling PureScript from JavaScript

```purs
-- Purescript
module Tools where

import Prelude

-- find the greatest common divisor
gcd :: Int -> Int -> Int
gcd n m | n == 0 = m
gcd n m | m == 0 = n
gcd n m | n > m = gcd (n - m) m
gcd n m = gcd (m - n) n
```

PureScript functions always get turned into Javascript functions of a single argument, so we need to apply its arguments one-by-one:

```js
// JavaScript

import { gcd } from 'Tools'

gcd(15)(20)
```

### Calling Javascript from PureScript

#### Foreign Modules

A foreign module is just an ES module which is associated with a PureScript module.

- All the ES module exports must be of the form `export const name = value` or `export function name() { ... }`.
- The PureScript module must have the same as the ES one but with the `.purs` extension. It contains the signatures of the exports.

##### Unary functions

```js
// JavaScript (src/Interest.js)

export function calculateInterest(amount) {
  return amount * 0.1
}
```

```purs
-- PureScript (src/Interest.purs)

module Interest where

foreign import calculateInterest :: Number -> Number
```

##### Functions of Multiple Arguments

PureScript functions are curried by default, so Javascript functions of multiple arguments require special treatment.

```js
// JavaScript

export function calculateInterest(amount, months) {
  return amount * Math.exp(0.1, months)
}
```

```purs
-- PureScript

module Interest where

-- available for function arities from 0 to 10
import Data.Function.Uncurried (Fn2)

foreign import calculateInterest :: Fn2 Number Number Number
```

We can write a curried wrapper function in PureScript which will allow partial application:

```purs
calculateInterestCurried :: Number -> Number -> Number
calculateInterestCurried = runFn2 calculateInterest
```

An alternative is to use curried functions in the native module, using multiple nested functions, each with a single argument:

```js
// JavaScript

export function calculateInterest(amount) {
  return function(months) {
    return amount * Math.exp(0.1, months)
  }
}
```

This time, we can assign the curried function type directly:

```purs
-- PureScript

foreign import calculateInterest :: Number -> Number -> Number
```

## Sanitizing Foreign Data

It is important to sanitize data when working with values returned from Javascript functions using the FFI. For this we will use  [`purescript-foreign-generic`](https://pursuit.purescript.org/packages/purescript-foreign-generic).


```purs
import Data.Foreign
import Data.Foreign.Generic
import Data.Foreign.JSON
```

`purescript-foreign-generic` has the following functions:

```purs
parseJSON :: String -> F Foreign
decodeJSON :: forall a. Decode a => String -> F a
```

`F` is a type alias:

```purs
type F = Except (NonEmptyList ForeignError)
```

`Except` is an monad for handling exceptions in pure code, much like `Either`. We can convert a value in the `F` monad into a value in the `Either` monad by using the `runExcept` function.

```purs
import Control.Monad.Except

runExcept (decodeJSON "\"Testing\"" :: F String)
-- Right "Testing"

runExcept (decodeJSON "true" :: F Boolean)
-- Right true

runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
-- Right [1, 2, 3]

runExcept (decodeJSON "[1, 2, true]" :: F (Array Int))
-- Left (NonEmptyList (NonEmpty (ErrorAtIndex 2 (TypeMismatch "Int" "Boolean")) Nil))
```

Real-world JSON documents contain null and undefined values, so we need to be able to handle those too.

`purescript-foreign-generic` defines a type constructors which solves this problem: `NullOrUndefined`. It uses the `Maybe` type constructor internally to represent missing values.

The module also provides a function `unNullOrUndefined` to unwrap the inner value. We can lift the appropriate function over the `decodeJSON` action to parse JSON documents which permit null values:

```purs
import Data.Foreign.NullOrUndefined

runExcept (unNullOrUndefined <$> decodeJSON "42" :: F (NullOrUndefined Int))
-- Right (Just 42)

runExcept (unNullOrUndefined <$> decodeJSON "null" :: F (NullOrUndefined Int))
-- Right Nothing
```

To parse arrays of integers where each element might be null, we can lift the function `map unNullOrUndefined` over the `decodeJSON` action:

```purs
runExcept (map unNullOrUndefined <$> decodeJSON "[1, 2, null]" :: F (Array (NullOrUndefined Int)))
-- Right [(Just 1),(Just 2),Nothing]
```

## Front-end frameworks

[Halogen](https://purescript-halogen.github.io/purescript-halogen/) is the most used front-end framework for PureScript but we will use [Flame](https://flame.asafe.dev/) because it is inspired by the Elm architecture with focus on simplicity and performance.

- Message based state updating – see [Handling events](https://flame.asafe.dev/events)
- Subscriptions – see [Handling external events](https://flame.asafe.dev/events#subscriptions)
- Server side rendering – see [Rendering the app](https://flame.asafe.dev/rendering)
- Performance comparable to native JavaScript frameworks – see [benchmarks](https://flame.asafe.dev/benchmarks)
- Parse HTML into Flame markup with [breeze](https://github.com/easafe/haskell-breeze)
