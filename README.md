# PureScript for Elm developers 🤯

This README file is a crash course on [PureScript](https://www.purescript.org/) targeted at [Elm](https://elm-lang.org/) developers. It is
based on information picked from:

- https://github.com/alpacaaa/elm-to-purescript-cheatsheet
- https://github.com/purescript/documentation
- https://book.purescript.org
- https://jordanmartinez.github.io/purescript-jordans-reference-site/
- https://learnxinyminutes.com/docs/purescript/
- https://gist.github.com/justinwoo/0118be3e4a0d7394a99debbde2515f9b
- https://www.tobyhobson.com/posts/cats/
- https://github.com/alpacaaa/zero-bs-haskell

Sometimes I did a shameless copy-paste instead of writing a bad paraphrase. I think it is fair use but please let me know if I am infringing any copyright. Feel free to open an issue if you find any mistakes.

Happy monad lifting! 🏋

Laurent

<hr>

## Common packages

[Pursuit](https://pursuit.purescript.org/) is the home of PureScript packages documentation. It lets you search by package, module, and function names, as well as approximate type signatures.

| **Elm**                                  | **Purescript**                           | **Notes**                                |
| ---------------------------------------- | ---------------------------------------- | ---------------------------------------- |
| [String](http://package.elm-lang.org/packages/elm-lang/core/latest/String) | [Data.String](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String) |                                          |
| [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) | [Data.Maybe](https://pursuit.purescript.org/packages/purescript-maybe/docs/Data.Maybe) |                                          |
| [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) | [Data.Either](https://pursuit.purescript.org/packages/purescript-either/docs/Data.Either) | `Err` is `Left` and `Ok` is `Right`      |
| [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array) | [Data.Array](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array) | `[]` is the empty array |
| [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List) | [Data.List](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List) | `Nil` is the empty list|
| [Tuple](http://package.elm-lang.org/packages/elm-lang/core/latest/Tuple) | [Data.Tuple](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple) |                                    |
| [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) | [Data.Map](https://pursuit.purescript.org/packages/purescript-maps/docs/Data.Map) |                                          |
| [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set) | [Data.Set](https://pursuit.purescript.org/packages/purescript-sets/docs/Data.Set) |                                          |
| () | [Data.Unit](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Unit) | `()` is the empty [`Row` type](https://github.com/purescript/documentation/blob/master/language/Types.md#rows) in PureScript |
| [Never](https://package.elm-lang.org/packages/elm/core/latest/Basics#Never) | [Data.Void](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Void) | |
| [Debug](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug) | [Debug.Trace](https://pursuit.purescript.org/packages/purescript-debug) | `Debug.spy` is the closest thing to `Debug.log` |

## Common Functions

| **Elm**                                  | **Purescript**                           | **Notes** |
| ---------------------------------------- | ---------------------------------------- | --------- |
| `()` | [unit](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Unit#v:unit) |
| [identity](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#identity) | [identity](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Function#v:identity) |
| [always](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#always) | [const](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Function#v:const) |
| [never](https://package.elm-lang.org/packages/elm/core/latest/Basics#never) | [absurd](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Void#v:absurd) |
| [toString](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#toString) | [show](https://pursuit.purescript.org/packages/purescript-prelude) |
| `>>`                                     | `>>>`                                    |           |
| `<<`                                     | `<<<`                                    |           |
| `\|>`                                    | `#`                                      |           |
| `<\|`                                    | `$`                                      |           |
| `++`                                     | `<>`                                     | [Semigroup](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Semigroup#t:Semigroup) concatenation (`String`, `Array`, `List`, `Tuple`…)|

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

In PureScript, arrays are the most common data structure for sequences of items. They are constructed with square brackets.

```purs
import Data.Array ()

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

You can use pattern matching for arrays of a *fixed* length:

```purs
isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0
```

For performance reasons, PureScript does *not* provide a direct way of destructuring arrays of an *unspecified* length. If you need a data structure which supports this sort of matching, the recommended approach is to use lists.

Another way is to use `uncons` or `unsnoc` to break an array into its first or last element and remaining elements:

```purs
import Data.Array (uncons, unsnoc)

uncons [1, 2, 3] -- Just {head: 1, tail: [2, 3]}
uncons [] -- Nothing

unsnoc [1, 2, 3] -- Just {init: [1, 2], last: 3}
unsnoc [] -- Nothing

case uncons myArray of
  Just { head: x, tail: xs } -> somethingWithXandXs
  Nothing -> somethingElse
```

Beware `unsnoc` is O(n) where n is the length of the array.

## Lists

**Be careful!**  The literal `[1,2,3]` has a type of `List Int` in Elm but `Array Int` in Purescript.

PureScript lists are [linked lists](https://en.wikipedia.org/wiki/Linked_list). You can create them using the `Cons` infix alias `:` and `Nil` when there is no link to the next element (end of the list).

```purs
myList = 1 : 2 : 3 : Nil

myNewList = 1 : myList
```

Another way to create a list is from a Foldable structure (an Array in this case):

```purs
myList = List.fromFoldable [2,4,3]
```

### Destructuring

```purs
case xs of
  Nil -> ... -- empty list
  x : rest -> ... -- head and tail
```

## Foldables

[Data.Foldable](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Foldable) contains common functions (`sum`, `product`, `minimum`, `maximum` etc.) for data structures which can be folded, such as `Array` and `List`.

## Non empty arrays/lists

There is a [Data.NotEmpty](https://pursuit.purescript.org/packages/purescript-nonempty/7.0.0/docs/Data.NonEmpty) module that defines a generic `NonEmpty` data structure. `:|` is the infix alias for its constructor.

This quite useful to flatten cases as described in the famous ["Parse, don’t validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) blog post:

```purs
import Data.NonEmpty (NonEmpty, (:|))

-- no Maybe when getting the head
arrayHead :: NonEmpty Array a -> a
arrayHead (x :| _) = x
```

Instead the generic `Data.NonEmpty` module, use specific modules when possible:
- [Data.Array.NonEmpty](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array.NonEmpty) to create `NonEmptyArray`
- [Data.List.NonEmpty](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.NonEmpty) to create `NonEmptyList`

For convenience, [Data.Array.NonEmpty.Internal](https://pursuit.purescript.org/packages/purescript-arrays/7.3.0/docs/Data.Array.NonEmpty.Internal#t:NonEmptyArray) provides the internal constructor  [`NonEmptyArray`](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array.NonEmpty.Internal#t:NonEmptyArray). Beware you can create a `NonEmptyArray` that is actually empty with it so use this at your own risk when you know what you are doing.

## Tuples

Tuples are just a data type in Purescript. Use records when possible.

```purs
import Data.Tuple (Tuple(..), fst, snd)

coords2D :: Tuple Int Int
coords2D = Tuple 10 20

getX :: Tuple Int Int -> Int
getX coords = fst coords

getY :: Tuple Int Int -> Int
getY coords = snd coords
```

You can use tuples that are not restricted to two elements with [Data.Tuple.Nested](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple.Nested). All nested tuple functions are numbered from 1 to 10:

```purs
import Data.Tuple.Nested (Tuple3, tuple3, get2)

coords3D :: Tuple3 Int Int Int
coords3D = tuple3 10 20 30

getY :: Tuple3 Int Int Int -> Int
getY coords = get2 coords
```

`/\` is the infix alias for `Tuple` that allows nested tuples of arbitrary length (depth). The same alias exists for types. The previous example could be rewritten as:

```purs
import Data.Tuple.Nested (type (/\), (/\), get2)

coords3D :: Int /\ Int /\ Int
coords3D = 10 /\ 20 /\ 30

getY :: Int /\ Int /\ Int -> Int
getY coords = get2 coords
```

### Destructuring

```purs
distance2D :: Tuple Int Int -> Int
distance2D (Tuple x y) =
  x * x + y * y

distance3D :: Int /\ Int /\ Int -> Int
distance3D (x /\ y /\ z) =
  x * x + y * y + z * z
```

## Records

```purs
type Person =
  { name :: String
  , age :: Int
  }

myPerson :: Person
myPerson = { name: "Bob", age: 30 }

edited :: Person
edited = myPerson { age = 31 }

toPerson :: String -> Int -> Person
toPerson name age =
  { name: name, age: age }

toPerson2 :: String -> Int -> Person
toPerson2 name age =
  { name, age }

toPerson3 :: String -> Int -> Person
toPerson3 =
  { name: _, age: _ } -- equivalent to `\name age -> { name, age }` (types inferred by the signature)
```

### Property accessors

In PureScript `(_ + 5)` is the same as `(\n -> n + 5)`, so `(_.prop)` is the same as `(\r -> r.prop)`.

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
### Pattern matching

```purs
ecoTitle {author: "Umberto Eco", title: t} = Just t
ecoTitle _ = Nothing

ecoTitle {title: "Foucault's pendulum", author: "Umberto Eco"} -- (Just "Foucault's pendulum")
ecoTitle {title: "The Quantum Thief", author: "Hannu Rajaniemi"} -- Nothing
-- ecoTitle requires both field to type check
ecoTitle {title: "The Quantum Thief"} -- Object lacks required property "author"
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

## Guards

When using patterns to define a function at the top level, guards appear after all patterns:

```purs
greater x y
  | x > y = true
  | otherwise = false
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

Instead of

```purs
fullName :: String -> String -> String
fullName firstName lastName =
  firstName <> " " <> lastName

fullName "Phillip" "Freeman" -- "Phillip Freeman"
fullName "Freeman" "Phillip" -- "Freeman Phillip" wrong order!
```

we could write more explicit types but that would not prevent arguments ordering errors:

```purs
type FirstName = String
type LastName = String
type FullName = String

fullName :: FirstName -> LastName -> FullName
fullName firstName lastName =
  firstName <> " " <> lastName

fullName "Phillip" "Freeman" -- "Phillip Freeman"
fullName "Freeman" "Phillip" -- "Freeman Phillip" still wrong order!
```

Instead can use single constructor data types and destructure them to ensure the right arguments are provided:
```purs
data FirstName = FirstName String
data LastName = LastName String
data FullName = FullName String

fullName :: FirstName -> LastName -> FullName
fullName (FirstName firstName) (LastName lastName) =
  firstName <> " " <> lastName

fullName (FirstName "Phillip") (LastName "Freeman") -- "Phillip Freeman"
fullName (LastName "Freeman") (FirstName "Phillip") -- compiler error!
```

For the compiler to optimize the output for this common pattern, it is even better to use the `newtype` keyword which is especially restricted to a single constructor which contains a single argument.

```purs
newtype FirstName = FirstName String
newtype LastName = LastName String
newtype FullName = FullName String
```

Newtypes are especially useful when dealing with raw data as you can write a "validation" function without exposing the type constructor itself in exports. This is known as the [*smart constructor*](https://github.com/JordanMartinez/purescript-jordans-reference/blob/latestRelease/31-Design-Patterns/01-Smart-Constructors.md) pattern:

```purs
module Password
  ( Password -- not Password(..) to prevent exposing the Password constructor
  , toPassword
  ) where

newtype Password = Password String

toPassword :: String -> Either String Password
toPassword str =
  if length str >= 6 then
    Right (Password str)
  else
    Left "Size should be at least 6"

myPassword = toPassword "123456"
```

## Modules

```purs
module Syntax.Module.FullExample
  -- exports go here by just writing the name
  ( value

  , function, (>@>>>) -- aliases must be wrapped in parenthesis

  -- when exporting type classes, there are two rules:
  -- - you must precede the type class name with the keyword 'class'
  -- - you must also export the type class' function (or face compilation errors)
  , class TypeClass, tcFunction

  -- when exporting modules, you must precede the module name with
  -- the keyword 'module'
  , module ExportedModule

  -- The type is exported, but no one can create a value of it
  -- outside of this module
  , ExportDataType1_ButNotItsConstructors

  -- syntax sugar for 'all constructors'
  -- Either all or none of a type's constructors must be exported
  , ExportDataType2_AndAllOfItsConstructors(..)

  -- Type aliases can also be exported
  , ExportedTypeAlias

  -- When type aliases are aliased using infix notation, one must export
  -- both the type alias, and the infix notation where 'type' must precede
  -- the infix notation
  , ExportedTypeAlias_InfixNotation, type (<|<>|>)

  -- Data constructor alias; exporting the alias requires you
  -- to also export the constructor it aliases
  , ExportedDataType3_InfixNotation(Infix_Constructor), (<||||>)

  , ExportedKind
  , ExportedKindValue
  ) where

-- imports go here

-- imports just the module
import Module

-- import a submodule
import Module.SubModule.SubSubModule

-- import values from a module
import ModuleValues (value1, value2)

-- imports functions from a module
import ModuleFunctions (function1, function2)

-- imports function alias from a module
import ModuleFunctionAliases ((/=), (===), (>>**>>))

-- imports type class from the module
import ModuleTypeClass (class TypeClass)

-- import a type but none of its constructors
import ModuleDataType (DataType)

-- import a type and one of its constructors
import ModuleDataType (DataType(Constructor1))

-- import a type and some of its constructors
import ModuleDataType (DataType(Constructor1, Constructor2))

-- import a type and all of its constructors
import ModuleDataType (DataType(..))

-- resolve name conflicts using "hiding" keyword
import ModuleNameClash1 (sameFunctionName1)
import ModuleNameClash2 hiding (sameFunctionName1)

-- resolve name conflicts using module aliases
import ModuleNameClash1 as M1
import ModuleNameClash2 as M2

-- Re-export modules
import Module1 (anInt1) as Exports
import Module2 (anInt2) as Exports
import Module3 (anInt3) as Exports
import Module4.SubModule1 (someFunction) as Exports

import ModuleKind (ImportedKind, ImportedKindValue) as Exports

import Prelude

import ExportedModule

-- To prevent warnings from being emitted during compilation
-- the above imports have to either be used here or
-- re-exported (explained later in this folder).

value :: Int
value = 3

function :: String -> String
function x = x

infix 4 function as >@>>>

class TypeClass a where
  tcFunction :: a -> a -> a

-- now 'sameFunctionName1' refers to ModuleF1's function, not ModuleF2's function
myFunction1 :: Int -> Int
myFunction1 a = sameFunctionName1 a

myFunction2 :: Int -> Int
myFunction2 a = M1.sameFunctionName1 (M2.sameFunctionName1 a)

dataDifferences :: M1.SameDataName -> M2.SameDataName -> String
dataDifferences M1.Constructor M2.Constructor = "code works despite name clash"

data ExportDataType1_ButNotItsConstructors = Constructor1A

data ExportDataType2_AndAllOfItsConstructors
  = Constructor2A
  | Constructor2B
  | Constructor2C

type ExportedTypeAlias = Int

data ExportedDataType3_InfixNotation = Infix_Constructor Int Int

infixr 4 Infix_Constructor as <||||>

type ExportedTypeAlias_InfixNotation = String

infixr 4 type ExportedTypeAlias_InfixNotation as <|<>|>

data ExportedKind

foreign import data ExportedKindValue :: ExportedKind
```

## Type classes

The `show` function takes a value and displays it as a string. `show` is defined by a type class in the Prelude module called Show, which is defined as follows:

```purs
class Show a where
  show :: a -> String
```

This code declares a new type class called `Show`, which is parameterized by the type variable `a`.

A type class instance contains implementations of the functions defined in a type class, specialized to a particular type.
You can add any type to a class, as long as you define the required functions.

For example, here is the definition of the `Show` type class instance for `Boolean` values, taken from the Prelude.  We say that the `Boolean` type *belongs* to the `Show` *type class*.

```purs
instance Show Boolean where
  show true = "true"
  show false = "false"
```

Instead of defining a different `map` for each type (Maybe, Result etc.) like in Elm, PureScript uses type classes.

For instance, `map` is defined once for all with the [`Functor`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor) type class. A `Functor` is a type constructor which supports a mapping operation `map`.

```purs
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

## Type class deriving

The compiler can derive type class instances to spare you the tedium of writing boilerplate. There are a few ways to do this depending on the specific type and class being derived.

Since PureScript version 0.15.0, giving class instances a name (for generated code readability) is optional. It it generated by the compiler if missing.

### Classes with built-in compiler support

Some classes have special built-in support (such as `Eq`), and their instances can be derived from all types.

For example, if you you'd like to be able to remove duplicates from an array of an ADT using `nub`, you need an `Eq` and `Ord` instance. Rather than writing these manually, let the compiler do the work.

```purs
import Data.Array (nub)

data MyADT
  = Some
  | Arbitrary Int
  | Contents Number String

derive instance Eq MyADT
derive instance Ord MyADT

nub [Some, Arbitrary 1, Some, Some] == [Some, Arbitrary 1]
```

Currently (in PureScript version 0.15.12), instances for the following classes can be derived by the compiler:
- Data.Generic.Rep (class Generic) [see below](#deriving-from-generic)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Ord#t:Ord)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor#t:Functor)
- [Data.Functor.Contravariant (class Contravariant)](https://pursuit.purescript.org/packages/purescript-contravariant/docs/Data.Functor.Contravariant#t:Contravariant)
- [Data.Bifunctor (class Bifunctor)](https://pursuit.purescript.org/packages/purescript-bifunctors/docs/Data.Bifunctor#t:Bifunctor)
- [Data.Bifoldable (class Bifoldable)](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Bifoldable#t:Bifoldable)
- [Data.Bitraversable (class Bitraversable)](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Bitraversable#t:Bitraversable)
- [Data.Profunctor (class Profunctor)](https://pursuit.purescript.org/packages/purescript-profunctor/docs/Data.Profunctor#t:Profunctor)

### Derive from `newtype`

If you would like your newtype to defer to the instance that the underlying type uses for a given class, then you can use newtype deriving via the `derive newtype` keywords.

For example, let's say you want to add two `Score` values using the `Semiring` instance of the wrapped `Int`.

```purs
newtype Score = Score Int

derive newtype instance Semiring Score

tenPoints :: Score
tenPoints = (Score 4) + (Score 6)
```

That `derive` line replaced all this code:

```purs
-- No need to write this
instance Semiring Score where
  zero = Score 0
  add (Score a) (Score b) = Score (a + b)
  mul (Score a) (Score b) = Score (a * b)
  one = Score 1
```

[Data.Newtype](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype) provides useful functions via deriving newtypes instances:

```purs
import Data.Newtype (Newtype, un)

newtype Address = Address String
derive instance Newtype Address _

printAddress :: Address -> Eff _ Unit
printAddress address = Console.log (un Address address)
```

### Deriving from `Generic`

For type classes without build-in support for deriving (such as `Show`) and for types other than newtypes where newtype deriving cannot be used, you can derive from `Generic` if the author of the type class library has implemented a generic version.

```purs
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Console (logShow)

derive instance Generic MyADT _

instance Show MyADT where
  show = genericShow

-- logs `[Some,(Arbitrary 1),(Contents 2.0 "Three")]`
main = logShow [Some, Arbitrary 1, Contents 2.0 "Three"]
```

## Type class constraints

Here is a type class constraint `Eq a`, separated from the rest of the type by a double arrow `=>`:

```purs
threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3
```

This type says that we can call `threeAreEqual` with any choice of type `a`, as long as there is an `Eq` instance available for `a`.

Multiple constraints can be specified by using the `=>` symbol multiple times:

```purs
showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2
  | a1 < a2 = show a1 <> " is less than " <> show a2
  | a1 > a2 = show a1 <> " is greater than " <> show a2
  | otherwise = show a1 <> " is equal to " <> show a2
```

The implementation of type class instances can depend on other type class instances. Those instances should be grouped in parentheses and separated by commas on the left-hand side of the `=>` symbol:

```purs
instance (Show a, Show b) => Show (Either a b) where
  ...
```

## The Warn type class

There is a type class in `Prim` called `Warn`. When the compiler solves a `Warn` constraint it will trivially solve the instance and print out the message as a user defined warning.

```purescript
meaningOfLife :: Warn (Text "`meaningOfLife` result is hardcoded, for now.) => Int
meaningOfLife = 42
```

## Functors

`<$>` is the infix alias of the `map` operator defined in the [Functor](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor) type class.

```purs
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

The two following lines are equivalent:

```purs
map (\n -> n + 1) (Just 5)
(\n -> n + 1) <$> (Just 5)
```

## Applicatives

To *lift* a function means to turn it into a function that works with functor-wrapped arguments. Applicative functors are functors that allow lifting of functions.

`<*>` is the infix alias of the `apply` operator defined in the [Apply](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Apply) type class (that extends [`Functor`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor)). `<*>` is equivalent to [`|> andMap`](https://thoughtbot.com/blog/running-out-of-maps#one-liner-to-rule-them-all) in Elm.

The [Applicative](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Applicative) type class extends the `Apply` type class with a `pure` function that takes a value and returns that value lifted into the applicative functor. With `Maybe`, `pure` is the same as `Just`, and with `Either`, `pure` is the same as `Right`, but it is recommended to use `pure` in case of an eventual applicative functor change.

```purs
class Applicative f where
  pure :: a -> f a
  apply :: f (a -> b) -> f a -> f b -- "inherited" from the `Apply` type class
```

Applicative lets us perform N operations independently, then it aggregates the results for us.
You are in an applicative context when using [`Decoder`](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#map2) in Elm.

### Applicative validation

Let’s lift the function `fullName` over a `Maybe`:

```purs
import Prelude
import Data.Maybe

fullName :: String -> String -> String -> String
fullName first middle last = last <> ", " <> first <> " " <> middle

fullName "Phillip" "A" "Freeman" -- "Freeman, Phillip A"

fullName <$> Just "Phillip" <*> Just "A" <*> Just "Freeman" -- Just ("Freeman, Phillip A")

fullName <$> Just "Phillip" <*> Nothing <*> Just "Freeman" -- Nothing
```

Just like with `Maybe`, if we lift `fullName` over `Either String String`, we get a unique error even if multiple errors occur:

```purs
import Test.Assert (assert)
import Data.Either (Either(..))

type Contact =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , country :: String
  }

goodContact :: Contact
goodContact =
  { firstName: "John"
  , lastName: "Doe"
  , address:
      { street: "123 Main St."
      , city: "Springfield"
      , country: "USA"
      }
  }

badContact :: Contact
badContact = goodContact { firstName = "", lastName = "" }

nonEmptyEither :: String -> String -> Either String String
nonEmptyEither fieldName value
  | value == "" = Left $ "Field '" <> fieldName <> "' cannot be empty"
  | otherwise = Right value

validateContactEither :: Contact -> Either String Contact
validateContactEither c = { firstName: _, lastName: _, address: _ }
  <$> nonEmptyEither "First Name" c.firstName
  <*> nonEmptyEither "Last Name" c.lastName
  -- lifting the `c.address` value into `Either` (we could also have used `Right c.address`)
  <*> pure c.address

assert $ validateContactEither goodContact == Right goodContact
assert $ validateContactEither badContact ==  Left "Field 'First Name' cannot be empty"
```

To get an array of all the errors we can use the `V` functor of [`Data.Validation.Semigroup`](https://pursuit.purescript.org/packages/purescript-validation/docs/Data.Validation.Semigroup) that it allows us to collect multiple errors using an arbitrary semigroup (such as `Array String` in the example below).

```purs
import Data.Validation.Semigroup (V, invalid, isValid)

type ErrorMessages = Array String

nonEmptyV :: String -> String -> V ErrorMessages String
nonEmptyV fieldName value
  | value == "" = invalid [ "Field '" <> fieldName <> "' cannot be empty" ]
  | otherwise = pure value

validateContactV :: Contact -> V ErrorMessages Contact
validateContactV c = { firstName: _, lastName: _, address: _ }
  <$> nonEmptyV "First Name" c.firstName
  <*> nonEmptyV "Last Name" c.lastName
  <*> pure c.address

assert $ isValid $ validateContactV goodContact
assert $ not isValid $ validateContactV badContact
assert $ validateContactV badContact ==
  invalid
    [ "Field 'First Name' cannot be empty"
    , "Field 'Last Name' cannot be empty"
    ]
```

### Applicative do notation

With the `ado` keyword:

```purs
validateContactVAdo :: Contact -> V ErrorMessages Contact
validateContactVAdo c = ado
  fistName <- nonEmptyV "First Name" c.firstName
  lastName <- nonEmptyV "Last Name" c.lastName
  address <- pure c.address
  in { firstName, lastName, address }
```

## Monads

`>>=` is the infix alias of the `bind` operator defined in the [Bind](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Bind) type class (that extends [`Apply`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Apply)). `>>=` is equivalent to [`|> andThen`](https://package.elm-lang.org/packages/elm/core/latest/Maybe#andThen) in Elm.

The [Monad](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Applicative) type class combines the operations of the `Bind` and Applicative type classes. Therefore, `Monad` instances represent type constructors which support both sequential composition and function lifting.

```purs
class Monad m where
  bind :: m a -> (a -> m b) -> m b
```

So, to define a monad we need to define the `map`, `apply`, `pure` and `bind` operations:

```purs
data Box a =
  Box a

instance Functor Box where
  map :: forall a b. (a -> b) -> Box a -> Box  b
  map f (Box a) = Box (f a)

instance Apply Box where
  apply :: forall a b. Box (a -> b) -> Box a -> Box  b
  apply (Box f) (Box a) = Box (f a)

instance Applicative Box where
  pure :: forall a. a -> Box a
  pure a = Box a

instance Bind Box where
  bind :: forall a b. Box a -> (a -> Box b) -> Box b
  bind (Box a) f = f a

instance Monad Box
```

Monadic operations operate sequentially not concurrently. That’s great when we have a dependency between the operations e.g. lookup user_id based on email then fetch the inbox based on the user_id. But for independent operations monadic calls are very inefficient as they are inherently sequential. Monads fail fast which makes them poor for form validation and similar use cases. Once something "fails" the operation aborts.
You are in a monadic context when using [`Task`](https://package.elm-lang.org/packages/elm/core/latest/Task#andThen) in Elm.

### Monad do-notation

The `do` keyword is "syntactic sugar" for chained `>>=`. It removes the need for indentations.

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
  let y = x + 4 -- `in` keyword not needed
  z <- toString y
  print z -- not `value <- computation` but just `computation`
```

## Effects

The `main` function of PureScript programs uses the `Effect` monad for side effects:

```purs
import Effect.Random (random) -- random :: Effect Number

main :: Effect Unit
main =
  (log "Below is a random number between 0.0 and 1.0:") >>= (\_ ->
    random >>= (\n->
      log $ show n
    )
  )
```

and is more readable using the do-notation:

```purs
main :: Effect Unit
main = do
  log "Below is a random number between 0.0 and 1.0:"
  n <- random
  log $ show n
```

The above example works because the last line has a `log` that returns `Effect Unit`.
We can use the [`void`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Prelude#v:void) function to ignore the type wrapped by a Functor and replace it with `Unit`:

```purs
void :: forall f a. Functor f => f a -> f Unit
```

That is useful when using the do-notation:

```purs
main :: Effect Unit
main = do
  log "Generating random number..."
  void random
```

## Asynchronous Effects (`Aff`)

Using asynchronous effects in PureScript is like using promises in JavaScript.

PureScript applications use the `main` function in the context of the `Effect` monad. To start the `App` monad context from the `Effect` context, we use the [`launchAff`](https://pursuit.purescript.org/packages/purescript-aff/docs/Effect.Aff#v:launchAff) function (or `launchAff_`  which is `void $ launchAff`).

When we have an Effect-based computation that we want to run in some other monadic context, we can use `liftEffect` from [Effect.Class](https://pursuit.purescript.org/packages/purescript-effect/docs/Effect.Class) if the target monad has an instance for `MonadEffect`:

```purs
class (Monad m) ⇐ MonadEffect m where
-- same as liftEffect :: forall a. Effect a -> m a
  liftEffect :: Effect ~> m
```

`Aff` has an instance for `MonadEffect`, so we can lift `Effect`-based computations (such as `log`) into an `Aff` monadic context:

```purs
import Prelude

import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Timer (setTimeout, clearTimeout)

main :: Effect Unit
main = launchAff_ do
  timeoutID <- liftEffect $ setTimeout 1000 (log "This will run after 1 second")

  delay (Milliseconds 1300.0)

  liftEffect do
    log "Now cancelling timeout"
    clearTimeout timeoutID
```

We can run multiple computations concurrently with [`forkAff`](https://pursuit.purescript.org/packages/purescript-aff/docs/Effect.Aff#v:forkAff). Then, we'll use [`joinFiber`](https://pursuit.purescript.org/packages/purescript-aff/docs/Effect.Aff#v:joinFiber) to ensure all computations are finished before we do another computation.

```purs
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, forkAff, joinFiber, launchAff_)

main :: Effect Unit
main = launchAff_ do

  fiber1 <- forkAff do
    liftEffect $ log "Fiber 1: Waiting for 1 second until completion."
    delay $ Milliseconds 1000.0
    liftEffect $ log "Fiber 1: Finished computation."

  fiber2 <- forkAff do
    liftEffect $ log "Fiber 2: Computation 1 (takes 300 ms)."
    delay $ Milliseconds 300.0
    liftEffect $ log "Fiber 2: Computation 2 (takes 300 ms)."
    delay $ Milliseconds 300.0
    liftEffect $ log "Fiber 2: Computation 3 (takes 500 ms)."
    delay $ Milliseconds 500.0
    liftEffect $ log "Fiber 2: Finished computation."

  fiber3 <- forkAff do
    liftEffect $ log "Fiber 3: Nothing to do. Just return immediately."
    liftEffect $ log "Fiber 3: Finished computation."

  joinFiber fiber1
  liftEffect $ log "Fiber 1 has finished. Now joining on fiber 2"
  joinFiber fiber2
  liftEffect $ log "Fiber 3 has finished. Now joining on fiber 3"
  joinFiber fiber3
  liftEffect $ log "Fiber 3 has finished. All fibers have finished their computation."
```

If instead of `forkAff` we used [`suspendAff`](https://pursuit.purescript.org/packages/purescript-aff/docs/Effect.Aff#v:suspendAff), then the fibers would not be run *concurrently* as soon as defined, but they would be suspended and ran *sequentially* one by one after their respective `joinFiber`.

## Foreign Function Interface (FFI)

### Calling PureScript from JavaScript

```purs
-- Purescript
module Tools where

import Prelude

-- find the greatest common divisor
gcd :: Int -> Int -> Int
gcd n m
  | n == 0 = m
  | m == 0 = n
  | n > m = gcd (n - m) m
  | otherwise = gcd (m - n) n
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

export const calculateInterest = amount => months => amount * Math.exp(0.1, months)
```

This time, we can assign the curried function type directly:

```purs
-- PureScript

foreign import calculateInterest :: Number -> Number -> Number
```

#### Promises

Promises in JavaScript translate directly to asynchronous effects in PureScript with the help of [`Promise.Aff`](https://pursuit.purescript.org/packages/purescript-js-promise-aff/docs/Promise.Aff).

In JavaScript, you need to wrap asynchronous functions in a PureScript Effect with a "thunk" `() =>` so the function is not considered pure and is run every time:

```js
// JavaScript

export const catBase64JS = text => fontSize =>
    async () => {
        const response = await fetch(`https://cataas.com/cat/says/${text}?fontSize=${fontSize}&fontColor=red`)
        const array = await response.body.getReader().read()
        return btoa(String.fromCharCode.apply(null, array.value))
    }
```

Then in PureScript use the `toAffE` function:

```purs
-- PureScript

import Promise.Aff (Promise, toAffE)

foreign import catBase64JS :: String -> Int -> Effect (Promise String)

catBase64 :: String -> Int -> Aff String
catBase64 text fontSize = toAffE $ catBase64JS text fontSize
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

Note: [The usage of the `F` alias is now discouraged](https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md#discouraging-usage-of-f-and-ft).

`Except` is an monad for handling exceptions, much like `Either`. We can convert a value in the `F` monad into a value in the `Either` monad by using the `runExcept` function.

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

In [The state of PureScript 2023 survey results](https://github.com/purescript/survey/blob/main/results/The%20State%20of%20PureScript%202023%20Results.pdf), at page 24, you can see a chart of the most used front-end frameworks:

<img src="ps-frameworks-2023.png" alt="PureScript frameworks usage chart for 2023" width="600">

- [**Halogen**](https://purescript-halogen.github.io/purescript-halogen/) is by far the most used front-end framework for PureScript.
  - *Not* using [The Elm Architecture](https://guide.elm-lang.org/architecture/) ("TEA").
  - You can create components if you’re into that stuff. There is [purescript-halogen-store](https://github.com/thomashoneyman/purescript-halogen-store) for global state management.
  - Does not have a router. You will have to use [purescript-routing](https://github.com/purescript-contrib/purescript-routing) or [purescript-routing-duplex](https://github.com/natefaubion/purescript-routing-duplex).
  - [A bit slower](https://github.com/purescript-halogen/purescript-halogen/issues/632#issuecomment-609952547) than Elm.
  - About twice heavier than Elm with brotli compression for a real world app.


- [**Elmish**](https://collegevine.github.io/purescript-elmish/), as its name suggests, uses Elm ideas:
  - (loosely) follows TEA principles, implemented as a thin layer on top of React.
  - Has [routing capabilities](https://pursuit.purescript.org/packages/purescript-elmish/0.1.0/docs/Elmish.Component#t:ComponentReturnCallback).
  - Bloated with React 17 (I couldn’t figure out how to get it to work with [Preact](https://preactjs.com/)).
  - Seems abandoned as the package maintainer [no longer has the motivation to update it to React 18 and above](https://github.com/collegevine/purescript-elmish/pull/66#issuecomment-1810431289).

- [**Flame**](https://flame.asafe.dev/) is a relatively new framework inspired by Elm:

  - The [Flame architecture](https://flame.asafe.dev/events) is inspired by TEA.
  - Does not have a router. You will have to use [purescript-routing](https://github.com/purescript-contrib/purescript-routing) or [purescript-routing-duplex](https://github.com/natefaubion/purescript-routing-duplex).
  - Performance is [comparable to Elm](https://flame.asafe.dev/benchmarks).
  - Server Side Rendering is [supported](https://flame.asafe.dev/rendering).

### Flame example

This repo contains a minimal Flame example with a counter increment/decrement buttons, random number generation, synchronous and asynchronous FFI calls, subscription and decoding of a JSON object.

#### Installation

```bash
npm i
```

#### Vite setup notes

- When using [PureScript IDE for VS code](https://marketplace.visualstudio.com/items/nwolverson.ide-purescript) the project is built every time you save a file. There is no need for a special Vite plugin. `output/Main/index.js` is simply imported in the JavasScript entry file.
- [Terser](https://terser.org/) is used for better compression results.


## Go further

We only covered the basics of PureScript here. If you want to learn more, check out the following resources:

- [Official documentation](https://github.com/purescript/documentation) of course.
- [PureScript: Jordan's Reference](https://jordanmartinez.github.io/purescript-jordans-reference-site/) is a must.
- [PureScript By Example](https://book.purescript.org/) is a free online book containing several practical projects for PureScript beginners.
- [Functional Programming Made Easier](https://leanpub.com/fp-made-easier) by Charles Scalfani is a great book to learn PureScript.
- [PureScript Discourse](https://discourse.purescript.org/) forum
- [PureScript Discord](https://purescript.org/chat) chat

## License

[MIT](https://github.com/laurentpayot/purescript-for-elm-developers/blob/main/LICENSE)

## Stargazers :heart:

[![Stargazers repo roster for @laurentpayot/purescript-for-elm-developers](http://reporoster.com/stars/laurentpayot/purescript-for-elm-developers)](https://github.com/laurentpayot/purescript-for-elm-developers/stargazers)
