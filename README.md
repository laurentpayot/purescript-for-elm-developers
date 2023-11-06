# From Elm to PureScript 🤯

Based on information picked from:
- https://github.com/alpacaaa/elm-to-purescript-cheatsheet
- https://github.com/purescript/documentation
- https://book.purescript.org
- https://jordanmartinez.github.io/purescript-jordans-reference-site/
- https://learnxinyminutes.com/docs/purescript/
- https://gist.github.com/justinwoo/0118be3e4a0d7394a99debbde2515f9b
- https://www.tobyhobson.com/posts/cats/
- https://github.com/alpacaaa/zero-bs-haskell

## Common packages

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

The they are `NonEmpty` modules for [Array](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array.NonEmpty) and [List](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.NonEmpty). They are quite useful to flatten cases as described in the famous ["Parse, don’t validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) blog post.

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

## Type class constraints

Here is a type class constraint `Eq a`, separated from the rest of the type by a double arrow `=>`:

```purs
threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3
```

This type says that we can call `threeAreEqual` with any choice of type `a`, as long as there is an `Eq` instance available for `a` in one of the imported modules.

Multiple constraints can be specified by using the => symbol multiple times:

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

Currently, instances for the following classes can be derived by the compiler:
- Data.Generic.Rep (class Generic) [see below](#deriving-from-generic)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Ord#t:Ord)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor#t:Functor)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype)

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
  (<*>) :: f (a -> b) -> f a -> f b -- "inherited" from the `Apply` type class
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
nonEmptyEither label "" = Left $ "Field '" <> label <> "' cannot be empty"
nonEmptyEither _ value = Right value

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
nonEmptyV label "" = invalid [ "Field '" <> label <> "' cannot be empty" ]
nonEmptyV _ value = pure value

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
  (>>=) :: m a -> (a -> m b) -> m b
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

The `main` function of PureScript programs uses the `Effect` monad:

```purs
main :: Effect Unit
main = (log "This is outputted first") >>= (\_ ->
          (log "This is outputted second") >>= (\_ ->
            log "This is outputted third"
          )
        )
```

and is more readable using the do-notation:

```purs
main :: Effect Unit
main = do
  log "This is outputted first"
  log "This is outputted second"
  log "This is outputted third"
```

The above example works because `log` returns `Effect Unit`.
We can use the [`void`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Prelude#v:void) function to ignore the type wrapped by a Functor and replace it with `Unit`:

```purs
void :: forall f a. Functor f => f a -> f Unit
```

That is useful when using the do-notation:

```purs
foo :: Int -> Effect Int

main :: Effect Unit
main = do
  log "Starting..."
  void $ foo 42
  log "Done!"
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

export const calculateInterest = amount => months => amount * Math.exp(0.1, months)
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

## Vite setup

When using [PureScript IDE for VS code](https://marketplace.visualstudio.com/items/nwolverson.ide-purescript) the project is built every time you save a file. There is no need for a special Vite plugin. Just import `output/Main/index.js` in your Vite `main.js` file.

## The Flame front-end framework

[Halogen](https://purescript-halogen.github.io/purescript-halogen/) is the most used front-end framework for PureScript but we will use the [**Flame**](https://flame.asafe.dev/) framework because:

- The [Flame architecture](https://flame.asafe.dev/events) is inspired by the [Elm architecture](https://guide.elm-lang.org/architecture/).
- Performance is [comparable to Elm](https://flame.asafe.dev/benchmarks).
- Server Side Rendering is [supported](https://flame.asafe.dev/rendering).

Note: If GitHub errors like "Empty reply from server" during `pnpm spago install flame`, remove the erroneous packages directories from the /.spago folder and try `pnpm spago build` to reinstall them.
