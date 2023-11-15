{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "purescript-counter-web-app"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "flame"
  , "foreign"
  , "js-promise-aff"
  , "js-timers"
  , "maybe"
  , "prelude"
  , "random"
  , "tuples"
  , "validation"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/ps/**/*.purs", "tests/ps/**/*.purs" ]
}
