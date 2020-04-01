{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-template"
, dependencies =
  [ "aff"
  , "console"
  , "css"
  , "debug"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "prelude"
  , "psci-support"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "spec/**/*.purs" ]
}
