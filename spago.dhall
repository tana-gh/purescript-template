{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-template"
, dependencies =
  [ "aff"
  , "console"
  , "const"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "spec/**/*.purs" ]
}
