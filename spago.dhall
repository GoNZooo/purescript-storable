{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "foreign"
    , "generics-rep"
    , "proxy"
    , "simple-json"
    , "spec"
    , "spec-quickcheck"
    , "transformers"
    , "web-storage"
    ]
, packages =
    ./packages.dhall
}
