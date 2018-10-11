    let defs = ./package-defaults.dhall

in    defs
    ⫽ { name =
        "snack-readme"
    , dependencies =
        [ "lens", "wreq" ]
    , library =
        { source-dirs = "./src" }
    , executable =
        { main =
            "Main.hs"
        , source-dirs =
            "./app"
        , dependencies =
            [ "snack-readme" ]
        }
    }
