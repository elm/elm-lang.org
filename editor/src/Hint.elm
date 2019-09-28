module Hint exposing
  ( Table
  , Info(..)
  , Hint
  , lookup
  , defaultTable
  , buildTable
  )


import Dict exposing (Dict)
import Deps
import Header
import Set exposing (Set)



-- TABLE


type Table =
  Table (Dict String Info)


type Info
  = Specific Hint
  | Ambiguous


type alias Hint =
  { text : String
  , href : String
  }



-- LOOKUP


lookup : String -> Table -> Maybe Info
lookup name (Table table) =
  Dict.get name table



-- DEFAULT TABLE


defaultTable : Table
defaultTable =
  Table keywordsAndConventions


-- NOTE: The logic in editor.js normalizes certain keywords:
--
--   1. "type" is converted to "alias" if it appears to be in a type alias
--   2. "as" is converted to "import" if it appears to be in an import
--
keywordsAndConventions : Dict String Info
keywordsAndConventions =
  let
    hint t h = Specific (Hint t h)

    if_ = hint "If Expressions" "https://guide.elm-lang.org/core_language.html#if-expressions"
    tea_ = hint "The Elm Architecture" "https://guide.elm-lang.org/architecture/"
    subs_ = hint "Subscriptions" "https://guide.elm-lang.org/effects/"
    case_ = hint "Pattern Matching" "https://guide.elm-lang.org/types/pattern_matching.html"
    import_ = hint "Imports" "https://github.com/elm/compiler/blob/master/hints/imports.md"
  in
  Dict.fromList
    [ ("if", if_)
    , ("then", if_)
    , ("else", if_)
    , ("case", case_)
    , ("of", case_)
    , ("import", import_)
    , ("exposing", import_)
    , (":", hint "Reading Types" "https://guide.elm-lang.org/types/reading_types.html")
    , ("type", hint "Custom Types" "https://guide.elm-lang.org/types/custom_types.html")
    , ("alias", hint "Type Aliases" "https://guide.elm-lang.org/types/type_aliases.html")
    , ("port", hint "Ports" "https://guide.elm-lang.org/interop/ports.html")

    -- CONVENTIONS

    , ("Model", tea_)
    , ("Msg", tea_)
    , ("Cmd", hint "Commands" "https://guide.elm-lang.org/effects/")
    , ("Sub", subs_)
    , ("def:view", tea_)
    , ("def:update", tea_)
    , ("def:init", hint "init" "https://guide.elm-lang.org/effects/http.html#init")
    , ("def:subscriptions", subs_)

--  TODO add hints for `let` and `as` patterns
--  , ("let", let_)
--  , ("in", let_)
--  , ("as", hint "As Patterns" "/TODO")
    ]



-- BUILD TABLE


buildTable : Header.Imports -> Deps.Info -> Table
buildTable (Header.Imports imports) depsInfo =
  Table <| Dict.union keywordsAndConventions <|
    List.foldl addHint Dict.empty (List.foldl (addImport depsInfo) [] imports)


addHint : (String, Hint) -> Dict String Info -> Dict String Info
addHint (key, hint) table =
  case Dict.get key table of
    Nothing ->
      Dict.insert key (Specific hint) table

    Just info ->
      case info of
        Ambiguous ->
          table

        Specific { text, href } ->
          if hint.text == text && hint.href == href
          then table
          else Dict.insert key Ambiguous table



-- ADD IMPORT


addImport : Deps.Info -> Header.Import -> List (String, Hint) -> List (String, Hint)
addImport depsInfo imp hints =
  case Dict.get imp.module_ depsInfo of
    Nothing ->
      hints

    Just info ->
      let
        name = imp.module_
        url = "https://package.elm-lang.org/packages/" ++ info.pkg ++ "/latest/" ++ name
      in
      hints
        |> addQualified name imp.alias_ url info
        |> addExposed name imp.exposing_ url info



-- ADD QUALIFIED


addQualified : String -> Maybe String -> String -> Deps.ModuleInfo -> List (String, Hint) -> List (String, Hint)
addQualified moduleName maybeAlias moduleUrl info hints =
  let
    qualifier =
      Maybe.withDefault moduleName maybeAlias

    toHint name =
      ( qualifier ++ "." ++ name
      , Hint (moduleName ++ "." ++ name) (moduleUrl ++ "#" ++ name)
      )

    toVariantHint typeName variant =
      ( qualifier ++ "." ++ variant
      , Hint (moduleName ++ "." ++ variant) (moduleUrl ++ "#" ++ typeName)
      )
  in
  ("module:" ++ moduleName, Hint moduleName moduleUrl) :: hints
    |> addNames toHint info.values
    |> addNames toHint info.aliases
    |> addUnions toHint toVariantHint info.unions


addNames : (a -> b) -> Set a -> List b -> List b
addNames func set list =
  Set.foldr (\a bs -> func a :: bs) list set


addUnions : (k -> b) -> (k -> v -> b) -> Dict k (List v) -> List b -> List b
addUnions nameFunc variantFunc unions initialHints =
  let
    addVariant name variant hints =
      variantFunc name variant :: hints

    addUnion name variants hints =
      nameFunc name :: List.foldr (addVariant name) hints variants
  in
  Dict.foldr addUnion initialHints unions



-- ADD EXPOSED


addExposed : String -> Header.Exposing -> String -> Deps.ModuleInfo -> List (String, Hint) -> List (String, Hint)
addExposed moduleName exposing_ moduleUrl info hints =
  let
    toHint name =
      ( name, Hint (moduleName ++ "." ++ name) (moduleUrl ++ "#" ++ name) )

    toOperatorHint op =
      ( op, Hint ("(" ++ op ++ ")") (moduleUrl ++ "#" ++ op) )

    toVariantHint typeName variant =
      ( variant, Hint (moduleName ++ "." ++ variant) (moduleUrl ++ "#" ++ typeName) )
  in
  case exposing_ of
    Header.None ->
      hints

    Header.All ->
      hints
        |> addNames toOperatorHint info.ops
        |> addNames toHint info.aliases
        |> addNames toHint info.values
        |> addUnions toHint toVariantHint info.unions

    Header.Some { ops, lower, upper } ->
      hints
        |> addNames toOperatorHint (Set.intersect info.ops ops)
        |> addNames toHint (Set.intersect info.values lower)
        |> addNames toHint (Set.filter (\k -> Dict.member k upper) info.aliases)
        |> addSomeUnions toHint toVariantHint info.unions upper


addSomeUnions : (String -> b) -> (String -> v -> b) -> Dict String (List v) -> Dict String Bool -> List b -> List b
addSomeUnions nameFunc variantFunc allUnions exposedUnions initialHints =
  let
    ignore _ _ hints =
      hints

    addVariant name variant hints =
      variantFunc name variant :: hints

    addUnion name variants isOpen hints =
      if isOpen
      then nameFunc name :: List.foldr (addVariant name) hints variants
      else nameFunc name :: hints
  in
  Dict.merge ignore addUnion ignore allUnions exposedUnions initialHints
