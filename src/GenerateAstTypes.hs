{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified AST.Module as Module
import qualified AST.Expression.Canonical as Can
import qualified AST.Effects
import qualified AST.Expression.Canonical
import qualified AST.Expression.General
import qualified AST.Literal
import qualified AST.Type
import qualified AST.Variable
import qualified AST.Pattern
import qualified AST.Module
import qualified AST.Module.Name
import qualified Reporting.Region
import qualified Reporting.Annotation
import qualified Elm.Package

import Elm.Module
import Data.Proxy


main :: IO ()
main =
  putStrLn $ makeElmModule "ElmAST"
    [ DefineElm (Proxy :: Proxy (AST.Effects.Effects a b))
    , DefineElm (Proxy :: Proxy (AST.Effects.PortCanonical))
    , DefineElm (Proxy :: Proxy (AST.Effects.Kind))
    , DefineElm (Proxy :: Proxy (AST.Effects.ManagerType))
    , DefineElm (Proxy :: Proxy (AST.Expression.Canonical.Facts))
    , DefineElm (Proxy :: Proxy (AST.Expression.Canonical.Def))
    , DefineElm (Proxy :: Proxy (AST.Expression.General.Main c))
    , DefineElm (Proxy :: Proxy (AST.Expression.General.Expr' d e f g))
    , DefineElm (Proxy :: Proxy (AST.Literal.Literal))
    , DefineElm (Proxy :: Proxy (AST.Literal.GLShaderTipe))
    , DefineElm (Proxy :: Proxy (AST.Literal.GLTipe))
    , DefineElm (Proxy :: Proxy (AST.Module.Name.Canonical))
    , DefineElm (Proxy :: Proxy (AST.Module.Module h))
    , DefineElm (Proxy :: Proxy (AST.Pattern.Pattern' j k))
    , DefineElm (Proxy :: Proxy (AST.Type.Aliased l))
    , DefineElm (Proxy :: Proxy (AST.Type.Canonical))
    , DefineElm (Proxy :: Proxy (AST.Variable.Canonical))
    , DefineElm (Proxy :: Proxy (AST.Variable.Home))
    , DefineElm (Proxy :: Proxy (AST.Variable.TopLevel))
    , DefineElm (Proxy :: Proxy (Reporting.Annotation.Annotated m n))
    , DefineElm (Proxy :: Proxy (Reporting.Region.Region))
    , DefineElm (Proxy :: Proxy (Reporting.Region.Position))
    , DefineElm (Proxy :: Proxy (Elm.Package.Name))
    , DefineElm (Proxy :: Proxy (Elm.Package.Version))
    ]
