{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Test.DocTest

main :: IO ()
main
    = doctest
    [ "src/Implementation/Expression/Internal/Expr.hs"
    , "src/Implementation/Expression/Internal/MExpr.hs"
    , "src/Implementation/Expression/Internal/VarList.hs"
    , "src/Implementation/Expression/Internal/CExpr.hs"
    , "src/Implementation/Expression/Internal/SExpr.hs"
    ]
