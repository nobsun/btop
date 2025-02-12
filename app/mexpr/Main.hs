{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType #-}
module Main where

import Data.Bool
import Implementation.Expression.Hosi
import Implementation.Expression.Expr
import Implementation.Decode

import Debug.Trace qualified as Debug

debug :: Bool
debug = () == ()

main :: IO ()
main = do
    { print sample1
    ; print sample2
    }

sample1 :: MExpr (Expr Hs)
sample1 = m₁ (tracing $ variable e₁ e₁)

sample2 :: AMExpr
sample2 = decode @(MExpr (Expr Hs)) @AMExpr sample1

test :: String -> Bool -> IO ()
test s t = putStrLn $ s ++ ": " ++ bool "OK" "NG" t

trace :: String -> a -> a
trace | debug     = Debug.trace
      | otherwise = const id

tracing :: Show a => a -> a
tracing = trace . show <*> id

