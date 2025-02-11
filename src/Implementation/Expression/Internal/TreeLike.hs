-- # Implementation.Expression.Internal.TreeLike
-- TreeLikeクラス
-- 
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Implementation.Expression.Internal.TreeLike
    where

import Data.Functor.Foldable

class TreeLike t where
    branches :: t -> [t]

depth :: TreeLike t => t -> Int
depth = succ . foldl max 0 . map depth . branches

dfs :: TreeLike t => t -> [t]
dfs t = t : concatMap dfs (branches t)

bfs :: TreeLike t => t -> [t]
bfs = concat . levels

levels :: TreeLike t => t -> [[t]]
levels t = [t] : foldr cat [] (levels <$> branches t)
    where
        cat = lzw (++)

lzw :: (a -> a -> a) -> [a] -> [a] -> [a]
lzw f = \ case
    []         -> id
    xxs@(x:xs) -> \ case
        []         -> xxs
        y:ys       -> f x y : lzw f xs ys

paths :: TreeLike t => t -> [[t]]
paths t = case bs of
    [] -> [[t]]
    _  -> [ t:p | b <- bs, p <- paths b ]
    where
        bs = branches t

