-- # Implementation.Expression.Internal.MExpr
-- M式 (Meta Expression)
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Implementation.Expression.Internal.MExpr
    where

import Text.ParserCombinators.ReadP
import Data.Functor.Foldable
import Implementation.Decode
import Implementation.TreeLike
import Implementation.Expression.Hosi
import Implementation.Expression.Internal.Expr

import Debug.Trace

-- ## M式

newtype MExpr a = MExpr a deriving (Eq, Show)

instance TreeLike (MExpr (Expr Hs)) where
    branches :: MExpr (Expr Hs) -> [MExpr (Expr Hs)]
    branches = \ case
        MExpr e -> case branches e of
            [s,t]
                | s == e₁   -> []
                | t == e₁   -> [MExpr s]
                | otherwise -> [MExpr s, MExpr t]
            _ -> error "branches @(MExpr (Expr Hs)): impossible"

m₁ :: Variable (Expr Hs) -> MExpr (Expr Hs)
m₁ = \ case
    Variable e -> MExpr (e₂ e₁ e)

m₂ :: MExpr (Expr Hs)
m₂ = MExpr (e₂ e₁ e₁)

m₃ :: MExpr (Expr Hs) -> MExpr (Expr Hs)
m₃ (MExpr t) = MExpr (e₂ t e₁)

m₄ :: MExpr (Expr Hs) -> MExpr (Expr Hs) -> MExpr (Expr Hs)
m₄ (MExpr s) (MExpr t) = MExpr (e₂ s t)

sizeMExpr :: MExpr (Expr Hs) -> Int
sizeMExpr = sizeAMExpr . decode

-- ## M式のAST

data AMExprF r
    = VarF AVariable
    | NullF
    | SharpF r
    | PairF r r
    deriving (Eq, Functor)

data AMExpr
    = Var AVariable
    | Null
    | Sharp AMExpr
    | Pair AMExpr AMExpr
    deriving (Eq)

type instance Base AMExpr = AMExprF

instance Recursive AMExpr where
    project :: AMExpr -> Base AMExpr AMExpr
    project = \ case
        Var v    -> VarF v
        Null     -> NullF
        Sharp t  -> SharpF t
        Pair s t -> PairF s t

instance Corecursive AMExpr where
    embed :: Base AMExpr AMExpr -> AMExpr
    embed = \ case
        VarF v    -> Var v
        NullF     -> Null
        SharpF t  -> Sharp t
        PairF s t -> Pair s t

instance Decode (MExpr (Expr Hs)) AMExpr where
    encode :: AMExpr -> MExpr (Expr Hs)
    encode = cata phi
        where
            phi = \ case
                VarF v    -> m₁ (encode v)
                NullF     -> m₂
                SharpF t  -> m₃ t
                PairF s t -> m₄ s t

    decode :: MExpr (Expr Hs) -> AMExpr
    decode = ana psi
        where
            psi :: MExpr (Expr Hs) -> Base AMExpr (MExpr (Expr Hs))
            psi me@(MExpr e) = case branches me of
                []    -> case branches e of
                    [_,t] -> case branches t of
                        []    -> NullF
                        [ss,tt] -> VarF (decode (variable ss tt))
                        _     -> error $ "decode @(MExpr (Expr Hs)) @AMExpr: impossible"
                    _       -> error $ "decode @(MExpr (Expr Hs)) @AMExpr: impossible"
                [t]   -> SharpF t
                [s,t] -> PairF s t
                _     -> error $ "decode @(MExpr (Expr Hs)) @AMExpr: impossible"

instance Show AMExpr where
    showsPrec :: Int -> AMExpr -> ShowS
    showsPrec _ = cata phi
        where
            phi = \ case
                VarF v    -> shows v
                NullF     -> ('(' :) . (')' :)
                SharpF t  -> ('#' :) . showParen True t
                PairF s t -> showParen True (s . showChar '.' . t)

instance Read AMExpr where
    readsPrec :: Int -> ReadS AMExpr
    readsPrec _ = readP_to_S rAMExpr

rAMExpr :: ReadP AMExpr
rAMExpr = rVar +++ rNull +++ rSharp +++ rPair

rVar :: ReadP AMExpr
rVar = Var <$> rAVariable

rNull :: ReadP AMExpr
rNull = Null <$ string "()"

rSharp :: ReadP AMExpr
rSharp = Sharp <$>  (string "#(" *>  rAMExpr) <* string ")"

rPair :: ReadP AMExpr
rPair = Pair <$> (string "(" *> rAMExpr <* skipSpaces <* char '.' <* skipSpaces) <*> rAMExpr <* char ')'

sizeAMExpr :: AMExpr -> Int
sizeAMExpr = cata phi
    where
        phi = \ case
            SharpF t  -> succ t
            PairF s t -> succ (s + t)
            _         -> 0

{- ^
>>> sample1 = m₁ e₁ e₁
>>> sample1
MExpr (Expr ○●○●●)
>>> sample2 = decode @(MExpr (Expr Hs)) @AMExpr sample1
>>> sample2
●V●
>>> sample1 == encode sample2
True
>>> sample2 == read (show sample2)
True
>>> sample3 = m₂
>>> sample3
MExpr (Expr ○●●)
>>> sample4 = decode @(MExpr (Expr Hs)) @AMExpr sample3
>>> sample4
()
>>> sample4 == read (show sample4)
True
>>> sample5 = m₃ sample1
>>> sample5
MExpr (Expr ○○●○●●●)
>>> sample6 = decode @(MExpr (Expr Hs)) @AMExpr sample5
>>> sample6
#(●V●)
>>> sample6 == read (show sample6)
True
>>> sample7 = m₃ sample3
>>> sample7 
MExpr (Expr ○○●●●)
>>> sample8 = decode @(MExpr (Expr Hs)) @AMExpr sample7
>>> sample8
#(())
>>> sample8 == read (show sample8)
True
>>> sample9 = m₄ sample1 sample3
>>> sample9
MExpr (Expr ○○●○●●○●●)
>>> sample10 = decode @(MExpr (Expr Hs)) @AMExpr sample9
>>> sample10
(●V●.())
>>> sample9 == encode sample10
True
>>> sample10 == read (show sample10)
True

>>> sample11 = m₄ sample3 sample5
>>> sample11
MExpr (Expr ○○●●○○●○●●●)
>>> sample12 = decode @(MExpr (Expr Hs)) @AMExpr sample11
>>> sample12
(().#(●V●))
>>> sample11 == encode sample12
True
>>> sample12 == read (show sample12)
True
-}
