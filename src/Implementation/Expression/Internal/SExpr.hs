-- # Implementation.Expression.Internal.SExpr
-- S式 (Symbolic Expression)
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
module Implementation.Expression.Internal.SExpr
    where

import Data.Functor.Foldable
import Text.ParserCombinators.ReadP
import Implementation.Expression.Hosi
import Implementation.Expression.Internal.Decode
import Implementation.Expression.Internal.TreeLike
import Implementation.Expression.Internal.Expr
import Implementation.Expression.Internal.MExpr

-- ## S式

newtype SExpr a = SExpr a deriving (Eq, Show)

instance TreeLike (SExpr (MExpr (Expr Hs))) where
    branches :: SExpr (MExpr (Expr Hs)) -> [SExpr (MExpr (Expr Hs))]
    branches = \ case
        SExpr me -> SExpr <$> branches me

s₁ :: SExpr (MExpr (Expr Hs))
s₁ = SExpr m₂

s₂ :: SExpr (MExpr (Expr Hs)) -> SExpr (MExpr (Expr Hs)) -> SExpr (MExpr (Expr Hs))
s₂ (SExpr s) (SExpr t) = SExpr (m₄ s t)

-- ## S式のAST

data ASExprF r
    = ANilF
    | ADotedPairF r r
    deriving (Eq, Functor)

data ASExpr
    = ANil
    | ADotedPair ASExpr ASExpr
    deriving (Eq)

type instance Base ASExpr = ASExprF

instance TreeLike ASExpr where
    branches :: ASExpr -> [ASExpr]
    branches = \ case
        ANil           -> []
        ADotedPair s t -> [s, t]

instance Recursive ASExpr where
    project :: ASExpr -> Base ASExpr ASExpr
    project = \ case
        ANil           -> ANilF
        ADotedPair s t -> ADotedPairF s t

instance Corecursive ASExpr where
    embed :: Base ASExpr ASExpr -> ASExpr
    embed = \ case
        ANilF           -> ANil
        ADotedPairF s t -> ADotedPair s t

instance Decode (SExpr (MExpr (Expr Hs))) ASExpr where
    encode :: ASExpr -> SExpr (MExpr (Expr Hs))
    encode = cata phi
        where
            phi = \ case
                ANilF           -> s₁
                ADotedPairF s t -> s₂ s t

    decode :: SExpr (MExpr (Expr Hs)) -> ASExpr
    decode = ana psi
        where
            psi :: SExpr (MExpr (Expr Hs)) -> Base ASExpr (SExpr (MExpr (Expr Hs)))
            psi se = case branches se of
                []    -> ANilF
                [s,t] -> ADotedPairF s t
                _     -> error $ "decode @(SExpr (MExpr (Expr Hs))) @ASExpr: impossible"

instance Show ASExpr where
    showsPrec :: Int -> ASExpr -> ShowS
    showsPrec _ = cata phi
        where
            phi = \ case
                ANilF           -> showString "()"
                ADotedPairF s t -> showString "(" . s . showChar '.' . t . showString ")"

instance Read ASExpr where
    readsPrec :: Int -> ReadS ASExpr
    readsPrec _ = readP_to_S rASExpr

rASExpr :: ReadP ASExpr
rASExpr = rNil +++ rDotedPair

rNil :: ReadP ASExpr
rNil = ANil <$ string "()"

rDotedPair :: ReadP ASExpr
rDotedPair = ADotedPair <$> (char '(' *> rASExpr <* (skipSpaces *> char '.' <* skipSpaces)) <*> rASExpr <* char ')'

{- ^
>>> sample1 = s₁
>>> sample1
SExpr (MExpr (Expr ○●●))
>>> sample2 = decode @(SExpr (MExpr (Expr Hs))) @ASExpr sample1
>>> sample2
()
>>> sample1 == encode sample2
True
>>> sample2 == read (show sample2)
True
>>> sample3 = s₂ sample1 sample1
>>> sample3
SExpr (MExpr (Expr ○○●●○●●))
>>> sample4 = decode @(SExpr (MExpr (Expr Hs))) @ASExpr sample3
>>> sample4
(().())
>>> sample3 == encode sample4
True
>>> sample4 == read (show sample4)
True
>>> sample5 = s₂ sample1 sample3
>>> sample5
SExpr (MExpr (Expr ○○●●○○●●○●●))
>>> sample6 = decode @(SExpr (MExpr (Expr Hs))) @ASExpr sample5
>>> sample6
(().(().()))
>>> sample5 == encode sample6
True
>>> sample6 == read (show sample6)
True
-}
