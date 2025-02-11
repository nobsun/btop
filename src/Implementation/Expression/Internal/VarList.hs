-- # Implementation.Expression.Internal.VarList
-- 変数リスト
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
module Implementation.Expression.Internal.VarList
    where

import Data.Functor.Foldable
import Text.ParserCombinators.ReadP
import Implementation.Expression.Hosi
import Implementation.Expression.Internal.Decode
import Implementation.Expression.Internal.TreeLike
import Implementation.Expression.Internal.Expr
import Implementation.Expression.Internal.MExpr

-- ## リスト

newtype Lst a = Lst a deriving (Eq, Show)

instance TreeLike (Lst (MExpr (Expr Hs))) where
    branches :: Lst (MExpr (Expr Hs)) -> [Lst (MExpr (Expr Hs))]
    branches = \ case
        Lst me -> Lst <$> drop 1 (branches me)

l₁ :: Lst (MExpr (Expr Hs))
l₁ = Lst m₂

l₂ :: MExpr (Expr Hs) -> Lst (MExpr (Expr Hs)) -> Lst (MExpr (Expr Hs))
l₂ s (Lst l) = Lst (m₄ s l)

-- ## リストのAST

data ALstF r
    = AEmptyF
    | AConsF AMExpr r
    deriving (Eq,Functor)

data ALst
    = AEmpty
    | ACons AMExpr ALst
    deriving (Eq)

type instance Base ALst = ALstF

instance Recursive ALst where
    project :: ALst -> Base ALst ALst
    project = \ case
        AEmpty       -> AEmptyF
        ACons me lst -> AConsF me lst

instance Corecursive ALst where
    embed :: Base ALst ALst -> ALst
    embed = \ case
        AEmptyF       -> AEmpty
        AConsF me lst -> ACons me lst

instance Decode (Lst (MExpr (Expr Hs))) ALst where
    encode :: ALst -> Lst (MExpr (Expr Hs))
    encode = cata phi
        where
            phi = \ case
                AEmptyF       -> l₁
                AConsF me lst -> l₂ (encode me) lst

    decode :: Lst (MExpr (Expr Hs)) -> ALst
    decode = ana psi
        where
            psi lst@(Lst me) = case branches lst of
                []  -> AEmptyF
                [t] -> case branches me of
                    [h,_] -> AConsF (decode h) t
                    _     -> error $ "decode @(Lst (MExpr (Expr Hs))) @ALst: impossible"
                _   -> error $ "decode @(Lst (MExpr (Expr Hs))) @ALst: impossible"

instance Show ALst where
    showsPrec :: Int -> ALst -> ShowS
    showsPrec _ = cata phi
        where
            phi = \ case
                AEmptyF       -> showChar '(' . showChar ')'
                AConsF hd lst -> showChar '(' . shows hd . showChar ',' . lst . showChar ')'

instance Read ALst where
    readsPrec :: Int -> ReadS ALst
    readsPrec _ = readP_to_S rALst

rALst :: ReadP ALst
rALst = between (char '(') (char ')') (foldr ACons AEmpty <$> (sepBy rAMExpr (char ',')))

(∈) :: AMExpr -> ALst -> Bool
(∈) me = cata phi
    where
        phi = \ case
            AEmptyF     -> False
            AConsF hd b -> me == hd || b

lenALst :: ALst -> Int
lenALst = cata phi
    where
        phi = \ case
            AEmptyF    -> 0
            AConsF _ l -> succ l

(⊕) :: ALst -> ALst -> ALst
(⊕) = cata phi
    where
        phi = \ case
            AEmptyF      -> id
            AConsF hd tl -> ACons hd . tl

(−) :: ALst -> AMExpr -> ALst
l − x = case l of
    AEmpty -> AEmpty
    ACons hd tl
        | hd == x   -> tl   
        | otherwise -> ACons hd (tl − x)

vs :: AMExpr -> ALst
vs me = case me of
    Var _ _      -> ACons me AEmpty
    Null         -> AEmpty
    Sharp me₀    -> vs me₀
    Pair me₁ me₂ -> vs me₁ ⊕ vs me₂

vs₁ :: AMExpr -> ALst
vs₁ me = case me of
    Var _ _      -> ACons me AEmpty
    Null         -> AEmpty
    Sharp _      -> AEmpty
    Pair me₁ me₂ -> vs₁ me₁ ⊕ vs₁ me₂

vs₂ :: AMExpr -> ALst
vs₂ me = case me of
    Var _ _      -> AEmpty
    Null         -> AEmpty
    Sharp me₀    -> vs₂ me₀
    Pair me₁ me₂ -> vs₂ me₁ ⊕ vs₂ me₂

{-
>>> sample1 = l₁
>>> sample1
Lst (MExpr (Expr ○●●))
>>> sample2 = decode @(Lst (MExpr (Expr Hs))) @ALst sample1
>>> sample2
()
>>> sample1 == encode sample2
True
>>> sample2 == read (show sample2)
True
>>> sample3 = l₂ (m₁ e₁ e₁) l₁
>>> sample3
Lst (MExpr (Expr ○○●○●●○●●))

>>> sample4 = decode @(Lst (MExpr (Expr Hs))) @ALst sample3
>>> sample4
(Var ● ●,())
-}
