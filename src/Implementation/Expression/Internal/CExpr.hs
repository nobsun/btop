-- # Implementation.Expression.Internal.CExpr
-- C式 (Closed Expression)
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
module Implementation.Expression.Internal.CExpr
    where

import Data.Functor.Foldable
import Text.ParserCombinators.ReadP
import Implementation.Expression.Hosi
import Implementation.Expression.Internal.Decode
import Implementation.Expression.Internal.TreeLike
import Implementation.Expression.Internal.Expr
import Implementation.Expression.Internal.MExpr

-- ## C式

newtype CExpr a = CExpr a deriving (Eq, Show)

instance TreeLike (CExpr (MExpr (Expr Hs))) where
    branches :: CExpr (MExpr (Expr Hs)) -> [CExpr (MExpr (Expr Hs))]
    branches = \ case
        CExpr me -> CExpr <$> branches me

c₁ :: CExpr (MExpr (Expr Hs))
c₁ = CExpr m₂

c₂ :: CExpr (MExpr (Expr Hs)) -> CExpr (MExpr (Expr Hs))
c₂ (CExpr me) = CExpr (m₃ me)

c₃ :: CExpr (MExpr (Expr Hs)) -> CExpr (MExpr (Expr Hs)) -> CExpr (MExpr (Expr Hs))
c₃ (CExpr me) (CExpr ne) = CExpr (m₄ me ne)

-- ## C式のAST

data ACExprF r
    = CNullF
    | CSharpF r
    | CPairF r r
    deriving (Eq, Functor)

data ACExpr
    = CNull
    | CSharp ACExpr
    | CPair ACExpr ACExpr
    deriving (Eq)

type instance Base ACExpr = ACExprF

instance Recursive ACExpr where
    project = \ case
        CNull     -> CNullF
        CSharp t  -> CSharpF t
        CPair s t -> CPairF s t

instance Corecursive ACExpr where
    embed = \ case
        CNullF     -> CNull
        CSharpF t  -> CSharp t
        CPairF s t -> CPair s t

instance TreeLike ACExpr where
    branches = \ case
        CNull     -> []
        CSharp t  -> [t]
        CPair s t -> [s, t]

instance Decode (CExpr (MExpr (Expr Hs))) ACExpr where
    encode :: ACExpr -> CExpr (MExpr (Expr Hs))
    encode = cata phi
        where
            phi = \ case
                CNullF     -> c₁
                CSharpF t  -> c₂ t
                CPairF s t -> c₃ s t

    decode :: CExpr (MExpr (Expr Hs)) -> ACExpr
    decode = ana psi
        where
            psi ce = case branches ce of
                []    -> CNullF
                [t]   -> CSharpF t
                [s,t] -> CPairF s t
                _     -> error $ "decode @(CExpr (MExpr (Expr Hs))) @ACExpr: impossible"

instance Show ACExpr where
    showsPrec :: Int -> ACExpr -> ShowS
    showsPrec _ = cata phi
        where
            phi = \ case
                CNullF     -> showString "()"
                CSharpF t  -> showString "#(" . t . showString ")"
                CPairF s t -> showParen True (s . showChar '.' . t)

instance Read ACExpr where
    readsPrec :: Int -> ReadS ACExpr
    readsPrec _ = readP_to_S rACExpr

rACExpr :: ReadP ACExpr
rACExpr = rCNull +++ rCSharp +++ rCPair

rCNull :: ReadP ACExpr
rCNull = CNull <$ string "()"

rCSharp :: ReadP ACExpr
rCSharp = CSharp <$> (string "#(" *> rACExpr <* string ")")

rCPair :: ReadP ACExpr
rCPair = CPair <$> (string "(" *> rACExpr <* char '.') <*> rACExpr <* char ')'

{- ^
>>> sample1 = c₁
>>> sample1
CExpr (MExpr (Expr ○●●))
>>> sample2 = decode @(CExpr (MExpr (Expr Hs))) @ACExpr sample1
>>> sample2
()
>>> sample1 == encode sample2
True
>>> sample2 == read (show sample2)
True

>>> sample3 = c₂ sample1
>>> sample3
CExpr (MExpr (Expr ○○●●●))
>>> sample4 = decode @(CExpr (MExpr (Expr Hs))) @ACExpr sample3
>>> sample4
#(())
>>> sample3 == encode sample4
True
>>> sample4 == read (show sample4)
True
>>> sample5 = c₃ sample1 sample3
>>> sample5
CExpr (MExpr (Expr ○○●●○○●●●))
>>> sample6 = decode @(CExpr (MExpr (Expr Hs))) @ACExpr sample5
>>> sample6
(().#(()))
>>> sample5 == encode sample6
True
>>> sample6 == read (show sample6)
True
-}
