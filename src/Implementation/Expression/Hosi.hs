-- # Implementation.Expression.Hosi
-- 星および星取表
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
module Implementation.Expression.Hosi
    where

import Text.ParserCombinators.ReadP

-- ## 星

data Hosi
    = Kuro
    | Siro
    deriving (Eq)

instance Show Hosi where
    showsPrec :: Int -> Hosi -> ShowS
    showsPrec _ = \ case
        Kuro -> ('●' :)
        Siro -> ('○' :)

instance Read Hosi where
    readsPrec :: Int -> ReadS Hosi
    readsPrec _ = \ case
        "●" -> [(Kuro, "")]
        "○" -> [(Siro, "")]
        _   -> []

-- ## 星取表

type Hs = [Hosi]

instance {-# OVERLAPPING #-} Show Hs where
    showsPrec :: Int -> Hs -> ShowS
    showsPrec _ = \ case
        [] -> ('ε' :)
        hs -> foldr ((.) . showsPrec 0) id hs

instance {-# OVERLAPPING #-} Read Hs where
    readsPrec :: Int -> ReadS Hs
    readsPrec _ = \ case
        "ε"  -> [([], "")]
        hs   -> readP_to_S rHs hs

rHs :: ReadP Hs
rHs = many rHosi

rHosi :: ReadP Hosi
rHosi = rKuro +++ rSiro

rKuro :: ReadP Hosi
rKuro = pure Kuro <* char '●'

rSiro :: ReadP Hosi
rSiro = pure Siro <* char '○'

{-
>>> read @Hs "ε"
ε

>>> read @Hs "●"
●
>>> read @Hs "○"
○
>>> read @Hs "○●"
○●
-}

