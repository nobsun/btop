-- # Theory.Expression.Hosi
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
module Theory.Expression.Hosi
    where

import Implementation.Expression.Hosi
import Theory.Sing

data instance Sing (h :: Hosi) where
    SKuro :: Sing 'Kuro
    SSiro :: Sing 'Siro

type SHosi (h :: Hosi) = Sing (h :: Hosi)

instance SingKind Hosi where
    type Demote Hosi = Hosi
    toSing :: Demote Hosi -> SomeSing Hosi
    toSing = \ case
        Kuro -> SomeSing SKuro
        Siro -> SomeSing SSiro
    fromSing :: Sing (h :: Hosi) -> Demote Hosi
    fromSing = \ case
        SKuro -> Kuro
        SSiro -> Siro

instance SingI 'Kuro where
    sing :: Sing 'Kuro
    sing = SKuro

instance SingI 'Siro where
    sing :: Sing 'Siro
    sing = SSiro

