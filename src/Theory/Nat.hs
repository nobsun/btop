-- # Theory.Nat
-- 自然数
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
module Theory.Nat where

import Numeric.Natural
import Data.Functor.Foldable
import Data.Type.Equality
import Data.Kind

import Theory.Sing

-- recursion schmes

data NatF r
    = ZF
    | SF r
    deriving (Eq, Functor)

data Nat
    = Z
    | S Nat
    deriving (Eq)

type instance Base Nat = NatF

instance Recursive Nat where
    project :: Nat -> Base Nat Nat
    project = \ case
        Z   -> ZF
        S n -> SF n

instance Corecursive Nat where
    embed :: Base Nat Nat -> Nat
    embed = \ case
        ZF -> Z
        SF n -> S n

--

data instance Sing (n :: Nat) where
    SZ :: Sing 'Z
    SS :: Sing n -> Sing (S n)

type SNat (n :: Nat) = Sing (n :: Nat)

instance SingKind Nat where
    type Demote Nat = Nat
    toSing :: Demote Nat -> SomeSing Nat
    toSing = \ case
        Z   -> SomeSing SZ
        S n -> withSomeSing (toSing n)
           $ \ sn -> SomeSing (SS sn)
    fromSing :: Sing (a :: Nat) -> Demote Nat
    fromSing = \ case
        SZ   -> Z
        SS n -> fromSing n

instance SingI 'Z where
    sing :: Sing Z
    sing = SZ

instance SingI n => SingI (S n) where
    sing :: SingI n => Sing (S n)
    sing = SS sing

type family (m :: Nat) + (n :: Nat) :: Nat where
    Z   + n = n
    S m + n = S (m + n)

leftUnit :: SNat (n :: Nat) -> Z + n :~: n
leftUnit _n = Refl

左単位元 :: SNat (n :: Nat) -> Z + n :~: n
左単位元 = leftUnit

rightUnit :: SNat (m :: Nat) -> m + Z :~: m
rightUnit m = case m of
    SZ   -> Refl
    SS n -> case rightUnit n of
        Refl -> Refl

右単位元 :: SNat (m :: Nat) -> m + Z :~: m
右単位元 = rightUnit

associativity :: SNat k -> SNat m -> SNat n
              -> k + (m + n) :~: (k + m) + n
associativity k m n = case k of
    SZ -> Refl
    SS k' -> case associativity k' m n of
        Refl  -> Refl

加法結合性 :: SNat k -> SNat m -> SNat n
           -> k + (m + n) :~: (k + m) + n
加法結合性 = associativity

succFunction :: SNat n -> S n :~: S Z + n
succFunction _n = Refl

succCommutativity :: SNat m -> SNat n -> (S m + n :~: m + S n)
succCommutativity m n = case m of
    SZ    -> Refl
    SS m' -> case succCommutativity m' n of
        Refl  -> Refl

commutativity :: SNat m -> SNat n -> m + n :~: n + m
commutativity m n = case m of
    SZ -> case rightUnit n of
        Refl -> Refl
    SS m' -> case commutativity m' n of
        Refl -> case succCommutativity n m' of
            Refl -> case n of
                SZ    -> Refl
                SS n' -> case succCommutativity n' m' of
                    Refl  -> Refl

加法可換性 :: SNat m -> SNat n -> m + n :~: n + m
加法可換性 = commutativity
