-- # Theory.List
-- リスト
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
module Theory.List
    where

import Data.Type.Equality
import Unsafe.Coerce
import Theory.Sing
import Theory.Nat

data instance Sing (a :: [k]) where
    SNil  :: Sing '[]
    (:%)  :: Sing (x  :: k)
          -> Sing (xs :: [k])
          -> Sing (x ': xs)

type SList (a :: [k]) = Sing (a :: [k])

instance (k ~ Demote k, SingKind k)
    => SingKind [k] where
    type Demote [k] = [k]
    toSing :: (k ~ Demote k, SingKind k) => Demote [k] -> SomeSing [k]
    toSing = \ case
        []   -> SomeSing SNil
        x:xs -> withSomeSing (toSing x) 
            $ \ sx  -> withSomeSing (toSing xs)
            $ \ sxs -> SomeSing (sx :% sxs)
    fromSing :: forall (a :: [k]). (k ~ Demote k, SingKind k)
             => Sing a -> Demote [k]
    fromSing = \ case
        SNil -> []
        sx :% sxs -> fromSing sx : fromSing sxs

--

(++%) :: SList a -> SList b -> SList (a ++ b)
SNil      ++% b = b
(a :% a') ++% b = a :% (a' ++% b)

--

shead :: SList (a ': as) -> Sing a
shead = \ case
    x :% _ -> x 

stail :: SList (a ': as) -> Sing as
stail = \ case
    _ :% xs -> xs

type family Length (α :: [a]) :: Nat where
    Length '[]       = Z
    Length (_ ': xs) = S (Length xs)

sLength :: SList α -> Nat
sLength SNil      = Z
sLength (_ :% xs) = S (sLength xs)

newtype Len (s :: [k]) (n :: Nat) = Len (Length s :~: n)

len :: SList α -> Σ (Len α)
len ss = case ss of
    SNil    -> Σ SZ (Len Refl)
    _ :% as -> case len as of
        Σ sn (Len Refl) -> Σ (SS sn) (Len Refl)

-- 

type family Take (n :: Nat) (α :: [a]) :: [a] where
    Take Z     _         = '[]
    Take _     '[]       = '[]
    Take (S n) (a ': α') = a ': Take n α'

type family Drop (n :: Nat) (α :: [a]) :: [a] where
    Drop Z     α         = α
    Drop _     '[]       = '[]
    Drop (S n) (_ ': α') = Drop n α'

--

type family (α :: [a]) ++ (β :: [a]) :: [a] where
    '[]      ++ β = β
    (a ': α) ++ β = a ': (α ++ β)

newtype Juxtaposition α β γ = Juxtaposition (α ++ β :~: γ, α :~: Take (Length α) γ, β :~: Drop (Length α) γ)

(++%%), 連接:: SList α -> SList β -> Σ (Juxtaposition α β)
a ++%% b = case a of
    SNil    -> Σ b (Juxtaposition (Refl, Refl, Refl))
    x :% xs -> case xs ++%% b of
        Σ ys (Juxtaposition (Refl, Refl, Refl)) -> Σ (x :% ys) (Juxtaposition (Refl, Refl, Refl))

連接 = (++%%)

juxtapositionLeftUnit, 連接左単位元 :: SList β -> ('[] ++ β :~: β)
juxtapositionLeftUnit _ = Refl

連接左単位元 = juxtapositionLeftUnit

juxtapositionRightUnit, 連接右単位元 :: SList α -> (α ++ '[] :~: α)
juxtapositionRightUnit α = case α of
    SNil    -> Refl
    _ :% α' -> case juxtapositionRightUnit α' of
        Refl    -> Refl

連接右単位元 = juxtapositionRightUnit

associativityOfJuxtaposition, 連接結合律 :: SList α -> SList β -> SList γ
                             -> (α ++ (β ++ γ) :~: (α ++ β) ++ γ)
associativityOfJuxtaposition a b c
    = case a of
        SNil   -> Refl
        _ :% t -> case associativityOfJuxtaposition t b c of
            Refl      -> Refl

連接結合律 = associativityOfJuxtaposition

naturalityOfLength, 長さの自然性 :: SList α -> SList β -> Length (α ++ β) :~: (Length α + Length β)
naturalityOfLength a b = case a of
    SNil       -> Refl
    _ :% a' -> case naturalityOfLength a' b of
        Refl       -> Refl

長さの自然性 = naturalityOfLength

leftCancelationOfJuxtaposition, 連接左簡約律 :: SList α -> SList β  -> SList γ
                -> (α ++ β :~: α ++ γ) -> (β :~: γ)
leftCancelationOfJuxtaposition a b c Refl
    = case a of
        SNil       -> Refl
        _ :% a' -> case leftCancelationOfJuxtaposition a' b c Refl of
            Refl       -> Refl
連接左簡約律 = leftCancelationOfJuxtaposition

-- 両立性 Compatibility

newtype Compat α β α' β' = Compat (α ++ α' :~: β ++ β')
type Compatible α β = Σ₂ (Compat α β)
