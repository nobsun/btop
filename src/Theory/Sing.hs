-- # Theory.Sing
-- Singleton Library
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

module Theory.Sing where

-- ## import 宣言
--
import Data.Constraint
import Data.Kind (Type)
import Data.Maybe
import Data.Typeable
import Data.Void
import Unsafe.Coerce

-- ### データ型および族
data family Sing (a :: k)

data SomeSing k where
    SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing
    :: SomeSing k
    -> (forall (a :: k). Sing a -> r)
    -> r
withSomeSing (SomeSing s) f = f s

-- ## シングルトン関連クラス

class SingKind k where
    type Demote k = r | r -> k
    toSing :: Demote k -> SomeSing k
    fromSing :: Sing (a :: k) -> Demote k

class SingI (a :: k) where
    sing :: Sing a

class SDecide k where
    (%~) :: Sing (a :: k)
         -> Sing (b :: k)
         -> Decision (a :~: b)

data Decision a
    = Proved a
    | Disproved (a -> Void)

instance (Eq (Demote k), SingKind k) => SDecide k where
    (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)
    a %~ b =
        if fromSing a == fromSing b
            then Proved $ unsafeCoerce Refl
            else Disproved $ const undefined

-- ## 依存対
data Σ (f :: k -> Type) where
    Σ :: Sing a -> f a -> Σ f

withΣ :: (forall (a :: k). Sing a -> f a -> r)
          -> Σ f
          -> r
withΣ c (Σ s f) = c s f

toΣ :: SingI a
        => f a
        -> Σ f
toΣ fa = Σ sing fa

fromΣ :: forall k (a :: k) (f :: k -> Type). (SingI a, SDecide k)
          => Σ f
          -> Maybe (f a)
fromΣ (Σ s f)
    = case s %~ (sing :: Sing a) of
        Proved Refl -> Just f
        Disproved _ -> Nothing

data Σ₂ (f :: k₁ -> k₂ -> Type) where
    Σ₂ :: Sing a -> Sing b -> f a b -> Σ₂ f

withΣ₂ :: (forall (a :: k₁) (b :: k₂). Sing a -> Sing b -> f a b -> r)
          -> Σ₂ f
          -> r
withΣ₂ c (Σ₂ s₁ s₂ f) = c s₁ s₂ f

toΣ₂ :: (SingI a, SingI b)
    => f a b -> Σ₂ f
toΣ₂ fab = Σ₂ sing sing fab

fromΣ₂ :: forall k₁ k₂ (a :: k₁) (b :: k₂) (f :: k₁ -> k₂ -> Type). 
    (SingI a, SingI b, SDecide k₁, SDecide k₂)
    => Σ₂ f -> Maybe (f a b)
fromΣ₂ (Σ₂ s₁ s₂ f) = case (s₁ %~ (sing :: Sing a), s₂ %~ (sing :: Sing b)) of
    (Proved Refl, Proved Refl) -> Just f
    _                          -> Nothing

catΣs
    :: forall k (a :: k) f
     . (SingI a, SDecide k)
    => [Σ f]
    -> [f a]
catΣs = mapMaybe fromΣ

-- ## 辞書クラス

type Dict1
    :: forall k
     . (Type -> Constraint)
    -> (k -> Type)
    -> Constraint
    
class Dict1 c f where
  dict1 :: Sing a -> Dict (c (f a))

instance ( Dict1 Show (f :: k -> Type)
         , Show (Demote k)
         , SingKind k
         ) => Show (Σ f) where
  show :: (Dict1 Show f, Show (Demote k), SingKind k)
       => Σ f -> String
  show (Σ sa fa) =
    case dict1 @_ @Show @f sa of
      Dict -> mconcat
        [ "Σ "
        , show $ fromSing sa
        , " ("
        , show fa
        , ")"
        ]

instance ( Dict1 Eq (f :: k -> Type)
         , SDecide k
         ) => Eq (Σ f) where
  (==) :: (Dict1 Eq f, SDecide k)
       => Σ f -> Σ f -> Bool
  Σ sa fa == Σ sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @_ @Eq @f sa of
          Dict -> fa == fb
      Disproved _ -> False

instance ( Dict1 Eq  (f :: k -> Type)
         , Dict1 Ord f
         , SDecide k
         , SingKind k
         , Ord (Demote k)
         ) => Ord (Σ f) where
  compare :: (Dict1 Eq f, Dict1 Ord f, SDecide k, SingKind k, Ord (Demote k))
          => Σ f -> Σ f -> Ordering
  compare (Σ sa fa) (Σ sb fb) =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @_ @Ord @f sa of
          Dict -> compare fa fb
      Disproved _ ->
        compare (fromSing sa) (fromSing sb)
