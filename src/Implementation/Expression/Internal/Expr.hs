-- # Implementation.Expression.Internal.Expr
-- 式
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
module Implementation.Expression.Internal.Expr
    where

import Text.ParserCombinators.ReadP
import Data.Functor.Foldable
import Implementation.Decode
import Implementation.TreeLike
import Implementation.Expression.Hosi

-- ## 式

newtype Expr a = Expr a deriving (Eq, Show)

instance TreeLike (Expr Hs) where
    branches :: Expr Hs -> [Expr Hs]
    branches = \ case
        Expr hs -> case hs of
            Kuro:[] -> []
            Siro:rs -> case iter (0 :: Int) (0 :: Int) [] rs of
                (s,t)   -> Expr <$> [s, t]
                where
                    iter k s as = \ case
                        []      -> error "branches.iter: impossible"
                        bbs@(b:bs) | k == succ s -> (reverse as, bbs)
                                | otherwise   -> case b of
                                    Kuro -> iter (succ k) s (b:as) bs
                                    Siro -> iter k (succ s) (b:as) bs
            _ -> error "branches: impossible"

e₁ :: Expr Hs
e₁ = Expr [Kuro]

e₂ :: Expr Hs -> Expr Hs -> Expr Hs
e₂ (Expr s) (Expr t) = Expr ([Siro] ++ s ++ t)

-- ## 式のAST

data AExprF r
    = LeafF
    | BranchF r r
    deriving (Eq, Functor)

data AExpr
    = Leaf
    | Branch AExpr AExpr
    deriving (Eq)

type instance Base AExpr = AExprF

instance Recursive AExpr where
    project :: AExpr -> Base AExpr AExpr
    project = \ case
        Leaf       -> LeafF
        Branch s t -> BranchF s t

instance Corecursive AExpr where
    embed :: Base AExpr AExpr -> AExpr
    embed = \ case
        LeafF       -> Leaf
        BranchF s t -> Branch s t

instance Decode (Expr Hs) AExpr where
    encode :: AExpr -> Expr Hs
    encode = cata phi
        where
            phi = \ case
                LeafF       -> e₁
                BranchF s t -> e₂ s t

    decode :: Expr Hs -> AExpr
    decode = ana psi
        where
            psi e = case branches e of
                []    -> LeafF
                [s,t] -> BranchF s t
                _     -> error $ "abstractExpr: impossible"

instance TreeLike AExpr where
    branches :: AExpr -> [AExpr]
    branches = \ case
        Leaf       -> []
        Branch s t -> [s,t]

instance Show AExpr where
    showsPrec :: Int -> AExpr -> ShowS
    showsPrec _ = cata phi
        where
            phi = \ case
                LeafF       -> ('●' :)
                BranchF s t -> showParen True (s . ('.' :) . t)

instance Read AExpr where
    readsPrec :: Int -> ReadS AExpr
    readsPrec _ = readP_to_S rAExpr

rAExpr :: ReadP AExpr
rAExpr = rLeaf +++ rBranch

rLeaf :: ReadP AExpr
rLeaf = pure Leaf <* char '●'

rBranch :: ReadP AExpr
rBranch = uncurry Branch <$> between (char '(') (char ')') ((,) <$> rAExpr <* char '.' <*> rAExpr)

{- ^
>>> sample1 = e₁
>>> sample1
Expr ●
>>> sample2 = e₂ e₁ e₁
>>> sample2
Expr ○●●
>>> sample3 = decode sample1 :: AExpr
>>> sample3
●
>>> sample4 = decode sample2 :: AExpr
>>> sample4
(●.●)
>>> sample5 = e₂ sample1 sample2
>>> sample5
Expr ○●○●●
>>> sample6 = decode sample5 :: AExpr
>>> sample6
(●.(●.●))
>>> sample7 = e₂ sample2 sample1
>>> sample7
Expr ○○●●●
>>> sample8 = decode sample7 :: AExpr
>>> sample8
((●.●).●)
>>> sample9 = read @AExpr $ show @AExpr $ decode sample1
>>> sample9
●
>>> sample9 == sample3
True
>>> sample10 = encode sample9 :: Expr Hs
>>> sample10 == sample1
True
>>> sample11 = read @AExpr $ show @AExpr sample6
>>> sample11 == sample6
True
>>> sample12 = encode $ read @AExpr $ show @AExpr $ decode sample5
>>> sample12 == sample5
True
-}

-- ## 変数

newtype Variable a = Variable a deriving (Eq, Show)

variable :: Expr Hs -> Expr Hs -> Variable (Expr Hs)
variable s t = Variable (e₂ e₁ (e₂ s t))

data AVariable = AVariable AExpr AExpr deriving (Eq)

instance Decode (Variable (Expr Hs)) AVariable where
    encode :: AVariable -> Variable (Expr Hs)
    encode = \ case
        AVariable s t -> variable (encode s) (encode t)

    decode :: Variable (Expr Hs) -> AVariable
    decode = \ case
        Variable e -> case branches e of
            [_,t]      -> case branches t of
                [ss, ts] -> AVariable (decode ss) (decode ts)
                _        -> error "decode: impossible"
            _ -> error "decode: impossible"

instance Show AVariable where
    showsPrec :: Int -> AVariable -> ShowS
    showsPrec _ = \ case
        AVariable s t -> shows s . showChar 'V' . shows t

instance Read AVariable where
    readsPrec :: Int -> ReadS AVariable
    readsPrec _ = readP_to_S rAVariable

rAVariable :: ReadP AVariable
rAVariable = uncurry AVariable <$> between (char '(') (char ')') ((,) <$> rAExpr <* char 'V' <*> rAExpr)
