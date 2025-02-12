-- # Implementation.Expression.Subst
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
module Implementation.Expression.Subst
    where

import Data.Bool (bool)
import Implementation.Expression.Expr


replace :: AMExpr -> AVariable -> AMExpr -> AMExpr
replace s x t = case s of
    Var y    -> bool s t (x == y)
    Null     -> s
    Sharp _  -> s
    Pair a b -> Pair (replace a x t) (replace b x t)

substitute :: AMExpr -> AVariable -> AMExpr -> AMExpr
substitute s x t = replace s x (Sharp t)

