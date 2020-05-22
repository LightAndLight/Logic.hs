module Logic.Prelude where

import Logic

first :: Term -> Term -> Goal
first p output =
  fresh $ \l ->
  fresh $ \r ->
  p .= Pair (Var l) (Var r) .& output .= Var l

second :: Term -> Term -> Goal
second p output =
  fresh $ \l ->
  fresh $ \r ->
  p .= Pair (Var l) (Var r) .& output .= Var r

pair :: Term -> Term -> Term -> Goal
pair a b output =
  output .= Pair a b
