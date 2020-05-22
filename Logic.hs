module Logic
  ( Var, unsafeVar
  , Term(..)
  , Subst, listToSubst
  , State, project0
  , initialState
  , Goal, run, define
  , equal, (.=)
  , fresh
  , failure, disj, (.|), disjs
  , success, conj, (.&), conjs
  )
where

import qualified Data.List as List

newtype Var = MkVar Int deriving Eq

unsafeVar :: Int -> Var
unsafeVar = MkVar

instance Show Var where
  show (MkVar v) = "?" <> show v

data Term
  = Unit
  | Pair Term Term
  | Bool Bool
  | Sym String
  | Var Var
  deriving (Eq, Show)

newtype Subst = Subst [(Var, Term)]
  deriving (Eq, Show)

listToSubst :: [(Var, Term)] -> Subst
listToSubst = Subst

lookupVar :: Var -> Subst -> Maybe Term
lookupVar v (Subst s) = snd <$> List.find ((v ==) . fst) s

extend :: Var -> Term -> Subst -> Subst
extend v t (Subst s) = Subst $ (v, t) : s

applySubst :: Subst -> Term -> Term
applySubst s t =
  case t of
    Unit -> t
    Pair a b -> Pair (applySubst s a) (applySubst s b)
    Bool{} -> t
    Sym{} -> t
    Var v ->
      case lookupVar v s of
        Nothing -> t
        Just t' -> applySubst s t'

data State
  = State
  { counter :: Var
  , subst :: Subst
  } deriving (Eq, Show)

initialState :: State
initialState = State (MkVar 0) (Subst [])

project0 :: State -> Term
project0 (State _ s) =
  case lookupVar (MkVar 0) s of
    Nothing -> Var $ MkVar 0
    Just t -> applySubst s t

data Stream a
  = Nil
  | Cons a (Stream a)
  | Promise (Stream a)

streamToList :: Stream a -> [a]
streamToList s =
  case s of
    Nil -> []
    Cons a rest -> a : streamToList rest
    Promise s' -> streamToList s'

newtype Goal = Goal { runGoal :: State -> Stream State }

define :: ((a -> Goal) -> (a -> Goal)) -> a -> Goal
define f = go
  where
    go args =
      Goal $ \state ->
      Promise (runGoal (f go args) state)

run :: Maybe Int -> Goal -> [State]
run Nothing g = streamToList $ runGoal g initialState
run (Just limit) g = take limit . streamToList $ runGoal g initialState

find :: Subst -> Term -> Term
find s t =
  case t of
    Var n -> maybe t (find s) (lookupVar n s)
    _ -> t

occurs :: Var -> Term -> Bool
occurs v t =
  case t of
    Unit -> False
    Pair a b -> occurs v a || occurs v b
    Bool{} -> False
    Sym{} -> False
    Var v' -> v == v'

unify :: Subst -> Term -> Term -> Maybe Subst
unify s t1 t2 =
  case (t1, t2) of
    (Var v, Var v') | v == v' -> Just s
    (Var v, _) ->
      if occurs v t2
      then Nothing
      else Just $ extend v t2 s
    (_, Var v) ->
      if occurs v t1
      then Nothing
      else Just $ extend v t1 s
    (Unit, Unit) -> pure s
    (Pair a b, Pair a' b') -> do
      s' <- unify s a a'
      unify s' b b'
    (Sym a, Sym a') -> if a == a' then Just s else Nothing
    _ -> Nothing

equal :: Term -> Term -> Goal
equal t1 t2 =
  Goal $ \state ->
  let
    s = subst state
    m_s' = unify s (find s t1) (find s t2)
  in
    case m_s' of
      Nothing -> Nil
      Just s' -> Cons (state { subst = s' }) Nil

infix 6 .=
(.=) :: Term -> Term -> Goal
(.=) = equal

fresh :: (Var -> Goal) -> Goal
fresh k =
  Goal $ \state ->
  let
    MkVar v = counter state
  in
    runGoal (k $ MkVar v) (state { counter = MkVar (v+1) })

append :: Stream a -> Stream a -> Stream a
append as bs =
  case as of
    Nil -> bs
    Cons a rest -> Cons a (append rest bs)
    Promise as' -> Promise (append bs as')

disj :: Goal -> Goal -> Goal
disj a b =
  Goal $ \state ->
  append (runGoal a state) (runGoal b state)

infixr 4 .|
(.|) :: Goal -> Goal -> Goal
(.|) = disj

appendMap :: Stream a -> (a -> Stream b) -> Stream b
appendMap s f =
  case s of
    Nil -> Nil
    Cons a rest -> append (f a) (appendMap rest f)
    Promise s' -> Promise $ appendMap s' f

conj :: Goal -> Goal -> Goal
conj a b =
  Goal $ \state ->
  appendMap (runGoal a state) (runGoal b)

infixr 5 .&
(.&) :: Goal -> Goal -> Goal
(.&) = conj

success :: Goal
success = Goal $ \s -> Cons s Nil

conjs :: [Goal] -> Goal
conjs = foldr conj success

failure :: Goal
failure = Goal $ \_ -> Nil

disjs :: [Goal] -> Goal
disjs = foldr disj failure
