module Tests where

import Data.Foldable (traverse_)

import Logic

expectEqual :: (Eq a, Show a) => a -> a -> IO Bool
expectEqual actual expected =
  if actual == expected
  then pure True
  else do
    putStrLn $
      "Expected: " <> show expected <> "\n\n" <>
      "Got: " <> show actual <> "\n"
    pure False

expectAll :: [IO Bool] -> IO Bool
expectAll = fmap and . sequence

runTests :: IO ()
runTests =
  traverse_
    (\(n, val) -> do
        putStrLn n
        res <- val
        putStrLn $ if res then "passed\n" else "failed\n"
    )
    [ ( "fresh_1"
      , do
          let actual = project0 <$> run Nothing (fresh $ \x -> equal (Var x) (Sym "a"))
          let expected = [ Sym "a" ]
          expectEqual actual expected
      )
    , ( "peano"
      , do
          let
            peano =
              define $ \self n ->
              disj
                (equal n $ Sym "z")
                (fresh $ \r ->
                 conj
                   (equal n (Pair (Sym "s") (Var r)))
                   (self (Var r))
                )
          let actual = project0 <$> run (Just 2) (fresh $ \n -> peano (Var n))
          let
            expected =
              [ Sym "z"
              , Pair (Sym "s") (Sym "z")
              ]
          expectEqual actual expected
      )
    , ( "disj unproductive peano"
      , do
          let
            unproductive = define $ \self args -> self args
            peano =
              define $ \self n ->
              disj
                (equal n $ Sym "z")
                (fresh $ \r ->
                 conj
                   (equal n (Pair (Sym "s") (Var r)))
                   (self (Var r))
                )
          let actual = project0 <$> run (Just 2) (fresh $ \n -> disj (unproductive (Var n)) (peano (Var n)))
          let
            expected =
              [ Sym "z"
              , Pair (Sym "s") (Sym "z")
              ]
          expectEqual actual expected
      )
    , ( "append"
      , do
          let
            append =
              define $ \self (a, b, output) ->
              disjs
                [ a .= Unit .& output .= b
                , fresh $ \x ->
                  fresh $ \xs ->
                  a .= Pair (Var x) (Var xs) .&
                  fresh
                    (\output' ->
                     output .= Pair (Var x) (Var output') .&
                     self (Var xs, b, Var output')
                    )
                ]

          expectAll
            [ expectEqual
                (fmap project0 . run Nothing $
                 fresh $ \output ->
                 append (Pair (Sym "a") (Pair (Sym "b") Unit), Pair (Sym "c") (Pair (Sym "d") Unit), Var output)
                )
                [ Pair (Sym "a") (Pair (Sym "b") (Pair (Sym "c") (Pair (Sym "d") Unit))) ]
            , expectEqual
                (fmap project0 . run Nothing $
                 fresh $ \l ->
                 append
                   ( Var l
                   , Pair (Sym "c") (Pair (Sym "d") Unit)
                   , Pair (Sym "a") (Pair (Sym "b") (Pair (Sym "c") (Pair (Sym "d") Unit)))
                   )
                 )
                 [ Pair (Sym "a") (Pair (Sym "b") Unit) ]
            , expectEqual
                (fmap project0 . run Nothing $
                 fresh $ \r ->
                 append
                   ( Pair (Sym "a") (Pair (Sym "b") Unit)
                   , Var r
                   , Pair (Sym "a") (Pair (Sym "b") (Pair (Sym "c") (Pair (Sym "d") Unit)))
                   )
                 )
                 [ Pair (Sym "c") (Pair (Sym "d") Unit) ]
            ]
      )
    ]
