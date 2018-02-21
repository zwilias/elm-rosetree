module TreeTest exposing (..)

import Expect
import Fuzz as F
import Test exposing (..)
import Tree exposing (Tree(..))


lengthTest : Test
lengthTest =
    describe "count"
        [ test "count of singleton is 1" <|
            \_ ->
                Tree.singleton ()
                    |> Tree.count
                    |> Expect.equal 1
        , fuzz (F.intRange 0 100) "count of flat tree is children + 1" <|
            \childCount ->
                Tree () (List.repeat childCount (Tree () []))
                    |> Tree.count
                    |> Expect.equal (childCount + 1)
        , test "Count of hardcoded tree" <|
            \_ ->
                Tree ()
                    [ Tree () []
                    , Tree () [ Tree () [] ]
                    , Tree () [ Tree () [ Tree () [], Tree () [] ] ]
                    ]
                    |> Tree.count
                    |> Expect.equal 8
        ]


foldlTest : Test
foldlTest =
    test "foldl hardcoded tree" <|
        \_ ->
            Tree 1
                [ Tree 2 []
                , Tree 3 [ Tree 4 [] ]
                , Tree 5 [ Tree 6 [ Tree 7 [], Tree 8 [] ] ]
                ]
                |> Tree.foldl (::) []
                |> Expect.equal [ 1, 5, 6, 8, 7, 3, 4, 2 ]


foldrTest : Test
foldrTest =
    test "foldr hardcoded tree" <|
        \_ ->
            Tree 1
                [ Tree 2 []
                , Tree 3 [ Tree 4 [] ]
                , Tree 5 [ Tree 6 [ Tree 7 [], Tree 8 [] ] ]
                ]
                |> Tree.foldr (::) []
                |> Expect.equal [ 2, 4, 3, 7, 8, 6, 5, 1 ]


mapTest : Test
mapTest =
    describe "map"
        [ test "singleton can be mapped" <|
            \_ ->
                Tree.singleton "a"
                    |> Tree.map String.toUpper
                    |> Expect.equal (Tree.singleton "A")
        , test "flat tree can be mapped" <|
            \_ ->
                Tree "a"
                    [ Tree "b" []
                    , Tree "c" []
                    ]
                    |> Tree.map String.toUpper
                    |> Expect.equal
                        (Tree "A"
                            [ Tree "B" []
                            , Tree "C" []
                            ]
                        )
        , test "hardcoded tree can be mapped" <|
            \_ ->
                Tree 1
                    [ Tree 2 []
                    , Tree 3 [ Tree 4 [] ]
                    , Tree 5 [ Tree 6 [ Tree 7 [], Tree 8 [] ] ]
                    ]
                    |> Tree.map (\x -> x * 2)
                    |> Expect.equal
                        (Tree 2
                            [ Tree 4 []
                            , Tree 6 [ Tree 8 [] ]
                            , Tree 10 [ Tree 12 [ Tree 14 [], Tree 16 [] ] ]
                            ]
                        )
        ]


indexedMap : Test
indexedMap =
    test "Hardcoded tree can be indexed" <|
        \_ ->
            Tree ()
                [ Tree () []
                , Tree () [ Tree () [] ]
                , Tree () []
                , Tree () [ Tree () [ Tree () [], Tree () [] ] ]
                ]
                |> Tree.indexedMap always
                |> Expect.equal
                    (Tree
                        0
                        [ Tree 1 []
                        , Tree 2 [ Tree 3 [] ]
                        , Tree 4 []
                        , Tree 5 [ Tree 6 [ Tree 7 [], Tree 8 [] ] ]
                        ]
                    )
