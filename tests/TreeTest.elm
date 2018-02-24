module TreeTest exposing (..)

import Expect
import Fuzz as F
import Test exposing (..)
import Tree exposing (Tree, tree)


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
                tree () (List.repeat childCount (tree () []))
                    |> Tree.count
                    |> Expect.equal (childCount + 1)
        , test "Count of hardcoded tree" <|
            \_ ->
                tree ()
                    [ tree () []
                    , tree () [ tree () [] ]
                    , tree () [ tree () [ tree () [], tree () [] ] ]
                    ]
                    |> Tree.count
                    |> Expect.equal 8
        ]


foldlTest : Test
foldlTest =
    test "foldl hardcoded tree" <|
        \_ ->
            tree 1
                [ tree 2 []
                , tree 3 [ tree 4 [] ]
                , tree 5 [ tree 6 [ tree 7 [], tree 8 [] ] ]
                ]
                |> Tree.foldl (::) []
                |> Expect.equal [ 1, 5, 6, 8, 7, 3, 4, 2 ]


foldrTest : Test
foldrTest =
    test "foldr hardcoded tree" <|
        \_ ->
            tree 1
                [ tree 2 []
                , tree 3 [ tree 4 [] ]
                , tree 5 [ tree 6 [ tree 7 [], tree 8 [] ] ]
                ]
                |> Tree.foldr (::) []
                |> Expect.equal [ 2, 4, 3, 7, 8, 6, 5, 1 ]


flattenTest : Test
flattenTest =
    test "flatten hardcoded tree" <|
        \_ ->
            tree 1
                [ tree 2 []
                , tree 3 [ tree 4 [] ]
                , tree 5 [ tree 6 [ tree 7 [], tree 8 [] ] ]
                ]
                |> Tree.flatten
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
                tree "a"
                    [ tree "b" []
                    , tree "c" []
                    ]
                    |> Tree.map String.toUpper
                    |> Expect.equal
                        (tree "A"
                            [ tree "B" []
                            , tree "C" []
                            ]
                        )
        , test "hardcoded tree can be mapped" <|
            \_ ->
                tree 1
                    [ tree 2 []
                    , tree 3 [ tree 4 [] ]
                    , tree 5 [ tree 6 [ tree 7 [], tree 8 [] ] ]
                    ]
                    |> Tree.map (\x -> x * 2)
                    |> Expect.equal
                        (tree 2
                            [ tree 4 []
                            , tree 6 [ tree 8 [] ]
                            , tree 10 [ tree 12 [ tree 14 [], tree 16 [] ] ]
                            ]
                        )
        ]


indexedMap : Test
indexedMap =
    test "Hardcoded tree can be indexed" <|
        \_ ->
            tree ()
                [ tree () []
                , tree () [ tree () [] ]
                , tree () []
                , tree () [ tree () [ tree () [], tree () [] ] ]
                ]
                |> Tree.indexedMap always
                |> Expect.equal
                    (tree
                        0
                        [ tree 1 []
                        , tree 2 [ tree 3 [] ]
                        , tree 4 []
                        , tree 5 [ tree 6 [ tree 7 [], tree 8 [] ] ]
                        ]
                    )
