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
                |> Expect.equal [ 8, 7, 6, 5, 4, 3, 2, 1 ]


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
                |> Expect.equal [ 1, 2, 3, 4, 5, 6, 7, 8 ]


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
                |> Expect.equal [ 1, 2, 3, 4, 5, 6, 7, 8 ]


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


unfold : Test
unfold =
    describe "unfold"
        [ test "simple unfold" <|
            \_ ->
                Tree.unfold (always ( "foo", [] )) ()
                    |> Expect.equal (Tree.singleton "foo")
        , test "deeper unfold" <|
            \_ ->
                let
                    f : Int -> ( Int, List Int )
                    f x =
                        ( x, List.range 0 (x - 1) )
                in
                Tree.unfold f 4
                    |> Expect.equal
                        (Tree.tree 4
                            [ Tree.singleton 0
                            , Tree.tree 1 [ Tree.singleton 0 ]
                            , Tree.tree 2
                                [ Tree.singleton 0
                                , Tree.tree 1 [ Tree.singleton 0 ]
                                ]
                            , Tree.tree 3
                                [ Tree.singleton 0
                                , Tree.tree 1 [ Tree.singleton 0 ]
                                , Tree.tree 2
                                    [ Tree.singleton 0
                                    , Tree.tree 1 [ Tree.singleton 0 ]
                                    ]
                                ]
                            ]
                        )
        ]


map2 : Test
map2 =
    describe "map2"
        [ test "simple matching trees" <|
            \_ ->
                let
                    left : Tree Int
                    left =
                        Tree.tree 1
                            [ Tree.singleton 2
                            , Tree.singleton 3
                            , Tree.singleton 4
                            ]

                    right : Tree Int
                    right =
                        Tree.tree 2
                            [ Tree.singleton 3
                            , Tree.singleton 4
                            , Tree.singleton 5
                            ]

                    expected : Tree Int
                    expected =
                        Tree.tree 3
                            [ Tree.singleton 5
                            , Tree.singleton 7
                            , Tree.singleton 9
                            ]
                in
                Tree.map2 (+) left right
                    |> Expect.equal expected
        , test "shorter left is shorter output" <|
            \_ ->
                let
                    left : Tree Int
                    left =
                        Tree.tree 1
                            [ Tree.singleton 2
                            , Tree.singleton 3
                            , Tree.singleton 4
                            ]

                    right : Tree Int
                    right =
                        Tree.tree 2
                            [ Tree.singleton 3
                            , Tree.singleton 4
                            , Tree.tree 5
                                [ Tree.singleton 6 ]
                            ]

                    expected : Tree Int
                    expected =
                        Tree.tree 3
                            [ Tree.singleton 5
                            , Tree.singleton 7
                            , Tree.singleton 9
                            ]
                in
                Tree.map2 (+) left right
                    |> Expect.equal expected
        , test "shorter right is shorter output" <|
            \_ ->
                let
                    left : Tree Int
                    left =
                        Tree.tree 1
                            [ Tree.singleton 2
                            , Tree.singleton 3
                            , Tree.tree 4
                                [ Tree.singleton 5
                                , Tree.singleton 6
                                ]
                            , Tree.singleton 7
                            ]

                    right : Tree Int
                    right =
                        Tree.tree 2
                            [ Tree.singleton 3
                            , Tree.singleton 4
                            , Tree.singleton 5
                            ]

                    expected : Tree Int
                    expected =
                        Tree.tree 3
                            [ Tree.singleton 5
                            , Tree.singleton 7
                            , Tree.singleton 9
                            ]
                in
                Tree.map2 (+) left right
                    |> Expect.equal expected
        ]


indexedMap2 : Test
indexedMap2 =
    test "Indexed map2 receives all the things" <|
        \_ ->
            let
                f : Int -> ( Int, List Int )
                f x =
                    ( x, List.range 0 (x - 1) )

                g : Int -> ( String, List Int )
                g x =
                    f x |> Tuple.mapFirst toString

                h : Int -> ( ( Int, String ), List Int )
                h x =
                    ( ( x, toString x ), List.range 0 (x - 1) )

                left : Tree Int
                left =
                    Tree.unfold f 5

                right : Tree String
                right =
                    Tree.unfold g 5

                expected : Tree ( Int, Int, String )
                expected =
                    Tree.unfold h 5
                        |> Tree.indexedMap (\n ( i, s ) -> ( n, i, s ))
            in
            Tree.indexedMap2 (\n i s -> ( n, i, s )) left right
                |> Expect.equal expected
