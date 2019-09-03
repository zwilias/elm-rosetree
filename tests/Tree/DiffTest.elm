module Tree.DiffTest exposing (diffTest, sameTree)

import Expect
import Fuzz as F
import Test exposing (..)
import Tree exposing (Tree, singleton, tree)
import Tree.Diff exposing (Diff(..), Patch(..), diff)


diffTest : Test
diffTest =
    test "example" <|
        \_ ->
            diff (==)
                (tree "root"
                    [ tree "folder"
                        [ singleton "foo"
                        , singleton "bar"
                        ]
                    , singleton "yeah"
                    , singleton "keep me!"
                    ]
                )
                (tree "root"
                    [ tree "folder"
                        [ singleton "foo" ]
                    , tree "folder2"
                        [ singleton "nice" ]
                    , singleton "keep me!"
                    , singleton "add me"
                    ]
                )
                |> Expect.equal
                    (Patch
                        (Copy "root"
                            [ Patch
                                (Copy "folder"
                                    [ Keep (singleton "foo")
                                    , Patch (Delete (singleton "bar"))
                                    ]
                                )
                            , Patch
                                (Replace
                                    (singleton "yeah")
                                    (tree "folder2" [ singleton "nice" ])
                                )
                            , Keep (singleton "keep me!")
                            , Patch (Insert (singleton "add me"))
                            ]
                        )
                    )


sameTree : Test
sameTree =
    test "Same as left and right gives Keep" <|
        \_ ->
            let
                f : Int -> ( Int, List Int )
                f x =
                    ( x, List.range 0 (x - 1) )

                t : Tree Int
                t =
                    Tree.unfold f 5
            in
            diff (==) t t
                |> Expect.equal (Keep t)
