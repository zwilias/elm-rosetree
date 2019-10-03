module Tree.DiffTest exposing (diffTest, sameTree)

import Expect
import Fuzz as F
import Test exposing (..)
import Tree exposing (Tree, singleton, tree)
import Tree.Diff exposing (Diff(..), Tail(..), diff)


diffTest : Test
diffTest =
    test "example" <|
        \_ ->
            diff
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
                    (Copy "root"
                        [ Copy "folder"
                            [ Keep (singleton "foo") ]
                            (Left [ singleton "bar" ])
                        , Replace
                            (singleton "yeah")
                            (tree "folder2" [ singleton "nice" ])
                        , Keep (singleton "keep me!")
                        ]
                        (Right [ singleton "add me" ])
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
            diff t t
                |> Expect.equal (Keep t)
