module Tree.ZipperTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


treeOfInitIsSelf : Test
treeOfInitIsSelf =
    test "fromTree followed by tree is identity" <|
        \_ ->
            Zipper.fromTree tree
                |> Zipper.tree
                |> Expect.equal tree


rootOfAnyMotionIsSame : Test
rootOfAnyMotionIsSame =
    fuzz motions "root after a series of motions is always the same" <|
        \m ->
            Zipper.fromTree tree
                |> applyMotions m
                |> Zipper.root
                |> Zipper.tree
                |> Expect.equal tree


lastDescendant : Test
lastDescendant =
    test "lastDescendant works out" <|
        \_ ->
            Zipper.fromTree tree
                |> Zipper.lastDescendant
                |> Zipper.tree
                |> Expect.equal (Tree.singleton 99)


backward : Test
backward =
    test "backward from lastDescendant" <|
        \_ ->
            Zipper.fromTree tree
                |> Zipper.lastDescendant
                |> Zipper.backward
                |> Maybe.map Zipper.tree
                |> Expect.equal (Just <| Tree.singleton 7)


find : Test
find =
    describe "find"
        [ test "find finds the thing" <|
            \_ ->
                Zipper.fromTree tree
                    |> Zipper.findNext (\t -> t == 99)
                    |> Maybe.map Zipper.tree
                    |> Expect.equal (Just <| Tree.singleton 99)
        , test "expect when it's not there" <|
            \_ ->
                Zipper.fromTree tree
                    |> Zipper.findNext ((==) 100)
                    |> Expect.equal Nothing
        , test "searches within the focus" <|
            \_ ->
                Zipper.fromTree tree
                    |> Zipper.lastChild
                    |> Maybe.andThen (Zipper.findNext ((==) 2))
                    |> Maybe.map Zipper.tree
                    |> Expect.equal (Just <| Tree.singleton 2)
        ]


findFromRoot : Test
findFromRoot =
    test "Searches from root" <|
        \_ ->
            let
                expectedResult : Tree Int
                expectedResult =
                    Tree.tree 2
                        [ Tree.singleton 1
                        , Tree.singleton 2
                        , Tree.singleton 3
                        ]
            in
            Zipper.fromTree tree
                |> Zipper.lastChild
                |> Maybe.andThen (Zipper.findFromRoot ((==) 2))
                |> Maybe.map Zipper.tree
                |> Expect.equal (Just expectedResult)


motions : Fuzzer (List (Motion a))
motions =
    Fuzz.list <|
        Fuzz.oneOf <|
            List.map Fuzz.constant
                [ Zipper.forward
                , Zipper.backward
                , Zipper.firstChild
                , Zipper.lastChild
                , Zipper.parent
                , Zipper.nextSibling
                , Zipper.previousSibling
                ]


type alias Motion a =
    Zipper a -> Maybe (Zipper a)


applyMotions : List (Motion a) -> Zipper a -> Zipper a
applyMotions motions zipper =
    case motions of
        [] ->
            zipper

        m :: rest ->
            applyMotions rest (Maybe.withDefault zipper (m zipper))


tree : Tree Int
tree =
    Tree.tree 5
        [ Tree.singleton 10
        , Tree.tree 2
            [ Tree.tree 1 []
            , Tree.tree 2 []
            , Tree.tree 3 []
            ]
        , Tree.singleton 20
        , Tree.tree 3
            [ Tree.singleton 2
            , Tree.tree 4 [ Tree.singleton 7 ]
            , Tree.singleton 99
            ]
        ]
