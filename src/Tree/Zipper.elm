module Tree.Zipper
    exposing
        ( Crumb
        , Zipper
        , backward
        , current
        , find
        , findFromRoot
        , firstChild
        , forward
        , init
        , lastChild
        , lastDescendant
        , nextSibling
        , parent
        , previousSibling
        , root
        )

{-| TODO: docs


# Structure

@docs Zipper, Crumb, init, current


# Navigation

@docs firstChild, lastChild, parent, forward, backward, root, lastDescendant, nextSibling, previousSibling


# Utility

@docs find, findFromRoot

-}

import Tree exposing (Tree)


{-| TODO: docs
-}
type alias Crumb a =
    { before : List (Tree a)
    , datum : a
    , after : List (Tree a)
    }


{-| TODO: docs
-}
type alias Zipper a =
    { focus : Tree a, crumbs : List (Crumb a) }


{-| TODO: docs

    import Tree exposing (Tree, tree)


    myTree : Tree Int
    myTree =
        tree 1
            [ Tree.singleton 2
            , Tree.singleton 3
            ]


    init myTree
    --> { focus = myTree, crumbs = [] }

-}
init : Tree a -> Zipper a
init t =
    { focus = t, crumbs = [] }


{-| Try to move "forward". This means either to the first child, the next
sibling, the next descendant of an ancestor or - if all else fails - `Nothing`.
-}
forward : Zipper a -> Maybe (Zipper a)
forward zipper =
    firstOf [ firstChild, nextSibling, nextSiblingOfAncestor ] zipper


{-| TODO: docs
-}
backward : Zipper a -> Maybe (Zipper a)
backward zipper =
    firstOf [ previousSibling >> Maybe.map lastDescendant, parent ] zipper


{-| TODO: docs
-}
parent : Zipper a -> Maybe (Zipper a)
parent zipper =
    case zipper.crumbs of
        [] ->
            Nothing

        crumb :: rest ->
            Just <|
                { focus = reconstruct zipper.focus crumb
                , crumbs = rest
                }


{-| TODO: docs
-}
firstChild : Zipper a -> Maybe (Zipper a)
firstChild zipper =
    case Tree.children zipper.focus of
        [] ->
            Nothing

        c :: cs ->
            zipper
                |> withFocus c
                |> addCrumb
                    { before = []
                    , datum = Tree.datum zipper.focus
                    , after = cs
                    }
                |> Just


{-| TODO: docs
-}
lastChild : Zipper a -> Maybe (Zipper a)
lastChild zipper =
    case List.reverse <| Tree.children zipper.focus of
        [] ->
            Nothing

        c :: rest ->
            zipper
                |> withFocus c
                |> addCrumb
                    { before = rest
                    , datum = Tree.datum zipper.focus
                    , after = []
                    }
                |> Just


{-| TODO: docs
-}
root : Zipper a -> Zipper a
root zipper =
    case zipper.crumbs of
        [] ->
            zipper

        crumb :: rest ->
            root <|
                { focus = reconstruct zipper.focus crumb
                , crumbs = rest
                }


{-| TODO: docs
-}
lastDescendant : Zipper a -> Zipper a
lastDescendant zipper =
    case lastChild zipper of
        Nothing ->
            zipper

        Just child ->
            lastDescendant child


{-| TODO: docs
-}
nextSibling : Zipper a -> Maybe (Zipper a)
nextSibling zipper =
    case zipper.crumbs of
        crumb :: rest ->
            case crumb.after of
                [] ->
                    Nothing

                next :: after ->
                    Just <|
                        { focus = next
                        , crumbs =
                            { crumb
                                | before = zipper.focus :: crumb.before
                                , after = after
                            }
                                :: rest
                        }

        _ ->
            Nothing


{-| TODO: docs
-}
previousSibling : Zipper a -> Maybe (Zipper a)
previousSibling zipper =
    case zipper.crumbs of
        crumb :: rest ->
            case crumb.before of
                [] ->
                    Nothing

                previous :: before ->
                    Just <|
                        { focus = previous
                        , crumbs =
                            { crumb
                                | before = before
                                , after = zipper.focus :: crumb.after
                            }
                                :: rest
                        }

        _ ->
            Nothing


{-| TODO: docs
-}
find : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find f zipper =
    if f <| Tree.datum zipper.focus then
        Just zipper
    else
        case forward zipper of
            Just next ->
                find f next

            Nothing ->
                Nothing


{-| TODO: docs
-}
findFromRoot : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findFromRoot f zipper =
    find f (root zipper)


{-| TODO: docs
-}
current : Zipper a -> Tree a
current =
    .focus


firstOf : List (a -> Maybe b) -> a -> Maybe b
firstOf options v =
    case options of
        [] ->
            Nothing

        option :: rest ->
            case option v of
                Just r ->
                    Just r

                Nothing ->
                    firstOf rest v


nextSiblingOfAncestor : Zipper a -> Maybe (Zipper a)
nextSiblingOfAncestor zipper =
    case parent zipper of
        Nothing ->
            Nothing

        Just parent ->
            case nextSibling parent of
                Nothing ->
                    nextSiblingOfAncestor parent

                Just s ->
                    Just s


lastChildOfPreviousSibling : Zipper a -> Maybe (Zipper a)
lastChildOfPreviousSibling zipper =
    case previousSibling zipper of
        Nothing ->
            Nothing

        Just previous ->
            lastChild previous


reconstruct : Tree a -> Crumb a -> Tree a
reconstruct focus { before, datum, after } =
    Tree.tree datum (List.reverse before ++ [ focus ] ++ after)


withFocus : Tree a -> Zipper a -> Zipper a
withFocus focus zipper =
    { zipper | focus = focus }


addCrumb : Crumb a -> Zipper a -> Zipper a
addCrumb crumb zipper =
    { zipper | crumbs = crumb :: zipper.crumbs }
