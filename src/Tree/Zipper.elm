module Tree.Zipper
    exposing
        ( Crumb
        , Zipper
        , backward
        , children
        , find
        , findFromRoot
        , firstChild
        , forward
        , fromTree
        , label
        , lastChild
        , lastDescendant
        , nextSibling
        , parent
        , previousSibling
        , root
        , tree
        )

{-| TODO: docs


# Structure

@docs Zipper, Crumb, fromTree, tree, label, children


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
    , label : a
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


    fromTree myTree
    --> { focus = myTree, crumbs = [] }

-}
fromTree : Tree a -> Zipper a
fromTree t =
    { focus = t, crumbs = [] }


{-| TODO: docs
-}
toTree : Zipper a -> Tree a
toTree =
    tree << root


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
                    , label = Tree.label zipper.focus
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
                    , label = Tree.label zipper.focus
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


isRoot : Zipper a -> Bool
isRoot { crumbs } =
    List.isEmpty crumbs


isFirst : Zipper a -> Bool
isFirst { crumbs } =
    case crumbs of
        { before } :: _ ->
            List.isEmpty before

        _ ->
            False


isLast : Zipper a -> Bool
isLast { crumbs } =
    case crumbs of
        { after } :: _ ->
            List.isEmpty after

        _ ->
            False


hasChildren : Zipper a -> Bool
hasChildren =
    List.isEmpty << children


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
    if f <| Tree.label zipper.focus then
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
tree : Zipper a -> Tree a
tree =
    .focus


{-| TODO: docs
-}
label : Zipper a -> a
label =
    Tree.label << tree


{-| TODO: docs
-}
children : Zipper a -> List (Tree a)
children =
    Tree.children << tree


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


reconstruct : Tree a -> Crumb a -> Tree a
reconstruct focus { before, label, after } =
    Tree.tree label (List.reverse before ++ [ focus ] ++ after)


withFocus : Tree a -> Zipper a -> Zipper a
withFocus focus zipper =
    { zipper | focus = focus }


addCrumb : Crumb a -> Zipper a -> Zipper a
addCrumb crumb zipper =
    { zipper | crumbs = crumb :: zipper.crumbs }
