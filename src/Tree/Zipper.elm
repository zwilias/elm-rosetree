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
type Zipper a
    = Zipper { focus : Tree a, crumbs : List (Crumb a) }


{-| TODO: docs

    import Tree exposing (Tree)


    myTree : Tree Int
    myTree =
        Tree.tree 1
            [ Tree.singleton 2
            , Tree.singleton 3
            ]


    fromTree myTree
        |> label
    --> 1

-}
fromTree : Tree a -> Zipper a
fromTree t =
    Zipper { focus = t, crumbs = [] }


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
parent (Zipper zipper) =
    case zipper.crumbs of
        [] ->
            Nothing

        crumb :: rest ->
            Just <|
                Zipper
                    { focus = reconstruct zipper.focus crumb
                    , crumbs = rest
                    }


{-| TODO: docs
-}
firstChild : Zipper a -> Maybe (Zipper a)
firstChild zipper =
    case children zipper of
        [] ->
            Nothing

        c :: cs ->
            zipper
                |> withFocus c
                |> addCrumb
                    { before = []
                    , label = label zipper
                    , after = cs
                    }
                |> Just


{-| TODO: docs
-}
lastChild : Zipper a -> Maybe (Zipper a)
lastChild zipper =
    case List.reverse <| children zipper of
        [] ->
            Nothing

        c :: rest ->
            zipper
                |> withFocus c
                |> addCrumb
                    { before = rest
                    , label = label zipper
                    , after = []
                    }
                |> Just


{-| TODO: docs
-}
root : Zipper a -> Zipper a
root (Zipper zipper) =
    case zipper.crumbs of
        [] ->
            Zipper zipper

        crumb :: rest ->
            root <|
                Zipper
                    { focus = reconstruct zipper.focus crumb
                    , crumbs = rest
                    }


isRoot : Zipper a -> Bool
isRoot (Zipper { crumbs }) =
    List.isEmpty crumbs


isFirst : Zipper a -> Bool
isFirst (Zipper { crumbs }) =
    case crumbs of
        { before } :: _ ->
            List.isEmpty before

        _ ->
            False


isLast : Zipper a -> Bool
isLast (Zipper { crumbs }) =
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
nextSibling (Zipper zipper) =
    case zipper.crumbs of
        crumb :: rest ->
            case crumb.after of
                [] ->
                    Nothing

                next :: after ->
                    Just <|
                        Zipper
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
previousSibling (Zipper zipper) =
    case zipper.crumbs of
        crumb :: rest ->
            case crumb.before of
                [] ->
                    Nothing

                previous :: before ->
                    Just <|
                        Zipper
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
    if f <| label zipper then
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
tree (Zipper { focus }) =
    focus


{-| TODO: docs
-}
label : Zipper a -> a
label zipper =
    Tree.label <| tree zipper


{-| TODO: docs
-}
children : Zipper a -> List (Tree a)
children zipper =
    Tree.children <| tree zipper


mapTree : (Tree a -> Tree a) -> Zipper a -> Zipper a
mapTree f (Zipper zipper) =
    Zipper { zipper | focus = f zipper.focus }


replaceTree : Tree a -> Zipper a -> Zipper a
replaceTree t zipper =
    mapTree (always t) zipper


mapLabel : (a -> a) -> Zipper a -> Zipper a
mapLabel f zipper =
    mapTree (Tree.mapLabel f) zipper


replaceLabel : a -> Zipper a -> Zipper a
replaceLabel l zipper =
    mapLabel (always l) zipper


prepend : Tree a -> Zipper a -> Zipper a
prepend t (Zipper zipper) =
    case zipper.crumbs of
        [] ->
            Zipper zipper

        crumb :: rs ->
            Zipper { zipper | crumbs = { crumb | before = t :: crumb.before } :: rs }


append : Tree a -> Zipper a -> Zipper a
append t (Zipper zipper) =
    case zipper.crumbs of
        [] ->
            Zipper zipper

        crumb :: rs ->
            Zipper { zipper | crumbs = { crumb | after = t :: crumb.after } :: rs }


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
withFocus focus (Zipper zipper) =
    Zipper { zipper | focus = focus }


addCrumb : Crumb a -> Zipper a -> Zipper a
addCrumb crumb (Zipper zipper) =
    Zipper { zipper | crumbs = crumb :: zipper.crumbs }
