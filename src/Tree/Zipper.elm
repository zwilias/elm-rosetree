module Tree.Zipper
    exposing
        ( Zipper
        , append
        , backward
        , children
        , findFromRoot
        , findNext
        , findPrevious
        , firstChild
        , forward
        , fromTree
        , label
        , lastChild
        , lastDescendant
        , mapLabel
        , mapTree
        , nextSibling
        , parent
        , prepend
        , previousSibling
        , replaceLabel
        , replaceTree
        , root
        , toTree
        , tree
        )

{-| Imagine walking through a `Tree` structure. You can step from a node to its
parent, its children or one of its sibling. At every step of the way, you're "at"
a tree. A `Tree.Zipper` represents such a step, and offers an API to navigate and
modify the tree structure while walking through it.


# Structure

@docs Zipper, fromTree, toTree, tree, label, children


# Navigation

@docs firstChild, lastChild, parent, forward, backward, root, lastDescendant, nextSibling, previousSibling


# Modification

@docs mapTree, replaceTree, mapLabel, replaceLabel, append, prepend


# Utility

@docs findNext, findPrevious, findFromRoot

-}

import Tree exposing (Tree)


{-| Represents a location within a tree, always pointing at the root or one of
its descendant trees.
-}
type Zipper a
    = Zipper { focus : Tree a, crumbs : List (Crumb a) }


type alias Crumb a =
    { before : List (Tree a)
    , label : a
    , after : List (Tree a)
    }


{-| To start your journey, you need to start at a tree. `fromTree` creates a
zipper with the given tree as its root.

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


{-| `toTree` rebuilds the tree to its root, and returns that.

    import Tree exposing (Tree)


    myTree : Tree Int
    myTree =
        Tree.tree 1
            [ Tree.singleton 2
            , Tree.singleton 3
            ]


    fromTree myTree
        |> lastDescendant
        |> mapLabel (\x -> x * 2)
        |> toTree
    --> Tree.tree 1
    -->     [ Tree.singleton 2
    -->     , Tree.singleton 6
    -->     ]

-}
toTree : Zipper a -> Tree a
toTree =
    tree << root


{-| Sometimes you don't want to extract the tree from the root, but just look at
the tree that's current in "in focus" in isolation.

    import Tree exposing (Tree)


    myTree : Tree Int
    myTree =
        Tree.tree 1
            [ Tree.singleton 2
            , Tree.singleton 3
            ]


    fromTree myTree
        |> lastDescendant
        |> mapLabel (\x -> x * 2)
        |> tree
    --> Tree.singleton 6

-}
tree : Zipper a -> Tree a
tree (Zipper { focus }) =
    focus


{-| Try to move "forward". This means either to the first child, the next
sibling, the next descendant of an ancestor or - if all else fails - `Nothing`.

    import Tree exposing (Tree)

    myTree : Tree String
    myTree =
        Tree.tree "root"
            [ Tree.singleton "first child"
            , Tree.tree "second child"
                [ Tree.singleton "third child" ]
            , Tree.singleton "last child"
            ]

First direction is to go down to the first child.

    fromTree myTree
        |> forward
        |> Maybe.map label
    --> Just "first child"

Since the first child doesn't have children of its own, the next step is to go
to the next sibling.

    fromTree myTree
        |> forward
        |> Maybe.andThen forward
        |> Maybe.map label
    --> Just "second child"

The next sibling _does_ have a child, so we go there, next.

    fromTree myTree
        |> forward
        |> Maybe.andThen forward
        |> Maybe.andThen forward
        |> Maybe.map label
    --> Just "third child"

The third child doesn't have children or siblings. However, its parent does, so
we go up a level and to the next sibling. If the direct parent doesn't have a
next sibling either, `forward` will look up the chain of ancestors one that has
a sibling.

    fromTree myTree
        |> forward
        |> Maybe.andThen forward
        |> Maybe.andThen forward
        |> Maybe.andThen forward
        |> Maybe.map label
    --> Just "last child"

Finally, after the last child, there is no next node - it doesn't have children,
no next siblings, and no ancestors that have any siblings left. So, we've reached
the end of the tree.

    fromTree myTree
        |> forward
        |> Maybe.andThen forward
        |> Maybe.andThen forward
        |> Maybe.andThen forward
        |> Maybe.andThen forward
    --> Nothing

-}
forward : Zipper a -> Maybe (Zipper a)
forward zipper =
    firstOf [ firstChild, nextSibling, nextSiblingOfAncestor ] zipper


{-| `backward` is the inverse of `forward`. As such, it will first try to go the
last descendant of a previous sibling, or the previous sibling if it has no
descendants. If there are no previous siblings, it will move up to its parent.

If all else fails (i.e. you are at the root) this returns `Nothing`.

    import Tree exposing (Tree)

    myTree : Tree String
    myTree =
        Tree.tree "root"
            [ Tree.singleton "first child"
            , Tree.tree "second child"
                [ Tree.singleton "third child" ]
            , Tree.singleton "last child"
            ]


    fromTree myTree
        |> lastDescendant -- Focus on "last child"
        |> backward -- jump to the last child of the previous sibling
        |> Maybe.map label
    --> Just "third child"


    fromTree myTree
        |> backward
    --> Nothing

-}
backward : Zipper a -> Maybe (Zipper a)
backward zipper =
    firstOf [ previousSibling >> Maybe.map lastDescendant, parent ] zipper


{-| Move to the parent. If the focus is on the root, there is no parent and this
returns `Nothing`.

    import Tree exposing (Tree)


    fromTree (Tree.singleton "root")
        |> parent
    --> Nothing


    Tree.tree "root"
        [ Tree.singleton "child" ]
        |> fromTree
        |> firstChild
        |> Maybe.andThen parent
        |> Maybe.map label
    --> Just "root"

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


{-| Move to the first child of the currently focused tree, if it has children.

If the current tree is a singeton, this returns `Nothing`.

    import Tree exposing (Tree)


    fromTree (Tree.singleton "root")
        |> firstChild
    --> Nothing


    Tree.tree "root"
        [ Tree.singleton "child" ]
        |> fromTree
        |> firstChild
        |> Maybe.map label
    --> Just "child"

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


{-| If the current tree has children, move to the last of those.

    import Tree exposing (Tree)


    myTree : Tree String
    myTree =
        Tree.tree "root"
            [ Tree.singleton "first"
            , Tree.tree "last child"
                [ Tree.singleton "child of last child"
                ]
            ]


    fromTree myTree
        |> lastChild
        |> Maybe.map label
    --> Just "last child"


    fromTree myTree
        |> lastChild
        |> Maybe.andThen lastChild
        |> Maybe.map label
    --> Just "child of last child"

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


{-| From anywhere, this zooms back up to the root of the tree.

This is essentially equivalent to executing `parent` over and over as long as it
succeeds.

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


{-| The inverse of `root`. Think of it as repeating `lastChild` as long as it
returns something.

Note that this will only try to descent within the current focus.

    import Tree exposing (Tree)


    myTree : Tree Int
    myTree =
        Tree.tree 0
            [ Tree.tree 1
                [ Tree.singleton 2
                , Tree.singleton 3
                ]
            , Tree.tree 4
                [ Tree.singleton 5 ]
            ]


    fromTree myTree
        |> lastDescendant
        |> label
    --> 5


    fromTree myTree
        |> firstChild
        |> Maybe.map lastDescendant
        |> Maybe.map label
    --> Just 3

-}
lastDescendant : Zipper a -> Zipper a
lastDescendant zipper =
    case lastChild zipper of
        Nothing ->
            zipper

        Just child ->
            lastDescendant child


{-| Does what it says on the tin. Move to the next sibling if the node has some
more siblings left.
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


{-| Not too surprisingly, moves to the previous sibling if there are any.

In particular, this returns `Nothing` for nodes that are the first child or the
root.

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


{-| Looks for a matching tree _after_ the current focus, using `forward` to
navigate. Excludes the current focus.
-}
findNext : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findNext f zipper =
    find f forward zipper


{-| Looks for a matching tree _before_ the current focus, using `backward` to
navigate. Excludes the current focus.
-}
findPrevious : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findPrevious f zipper =
    find f backward zipper


find : (a -> Bool) -> (Zipper a -> Maybe (Zipper a)) -> Zipper a -> Maybe (Zipper a)
find predicate move zipper =
    case move zipper of
        Just next ->
            if predicate (label next) then
                Just next
            else
                find predicate move next

        Nothing ->
            Nothing


{-| Find a tree whose label matches a given predicate, starting from (and
including) the root of the tree this zipper operates over.
-}
findFromRoot : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findFromRoot f zipper =
    let
        r =
            root zipper
    in
    if f (label r) then
        Just r
    else
        findNext f r


{-| The label of the currently focused tree.
-}
label : Zipper a -> a
label zipper =
    Tree.label <| tree zipper


{-| The children of the currently focused tree.
-}
children : Zipper a -> List (Tree a)
children zipper =
    Tree.children <| tree zipper


{-| Execute a function on the currently focused tree, replacing it in the zipper.
-}
mapTree : (Tree a -> Tree a) -> Zipper a -> Zipper a
mapTree f (Zipper zipper) =
    Zipper { zipper | focus = f zipper.focus }


{-| Replace the currently focused tree in the zipper with a different one.
-}
replaceTree : Tree a -> Zipper a -> Zipper a
replaceTree t zipper =
    mapTree (always t) zipper


{-| Map a function on the label of the currently focused tree.
-}
mapLabel : (a -> a) -> Zipper a -> Zipper a
mapLabel f zipper =
    mapTree (Tree.mapLabel f) zipper


{-| Replace the label of the currently focused tree.
-}
replaceLabel : a -> Zipper a -> Zipper a
replaceLabel l zipper =
    mapLabel (always l) zipper


{-| Prepend a tree as a sibling _before_ the currently focused tree. If the
current tree is the root, this is a no-op.
-}
prepend : Tree a -> Zipper a -> Zipper a
prepend t (Zipper zipper) =
    case zipper.crumbs of
        [] ->
            Zipper zipper

        crumb :: rs ->
            Zipper { zipper | crumbs = { crumb | before = t :: crumb.before } :: rs }


{-| Append a tree as a sibling _after_ the currently focused tree. If the current
tree is the root, this is a no-op.
-}
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
