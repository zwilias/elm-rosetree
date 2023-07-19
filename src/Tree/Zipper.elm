module Tree.Zipper exposing
    ( Zipper, fromTree, fromForest, toTree, toForest, tree, label, children
    , firstChild, lastChild, parent, forward, backward, root, lastDescendant, nextSibling, previousSibling
    , siblingsBeforeFocus, siblingsAfterFocus
    , updateTree, replaceTree, removeTree, updateLabel, replaceLabel, append, prepend
    , findNext, findPrevious, findFromRoot
    )

{-| Imagine walking through a `Tree` structure. You can step from a node to its
parent, its children or one of its sibling. At every step of the way, you're "at"
a tree. A `Tree.Zipper` represents such a step, and offers an API to navigate and
modify the tree structure while walking through it.


# Structure

@docs Zipper, fromTree, fromForest, toTree, toForest, tree, label, children


# Navigation

@docs firstChild, lastChild, parent, forward, backward, root, lastDescendant, nextSibling, previousSibling


# Location

@docs siblingsBeforeFocus, siblingsAfterFocus


# Modification

@docs updateTree, replaceTree, removeTree, updateLabel, replaceLabel, append, prepend


# Utility

@docs findNext, findPrevious, findFromRoot

-}

import Tree exposing (Tree)


{-| Represents a location within a tree, always pointing at the root or one of
its descendant trees.
-}
type Zipper a
    = Zipper
        { focus : Tree a
        , before : List (Tree a)
        , after : List (Tree a)
        , crumbs : List (Crumb a)
        }


type alias Crumb a =
    { label : a
    , before : List (Tree a)
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
    Zipper { focus = t, before = [], after = [], crumbs = [] }


{-| Every once in a while, we'll start at a tree that has siblings, but no
parent.

    import Tree


    fromForest (Tree.singleton "first") [ Tree.singleton "second" ]
        |> nextSibling
        |> Maybe.map label
    --> Just "second"

-}
fromForest : Tree a -> List (Tree a) -> Zipper a
fromForest t ts =
    Zipper { focus = t, before = [], after = ts, crumbs = [] }


{-| `toTree` rebuilds the tree to its root, and returns that.

Note that if the root has siblings, these end up being ignored!

    import Tree exposing (Tree)


    myTree : Tree Int
    myTree =
        Tree.tree 1
            [ Tree.singleton 2
            , Tree.singleton 3
            ]


    fromTree myTree
        |> lastDescendant
        |> updateLabel (\x -> x * 2)
        |> toTree
    --> Tree.tree 1
    -->     [ Tree.singleton 2
    -->     , Tree.singleton 6
    -->     ]

-}
toTree : Zipper a -> Tree a
toTree =
    tree << root


{-| Occasionally, we'll want to rebuild the forest, returning the "root" and its
subsequent siblings.

    import Tree

    Tree.singleton "root"
        |> fromTree
        |> prepend (Tree.singleton "before")
        |> append (Tree.singleton "after")
        |> toForest
    --> ( Tree.singleton "before"
    --> , [ Tree.singleton "root"
    -->   , Tree.singleton "after"
    -->   ]
    --> )

-}
toForest : Zipper a -> ( Tree a, List (Tree a) )
toForest input =
    let
        (Zipper { focus, after }) =
            root input
    in
    ( focus, after )


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
        |> updateLabel (\x -> x * 2)
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
                    { focus = reconstruct zipper.focus zipper.before zipper.after crumb.label
                    , before = crumb.before
                    , after = crumb.after
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
firstChild (Zipper zipper) =
    case Tree.children zipper.focus of
        [] ->
            Nothing

        c :: cs ->
            Just <|
                Zipper
                    { focus = c
                    , before = []
                    , after = cs
                    , crumbs =
                        { label = Tree.label zipper.focus
                        , before = zipper.before
                        , after = zipper.after
                        }
                            :: zipper.crumbs
                    }


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
lastChild (Zipper zipper) =
    case List.reverse <| Tree.children zipper.focus of
        [] ->
            Nothing

        c :: rest ->
            Just <|
                Zipper
                    { focus = c
                    , before = rest
                    , after = []
                    , crumbs =
                        { label = Tree.label zipper.focus
                        , before = zipper.before
                        , after = zipper.after
                        }
                            :: zipper.crumbs
                    }


{-| From anywhere, this zooms back up to the root of the tree.

This is essentially equivalent to executing `parent` over and over as long as it
succeeds.

-}
root : Zipper a -> Zipper a
root zipper =
    case parent zipper of
        Nothing ->
            firstSibling zipper

        Just z ->
            root z


firstSibling : Zipper a -> Zipper a
firstSibling zipper =
    case previousSibling zipper of
        Nothing ->
            zipper

        Just z ->
            firstSibling z


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
    case zipper.after of
        [] ->
            Nothing

        next :: rest ->
            Just <|
                Zipper
                    { focus = next
                    , before = zipper.focus :: zipper.before
                    , after = rest
                    , crumbs = zipper.crumbs
                    }


{-| Not too surprisingly, moves to the previous sibling if there are any.

In particular, this returns `Nothing` for nodes that are the first child or the
root.

-}
previousSibling : Zipper a -> Maybe (Zipper a)
previousSibling (Zipper zipper) =
    case zipper.before of
        [] ->
            Nothing

        previous :: rest ->
            Just <|
                Zipper
                    { focus = previous
                    , before = rest
                    , after = zipper.focus :: zipper.after
                    , crumbs = zipper.crumbs
                    }


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


{-| The siblings before the currently focused tree.

    import Tree exposing (Tree)
    import Tree.Zipper as Zipper exposing (Zipper)

    myTree : Tree Int
    myTree =
        Tree.tree 0
            [ Tree.singleton 1
            , Tree.singleton 2
            , Tree.singleton 3
            ]


    fromTree myTree
        |> forward
        |> Maybe.map siblingsBeforeFocus
        |> Maybe.map (List.map Tree.label)
    --> Just []


    fromTree myTree
        |> lastDescendant
        |> siblingsBeforeFocus
        |> List.map Tree.label
    --> [1, 2]


    myForest : (Tree Int, List (Tree Int))
    myForest =
        ( Tree.singleton 0
        , [ Tree.singleton 4
          , Tree.singleton 5
          , Tree.singleton 6
          ]
        )


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> findFromRoot ((==) 4)
        |> Maybe.map siblingsBeforeFocus
        |> Maybe.map  (List.map Tree.label)
    --> Just [0]

-}
siblingsBeforeFocus : Zipper a -> List (Tree a)
siblingsBeforeFocus (Zipper { before }) =
    List.reverse before


{-| The siblings after the currently focused tree.

    import Tree exposing (Tree)
    import Tree.Zipper as Zipper exposing (Zipper)

    myTree : Tree Int
    myTree =
        Tree.tree 0
            [ Tree.singleton 1
            , Tree.singleton 2
            , Tree.singleton 3
            ]


    fromTree myTree
        |> forward
        |> Maybe.map siblingsAfterFocus
        |> Maybe.map (List.map Tree.label)
    --> Just [2, 3]


    fromTree myTree
        |> lastDescendant
        |> siblingsAfterFocus
        |> List.map Tree.label
    --> []


    myForest : (Tree Int, List (Tree Int))
    myForest =
        ( Tree.singleton 0
        , [ Tree.singleton 4
          , Tree.singleton 5
          , Tree.singleton 6
          ]
        )


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> siblingsAfterFocus
        |> List.map Tree.label
    --> [4, 5, 6]

-}
siblingsAfterFocus : Zipper a -> List (Tree a)
siblingsAfterFocus (Zipper { after }) =
    after


{-| Execute a function on the currently focused tree, replacing it in the zipper.
-}
updateTree : (Tree a -> Tree a) -> Zipper a -> Zipper a
updateTree f (Zipper zipper) =
    Zipper { zipper | focus = f zipper.focus }


{-| Replace the currently focused tree in the zipper with a different one.
-}
replaceTree : Tree a -> Zipper a -> Zipper a
replaceTree t (Zipper zipper) =
    Zipper { zipper | focus = t }


{-| Remove currently focused tree and return the parent, a previous sibling or a next sibling of it. If there is neither,
return `Nothing`.

    import Tree exposing (Tree)
    import Tree.Zipper as Zipper exposing (Zipper)

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
        |> removeTree
        |> Maybe.map label
    --> Just 4


    myForest : (Tree Int, List (Tree Int))
    myForest =
        ( Tree.tree 0
              [ Tree.singleton 1
              , Tree.singleton 2
              , Tree.singleton 3
              ]
        , [ Tree.singleton 4
          , Tree.singleton 5
          , Tree.singleton 6
          ]
        )


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> findFromRoot ((==) 1)
        |> Maybe.andThen removeTree
        |> Maybe.map label
    --> Just 0


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> findFromRoot ((==) 4)
        |> Maybe.andThen removeTree
        |> Maybe.map label
    --> Just 0


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> removeTree
        |> Maybe.map label
    --> Just 4


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> removeTree
        |> Maybe.andThen nextSibling
        |> Maybe.map label
    --> Just 5


    fromForest (Tuple.first myForest) (Tuple.second myForest)
        |> findFromRoot ((==) 6)
        |> Maybe.andThen removeTree
        |> Maybe.andThen previousSibling
        |> Maybe.map label
    --> Just 4

-}
removeTree : Zipper a -> Maybe (Zipper a)
removeTree (Zipper zipper) =
    case ( zipper.crumbs, zipper.before, zipper.after ) of
        ( [], [], [] ) ->
            Nothing

        ( crumb :: rest, before, after ) ->
            Just <|
                Zipper
                    { focus = reconstructWithoutFocus crumb.label before after
                    , before = crumb.before
                    , after = crumb.after
                    , crumbs = rest
                    }

        ( [], b :: bs, after ) ->
            Just <|
                Zipper
                    { focus = b
                    , before = bs
                    , after = after
                    , crumbs = []
                    }

        ( [], [], a :: as_ ) ->
            Just <|
                Zipper
                    { focus = a
                    , before = []
                    , after = as_
                    , crumbs = []
                    }


{-| Map a function on the label of the currently focused tree.
-}
updateLabel : (a -> a) -> Zipper a -> Zipper a
updateLabel f zipper =
    updateTree (Tree.updateLabel f) zipper


{-| Replace the label of the currently focused tree.
-}
replaceLabel : a -> Zipper a -> Zipper a
replaceLabel l zipper =
    updateLabel (always l) zipper


{-| Prepend a tree as a sibling _before_ the currently focused tree.
-}
prepend : Tree a -> Zipper a -> Zipper a
prepend t (Zipper zipper) =
    Zipper { zipper | before = t :: zipper.before }


{-| Append a tree as a sibling _after_ the currently focused tree.
-}
append : Tree a -> Zipper a -> Zipper a
append t (Zipper zipper) =
    Zipper { zipper | after = t :: zipper.after }


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

        Just parent_ ->
            case nextSibling parent_ of
                Nothing ->
                    nextSiblingOfAncestor parent_

                Just s ->
                    Just s


reconstruct : Tree a -> List (Tree a) -> List (Tree a) -> a -> Tree a
reconstruct focus before after l =
    Tree.tree l (List.reverse before ++ [ focus ] ++ after)


reconstructWithoutFocus : a -> List (Tree a) -> List (Tree a) -> Tree a
reconstructWithoutFocus l before after =
    Tree.tree l (List.reverse before ++ after)
