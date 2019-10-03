module Tree.Diff exposing
    ( Diff(..), Tail(..)
    , diff, diffWith, diffBy
    , mergeWith, mergeBy
    )

{-| Diffing and merging trees

The `Tree.Diff` module offers datastructures and function to handle diffing and
merging trees. A diff represents the abstract action that need to be taken to go
from one tree to another. Merging trees allows actually executing these actions.


## Data types

@docs Diff, Tail


## Diffing

@docs diff, diffWith, diffBy


## Merging

**Note:** Merging trees according to the diff structure described here using
regular equality on the labels will always result in the second tree being
returned. For that reason, only `mergeWith` and `mergeBy` exist: `merge a b = b`
feels like a silly function to offer!

@docs mergeWith, mergeBy

-}

import Tree exposing (Tree)


{-| Either nothing changed, and we can keep a (sub)tree, or something changed.

When something changed, there are essentially 2 cases:

  - the labels on the matching nodes were different. If so, we `Replace` the
    left with the right tree.
  - the labels on this node were the same, but there was a difference in one or
    more of the children.

In case there was a different with the children, there is also the possibility
that the length was different. This is described in the `Tail` of the `Copy`.

-}
type Diff a
    = Keep (Tree a)
    | Replace (Tree a) (Tree a)
    | Copy a (List (Diff a)) (Tail a)


{-| If the left node had more children than the right now, we get a `Left` tail
with the trailing children from the left tree.

On the other hand, if the right node had more children we get a `Right` tail.

If both trees has the same number of children, the tail is `Empty`.

-}
type Tail a
    = Left (List (Tree a))
    | Right (List (Tree a))
    | Empty


{-| Diffing 2 trees (using standard equivalence `(==)`) produces a `Diff`!

    import Tree.Diff as Diff
    import Tree exposing (tree, singleton)

    Diff.diff
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
            ]
        )
    --> Diff.Copy "root"
    -->     [ Diff.Copy "folder"
    -->         [ Diff.Keep (singleton "foo") ]
    -->         (Diff.Left [ singleton "bar" ])
    -->     , Diff.Replace
    -->         (singleton "yeah")
    -->         (tree "folder2" [ singleton "nice" ])
    -->     , Diff.Keep (singleton "keep me!")
    -->     ]
    -->     Diff.Empty

-}
diff : Tree a -> Tree a -> Diff a
diff left right =
    diffWith (==) left right


{-| Diff using custom equivalence.

This allows using a custom function to decide whether two labels are really
equivalent. Perhaps you're using some custom datatype and you consider two
instances of them to be equivalent if they hold the same data, regardless of
their structural equality? Or perhaps your labels are floats, and you want to
check using some epsilon value?

This is your function!

-}
diffWith : (a -> a -> Bool) -> Tree a -> Tree a -> Diff a
diffWith eq left right =
    if eq (Tree.label left) (Tree.label right) then
        walk diffConf
            eq
            { left = Tree.children left
            , right = Tree.children right
            , tree = left
            , done = []
            , isAllKeep = True
            }
            []

    else
        Replace left right


{-| Diff using regular equality on a derived property of the label.

This is related to `diffWith` in the same way `List.sortBy` is related to
`List.sortWith`. Imagine, for example, that your labels are tuples and you're
only interested in the second value.

You could either write `diffWith (\(_, x) (_, y) -> x == y) left right` or the
equivalent but much simple `diffBy Tuple.second`.

If you find yourself being worried about performance: Please benchmark!

-}
diffBy : (a -> b) -> Tree a -> Tree a -> Diff a
diffBy f =
    diffWith (\left right -> f left == f right)


diffConf : Conf a (Diff a)
diffConf =
    { keep = Keep
    , copyS = \a b -> Copy a (List.reverse b) Empty
    , copyL = \a b c -> Copy a (List.reverse b) (Left c)
    , copyR = \a b c -> Copy a (List.reverse b) (Right c)
    , replace = Replace
    }



--------------------------------------------------------------------------------


{-| -}
mergeWith : (a -> a -> Bool) -> Tree a -> Tree a -> Tree a
mergeWith eq left right =
    if eq (Tree.label left) (Tree.label right) then
        walk mergeConf
            eq
            { left = Tree.children left
            , right = Tree.children right
            , tree = left
            , done = []
            , isAllKeep = True
            }
            []

    else
        right


{-| Has the same relation to `mergeWith` as `diffBy` has to `diffWith`.

    import Tree.Diff as Diff
    import Tree exposing (tree, singleton)

    Diff.mergeBy Tuple.second
        (tree ( 1, "root" )
            [ tree ( 1, "folder" )
                [ singleton ( 1, "foo" )
                , singleton ( 1, "bar" )
                ]
            , singleton ( 1, "yeah" )
            , singleton ( 1, "keep me!" )
            ]
        )
        (tree ( 2, "root" )
            [ tree ( 2, "folder" )
                [ singleton ( 2, "foo" ) ]
            , tree ( 2, "folder2" )
                [ singleton ( 2, "nice" ) ]
            , singleton ( 2, "keep me!" )
            ]
        )
    --> tree ( 1, "root" )
    -->     [ tree ( 1, "folder" )
    -->         [ singleton ( 1, "foo" ) ]
    -->     , tree ( 2, "folder2" )
    -->         [ singleton ( 2, "nice" ) ]
    -->     , singleton ( 1, "keep me!" )
    -->     ]

-}
mergeBy : (a -> b) -> Tree a -> Tree a -> Tree a
mergeBy f =
    mergeWith (\left right -> f left == f right)


mergeConf : Conf a (Tree a)
mergeConf =
    { keep = identity
    , copyS = \l c -> Tree.tree l (List.reverse c)
    , copyL = \l c _ -> Tree.tree l (List.reverse c)
    , copyR = \l c cs -> Tree.tree l (List.foldl (::) cs c)
    , replace = \_ t -> t
    }



--------------------------------------------------------------------------------


type alias Acc a x =
    { left : List (Tree a)
    , right : List (Tree a)
    , tree : Tree a
    , done : List x
    , isAllKeep : Bool
    }


type alias Conf a x =
    { keep : Tree a -> x
    , copyS : a -> List x -> x
    , copyL : a -> List x -> List (Tree a) -> x
    , copyR : a -> List x -> List (Tree a) -> x
    , replace : Tree a -> Tree a -> x
    }


walk : Conf a x -> (a -> a -> Bool) -> Acc a x -> List (Acc a x) -> x
walk conf eq acc stack =
    case ( acc.left, acc.right ) of
        ( [], [] ) ->
            let
                p =
                    if acc.isAllKeep then
                        conf.keep acc.tree

                    else
                        conf.copyS (Tree.label acc.tree) acc.done
            in
            case stack of
                [] ->
                    p

                newAcc :: rest ->
                    walk conf
                        eq
                        { left = newAcc.left
                        , right = newAcc.right
                        , tree = newAcc.tree
                        , done = p :: newAcc.done
                        , isAllKeep = acc.isAllKeep && newAcc.isAllKeep
                        }
                        rest

        ( [], _ :: _ ) ->
            let
                p =
                    conf.copyR (Tree.label acc.tree) acc.done acc.right
            in
            case stack of
                [] ->
                    p

                newAcc :: rest ->
                    walk conf
                        eq
                        { left = newAcc.left
                        , right = newAcc.right
                        , tree = newAcc.tree
                        , done = p :: newAcc.done
                        , isAllKeep = acc.isAllKeep && newAcc.isAllKeep
                        }
                        rest

        ( _ :: _, [] ) ->
            let
                p =
                    conf.copyL (Tree.label acc.tree) acc.done acc.left
            in
            case stack of
                [] ->
                    p

                newAcc :: rest ->
                    walk conf
                        eq
                        { left = newAcc.left
                        , right = newAcc.right
                        , tree = newAcc.tree
                        , done = p :: newAcc.done
                        , isAllKeep = acc.isAllKeep && newAcc.isAllKeep
                        }
                        rest

        ( l :: restL, r :: restR ) ->
            if eq (Tree.label l) (Tree.label r) then
                walk conf
                    eq
                    { left = Tree.children l
                    , right = Tree.children r
                    , tree = l
                    , done = []
                    , isAllKeep = True
                    }
                    ({ left = restL
                     , right = restR
                     , tree = acc.tree
                     , done = acc.done
                     , isAllKeep = acc.isAllKeep
                     }
                        :: stack
                    )

            else
                walk conf
                    eq
                    { left = restL
                    , right = restR
                    , tree = acc.tree
                    , done = conf.replace l r :: acc.done
                    , isAllKeep = False
                    }
                    stack
