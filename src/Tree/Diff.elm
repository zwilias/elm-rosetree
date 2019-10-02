module Tree.Diff exposing
    ( Diff(..), Patch(..)
    , diff, diffWith, diffBy
    , mergeBy, mergeWith
    )

{-| Diffing and merging trees

The `Tree.Diff` module offers datastructures and function to handle diffing and
merging trees. A diff represents the abstract action that need to be taken to go
from one tree to another. Merging trees allows actually executing these actions.


## Data types

@docs Diff, Patch


## Diffing

@docs diff, diffWith, diffBy


## Merging

**Note:** Merging trees according to the diff structure described here using
regular equality on the labels will always result in the second tree being
returned. For that reason, only `mergeWith` and `mergeBy` exist: `merge a b = b`
feels like a silly function to offer!

@docs mergeWith mergeBy

-}

import Tree exposing (Tree)


{-| Either nothing changed, and we can keep a (sub)tree, or something changed.

When something changed, the resulting change is further described in the `Patch`
type.

-}
type Diff a
    = Keep (Tree a)
    | Patch (Patch a)


{-| A patch described the concrete operation required to go from the left tree
to the right tree.

At the root of the tree, only `Replace` and `Copy` make sense:

  - `Replace` means that the root label differed, and as a result, the entire tree
    is considered different.
  - `Copy` means that the label is the same for both the left and right trees, but
    something deeper in the tree has changed

Those nested diffs could potentially be further patches, including the deleting
and insertion of children.

-}
type Patch a
    = Insert (Tree a)
    | Delete (Tree a)
    | Replace (Tree a) (Tree a)
    | Copy a (List (Diff a))


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
    --> Diff.Patch
    -->     (Diff.Copy "root"
    -->         [ Diff.Patch
    -->             (Diff.Copy "folder"
    -->                 [ Diff.Keep (singleton "foo")
    -->                 , Diff.Patch (Diff.Delete (singleton "bar"))
    -->                 ]
    -->             )
    -->         , Diff.Patch
    -->             (Diff.Replace
    -->                 (singleton "yeah")
    -->                 (tree "folder2" [ singleton "nice" ])
    -->             )
    -->         , Diff.Keep (singleton "keep me!")
    -->         ]
    -->     )

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
        diffHelp eq
            { left = Tree.children left
            , right = Tree.children right
            , tree = left
            , done = []
            , isAllKeep = True
            }
            []

    else
        Patch (Replace left right)


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


type alias DiffAcc a =
    { left : List (Tree a)
    , right : List (Tree a)
    , tree : Tree a
    , done : List (Diff a)
    , isAllKeep : Bool
    }


diffHelp : (a -> a -> Bool) -> DiffAcc a -> List (DiffAcc a) -> Diff a
diffHelp eq acc stack =
    case ( acc.left, acc.right ) of
        ( [], [] ) ->
            let
                p =
                    if acc.isAllKeep then
                        Keep acc.tree

                    else
                        Patch (Copy (Tree.label acc.tree) (List.reverse acc.done))
            in
            case stack of
                [] ->
                    p

                newAcc :: rest ->
                    diffHelp eq
                        { newAcc
                            | done = p :: newAcc.done
                            , isAllKeep = acc.isAllKeep && newAcc.isAllKeep
                        }
                        rest

        ( [], r :: restR ) ->
            diffHelp eq
                { acc
                    | right = restR
                    , done = Patch (Insert r) :: acc.done
                    , isAllKeep = False
                }
                stack

        ( l :: restL, [] ) ->
            diffHelp eq
                { acc
                    | left = restL
                    , done = Patch (Delete l) :: acc.done
                    , isAllKeep = False
                }
                stack

        ( l :: restL, r :: restR ) ->
            if eq (Tree.label l) (Tree.label r) then
                diffHelp eq
                    { left = Tree.children l
                    , right = Tree.children r
                    , tree = l
                    , done = []
                    , isAllKeep = True
                    }
                    ({ acc | left = restL, right = restR } :: stack)

            else
                diffHelp eq
                    { acc
                        | left = restL
                        , right = restR
                        , done = Patch (Replace l r) :: acc.done
                        , isAllKeep = False
                    }
                    stack


{-| TODO
-}
mergeWith : (a -> a -> Bool) -> Tree a -> Tree a -> Tree a
mergeWith eq left right =
    if eq (Tree.label left) (Tree.label right) then
        mergeHelp eq
            { left = Tree.children left
            , right = Tree.children right
            , tree = left
            , done = []
            , isAllKeep = True
            }
            []

    else
        right


{-| TODO
-}
mergeBy : (a -> b) -> Tree a -> Tree a -> Tree a
mergeBy f =
    mergeWith (\left right -> f left == f right)


type alias MergeAcc a =
    { left : List (Tree a)
    , right : List (Tree a)
    , tree : Tree a
    , done : List (Tree a)
    , isAllKeep : Bool
    }


mergeHelp : (a -> a -> Bool) -> MergeAcc a -> List (MergeAcc a) -> Tree a
mergeHelp eq acc stack =
    case ( acc.left, acc.right ) of
        ( [], [] ) ->
            let
                p =
                    if acc.isAllKeep then
                        acc.tree

                    else
                        Tree.tree (Tree.label acc.tree) (List.reverse acc.done)
            in
            case stack of
                [] ->
                    p

                newAcc :: rest ->
                    mergeHelp eq
                        { newAcc
                            | done = p :: newAcc.done
                            , isAllKeep = acc.isAllKeep && newAcc.isAllKeep
                        }
                        rest

        ( [], r :: restR ) ->
            mergeHelp eq
                { acc
                    | right = restR
                    , done = r :: acc.done
                    , isAllKeep = False
                }
                stack

        ( l :: restL, [] ) ->
            mergeHelp eq
                { acc
                    | left = restL
                    , isAllKeep = False
                }
                stack

        ( l :: restL, r :: restR ) ->
            if eq (Tree.label l) (Tree.label r) then
                mergeHelp eq
                    { left = Tree.children l
                    , right = Tree.children r
                    , tree = l
                    , done = []
                    , isAllKeep = True
                    }
                    ({ acc | left = restL, right = restR } :: stack)

            else
                mergeHelp eq
                    { acc
                        | left = restL
                        , right = restR
                        , done = r :: acc.done
                        , isAllKeep = False
                    }
                    stack
