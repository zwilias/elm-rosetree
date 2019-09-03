module Tree.Diff exposing
    ( Diff(..), Patch(..)
    , diff, diffWith
    )

{-| A representation of the differences between two trees.

Keep in mind that this a rather naive implementation: as soon as any label is
considered different, the whole subtree is considered different. Likewise,
changes in "level" are considered replacements.


## Data types

@docs Diff, Patch


## Diffing

@docs diff, diffWith

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


type alias Acc a =
    { left : List (Tree a)
    , right : List (Tree a)
    , tree : Tree a
    , done : List (Diff a)
    , isAllKeep : Bool
    }


diffHelp : (a -> a -> Bool) -> Acc a -> List (Acc a) -> Diff a
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
