module Tree exposing
    ( Tree, singleton, tree, label, children
    , mapLabel, replaceLabel, mapChildren, replaceChildren, prependChild, appendChild
    , count, depth
    , foldl, foldr
    , flatten, leaves, links
    , map, indexedMap, mapAccumulate, map2, indexedMap2, mapAccumulate2, andMap
    , findBfs
    , sortWith, unfold, restructure
    , Step(..), breadthFirstFold, depthFirstFold, depthFirstTraversal
    )

{-| A multiway tree or rosetree is a labeled tree where each node can have zero,
one or more children, each of which represents a tree in its own right.

The root of the tree is always labeled, so a tree always has at least one label.

As an example, such a structure could represent a directory structure:

    tree "root"
        [ tree "home"
            [ tree "user1" []
            , tree "user2" []
            ]
        , tree "etc" []
        , tree "var"
            [ tree "log" []
            ]
        ]

In a sense, `Html msg` is pretty similar to how such trees look, but they can be
used to represent other things. A nested menu structure, or a sitemap, or any
other structure where a single root is connected to children which can each have
children of their own, and so on.


# Structure

@docs Tree, singleton, tree, label, children


# Modification

@docs mapLabel, replaceLabel, mapChildren, replaceChildren, prependChild, appendChild


# Describing a tree

@docs count, depth


# Folds

@docs foldl, foldr


# Converting to lists

@docs flatten, leaves, links


# Mapping and traversing

@docs map, indexedMap, mapAccumulate, map2, indexedMap2, mapAccumulate2, andMap


# Search

@docs findBfs


# Fancy stuff

@docs sortWith, unfold, restructure


# Advanced: Generic traversals

These functions have highly complex type signatures, but they abstract very
generic ways of working with trees and in fact nearly all the other functions
in this library are built using them. In general it is better to prefer the simpler
interfaces, but there are situations that may not be covered by other functions
in this library, where these more powerful functions can come in handy.

Note that all the callbacks passed receive four arguments:

  - the `state` variable that is accumulated throughout the whole computation
  - a list of `ancestors`, that is all the labels that lie above the current node
  - the current `label` of the node being processed
  - its `children`

I like to call these `\s a l c`, since "salc" is nice and pronouncable and quite
easy to remember.

@docs Step, breadthFirstFold, depthFirstFold, depthFirstTraversal

You may want to read the source of this module for inspiration on how to use these
functions.

-}


{-| Represents a multiway tree. Each node in the tree holds a piece of
information (the `label`) and a list of children, each of which is a tree.
-}
type Tree a
    = Tree a (List (Tree a))


{-| Creates a singleton tree. This corresponds to `tree v []`.

    singleton 5
        |> label
    --> 5

    singleton "foo"
        |> children
    --> []

-}
singleton : a -> Tree a
singleton v =
    Tree v []


{-| Construct a tree from a label and a list of children.

    tree 5 []
    --> singleton 5


    tree 5
        [ singleton 1
        , singleton 2
        , tree 3
            [ singleton 4
            , singleton 5
            ]
        ]
        |> count
    --> 6

-}
tree : a -> List (Tree a) -> Tree a
tree =
    Tree


{-| Gives you the label of a tree.

    tree "hello" [ singleton "world", singleton "etc" ]
        |> label
    --> "hello"

-}
label : Tree a -> a
label (Tree v _) =
    v


{-| Execute a function on the label of this tree.

    tree "hello" [ singleton "world", singleton "etc" ]
        |> mapLabel String.toUpper
    --> tree "HELLO" [ singleton "world", singleton "etc" ]

-}
mapLabel : (a -> a) -> Tree a -> Tree a
mapLabel f (Tree v cs) =
    Tree (f v) cs


{-| Replace the label of this tree.

    singleton "foo"
        |> replaceLabel "bar"
    --> singleton "bar"

-}
replaceLabel : a -> Tree a -> Tree a
replaceLabel v (Tree _ cs) =
    Tree v cs


{-| Returns the children of a tree as a list.

    singleton "heh"
        |> children
    --> []


    tree "hello" [ singleton "world", singleton "etc" ]
        |> children
    --> [ singleton "world", singleton "etc" ]

-}
children : Tree a -> List (Tree a)
children (Tree _ c) =
    c


{-| Execute a function on the children of a tree.

    tree "lower1"
        [ singleton "upper1"
        , tree "upper2" [ singleton "lower2"]
        , singleton "upper3"
        ]
        |> mapChildren (List.map (mapLabel String.toUpper))
    --> tree "lower1"
    -->     [ singleton "UPPER1"
    -->     , tree "UPPER2" [ singleton "lower2"]
    -->     , singleton "UPPER3"
    -->     ]

-}
mapChildren : (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
mapChildren f (Tree v cs) =
    Tree v (f cs)


{-| Replace the children of a tree.

    tree "hello" [ singleton "world" ]
        |> replaceChildren [ singleton "everyone" ]
    --> tree "hello" [ singleton "everyone" ]

-}
replaceChildren : List (Tree a) -> Tree a -> Tree a
replaceChildren cs (Tree v _) =
    Tree v cs


{-| Prepend a single child to a tree.

    tree "hello" [ singleton "everyone" ]
        |> prependChild (singleton "dear")
    --> tree "hello" [ singleton "dear", singleton "everyone" ]

-}
prependChild : Tree a -> Tree a -> Tree a
prependChild c (Tree v cs) =
    Tree v (c :: cs)


{-| Append a child to a tree. Note that this uses `children ++ [ newChild ]`
under the hood so use sparingly.

    tree "hello" [ singleton "you" ]
        |> appendChild (singleton "and you!")
    --> tree "hello" [ singleton "you", singleton "and you!" ]

-}
appendChild : Tree a -> Tree a -> Tree a
appendChild c (Tree v cs) =
    Tree v (cs ++ [ c ])


{-| Count the labels in a tree.

    singleton "foo"
        |> count
    --> 1

    tree "foo" [ singleton "bar", singleton "baz" ]
        |> count
    --> 3

-}
count : Tree a -> Int
count t =
    foldl (\_ x -> x + 1) 0 t


{-| Fold over all the labels in a tree, left to right, depth first.

    tree "Hello "
        [ singleton "world "
        , tree "and "
            [ singleton "you "
            , singleton "and "
            , singleton "you"
            ]
        , singleton "!"
        ]
        |> foldl (\label acc -> acc ++ label) ""
    --> "Hello world and you and you!"

-}
foldl : (a -> b -> b) -> b -> Tree a -> b
foldl f acc t =
    depthFirstFold (\s _ l _ -> Continue (f l s)) acc t


{-| Fold over all the labels in a tree, right to left, depth first.

    tree 1
        [ singleton 2
        , tree 3
            [ singleton 4
            , singleton 5
            ]
        , singleton 6
        ]
        |> foldr (::) []
    --> [ 1, 2, 3, 4, 5, 6 ]

-}
foldr : (a -> b -> b) -> b -> Tree a -> b
foldr f acc t =
    List.foldl f acc <| foldl (::) [] t



-- foldlHelp : (a -> b -> b) -> b -> List (Tree a) -> List (List (Tree a)) -> b
-- foldlHelp f acc trees nextSets =
--     case trees of
--         [] ->
--             case nextSets of
--                 set :: sets ->
--                     foldlHelp f acc set sets
--                 [] ->
--                     acc
--         (Tree d []) :: rest ->
--             foldlHelp f (f d acc) rest nextSets
--         (Tree d xs) :: rest ->
--             foldlHelp f (f d acc) xs (rest :: nextSets)


{-| Flattens the tree into a list. This is equivalent to `foldr (::) []`
-}
flatten : Tree a -> List a
flatten t =
    foldr (::) [] t


{-| Returns the nodes that have no children.

    tree 1
        [ singleton 2
        , tree 3
            [ singleton 4
            , tree 5
                [ singleton 6]
            ]
        , singleton 7
        ]
        |> leaves
    --> [ 2, 7, 4, 6 ]

-}
leaves : Tree a -> List a
leaves t =
    breadthFirstFold
        (\s _ l c ->
            case c of
                [] ->
                    Continue (l :: s)

                _ ->
                    Continue s
        )
        []
        t
        |> List.reverse


{-| Returns pairs representing parent-child relationships in the tree.

The left item is the label of the parent, the right item is the label of
the child. Useful for visualising trees.

    tree 1
        [ singleton 2
        , tree 3
            [ singleton 4
            , tree 5
                [ singleton 6]
            ]
        , singleton 7
        ]
        |> links
    --> [ ( 1, 2 ), ( 1, 3 ), ( 1, 7 ), ( 3, 4 ), ( 3, 5 ), ( 5, 6 ) ]

-}
links : Tree a -> List ( a, a )
links t =
    breadthFirstFold
        (\s a l _ ->
            case a of
                parent :: _ ->
                    Continue (( parent, l ) :: s)

                _ ->
                    Continue s
        )
        []
        t
        |> List.reverse



-- linksHelp : a -> List ( a, a ) -> List (Tree a) -> List ( a, List (Tree a) ) -> List ( a, a )
-- linksHelp parent soFar trees nextSets =
--     case trees of
--         [] ->
--             case nextSets of
--                 ( newParent, set ) :: sets ->
--                     linksHelp newParent soFar set sets
--                 [] ->
--                     List.reverse soFar
--         (Tree d []) :: rest ->
--             linksHelp parent (( parent, d ) :: soFar) rest nextSets
--         (Tree d xs) :: rest ->
--             linksHelp parent (( parent, d ) :: soFar) rest (( d, xs ) :: nextSets)


{-| Create a tree from a seed.

Running the function on the seed should return a label and a list of seeds to
use for the children.

For example, this function takes and int, and uses the string representation of
that int as the label, with its children representing the integers from 0 up to
but not including the value. The expected result is a tree in which each label
has the number of children mentioned in the label, recursively.

    unfolder : Int -> (String, List Int)
    unfolder x =
        ( String.fromInt x, List.range 0 (x - 1) )


    unfold unfolder 3
    --> tree "3"
    -->     [ singleton "0"
    -->     , tree "1" [ singleton "0" ]
    -->     , tree "2"
    -->         [ singleton "0"
    -->         , tree "1" [ singleton "0" ]
    -->         ]
    -->     ]

-}
unfold : (b -> ( a, List b )) -> b -> Tree a
unfold f seed =
    depthFirstTraversal
        (\s _ l _ ->
            let
                ( l_, c ) =
                    f l
            in
            ( s, l_, List.map singleton c )
        )
        defaultBottomUp
        ()
        (singleton seed)
        |> Tuple.second


{-| Run a function on every label in the tree.

    tree 1
        [ singleton 2
        , tree 3 [ singleton 4 ]
        , singleton 5
        ]
        |> map (\x -> String.fromInt (x * 2))
    --> tree "2"
    -->     [ singleton "4"
    -->     , tree "6" [ singleton "8" ]
    -->     , singleton "10"
    -->     ]

-}
map : (a -> b) -> Tree a -> Tree b
map f t =
    mapAccumulate (\_ e -> ( (), f e )) () t
        |> Tuple.second


{-| Run a function on every label in the tree while getting access to the
"index" of the label. This looks at thing in the same order as `foldl`.

    tree "foo"
        [ singleton "bar"
        , tree "baz" [ singleton "hello", singleton "world" ]
        , singleton "qlux"
        ]
        |> indexedMap (\idx val -> String.fromInt idx ++ " - " ++ val)
    --> tree "0 - foo"
    -->     [ singleton "1 - bar"
    -->     , tree "2 - baz"
    -->         [ singleton "3 - hello"
    -->         , singleton "4 - world"
    -->         ]
    -->     , singleton "5 - qlux"
    -->     ]

-}
indexedMap : (Int -> a -> b) -> Tree a -> Tree b
indexedMap f t =
    mapAccumulate (\idx elem -> ( idx + 1, f idx elem )) 0 t
        |> Tuple.second


{-| Map a function over every node while accumulating some value.

    tree 1
        [ singleton 2
        , tree 3 [ singleton 4 ]
        ]
        |> mapAccumulate (\acc label -> ( acc + label, String.fromInt label)) 0
    --> ( 10
    --> , tree "1"
    -->     [ singleton "2"
    -->     , tree "3" [ singleton "4" ]
    -->     ]
    --> )

-}
mapAccumulate : (s -> a -> ( s, b )) -> s -> Tree a -> ( s, Tree b )
mapAccumulate f state t =
    depthFirstTraversal
        (\s _ l c ->
            let
                ( s_, l_ ) =
                    f s l
            in
            ( s_, l_, c )
        )
        defaultBottomUp
        state
        t


{-| Map over 2 trees. Much like `List.map2`, the result will be truncated to the shorter result.

    left : Tree Int
    left =
        tree 3
            [ singleton 5
            , tree 6 [ singleton 12 ]
            , singleton 4
            ]

    right : Tree Int
    right =
        tree 8
            [ tree 5 [ singleton 9 ]
            , singleton 3
            ]


    map2 (\x y -> x + y) left right
    --> tree 11
    -->     [ singleton 10
    -->     , singleton 9
    -->     ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 f left right =
    mapAccumulate2 (\s a b -> ( s, f a b )) () left right
        |> Tuple.second


{-| Like `map2`, but with the "index" added as the first argument.
-}
indexedMap2 : (Int -> a -> b -> c) -> Tree a -> Tree b -> Tree c
indexedMap2 f left right =
    mapAccumulate2 (\s a b -> ( s + 1, f s a b )) 0 left right
        |> Tuple.second


{-| Given a tree of functions and a tree of values, applies the functions to the
matching labels in the tree of values, truncating branches to match the common
shape of the trees.
-}
andMap : Tree (a -> b) -> Tree a -> Tree b
andMap =
    map2 (<|)


{-| Allows mapping over 2 trees while also accumulating a value.

    left : Tree Int
    left =
        tree 3
            [ singleton 5
            , tree 6 [ singleton 12 ]
            , singleton 4
            ]

    right : Tree Int
    right =
        tree 8
            [ tree 5 [ singleton 9 ]
            , singleton 3
            ]


    mapAccumulate2 (\sum x y -> ( sum + x + y, x + y )) 0 left right
    --> ( 30
    --> , tree 11
    -->     [ singleton 10
    -->     , singleton 9
    -->     ]
    --> )

-}
mapAccumulate2 : (s -> a -> b -> ( s, c )) -> s -> Tree a -> Tree b -> ( s, Tree c )
mapAccumulate2 f s_ (Tree a xs) (Tree b ys) =
    let
        ( s, z ) =
            f s_ a b
    in
    mapAccumulate2Help f
        s
        { todoL = xs
        , todoR = ys
        , done = []
        , label = z
        }
        []


mapAccumulate2Help :
    (s -> a -> b -> ( s, c ))
    -> s
    -> Map2Acc a b c
    -> List (Map2Acc a b c)
    -> ( s, Tree c )
mapAccumulate2Help f state acc stack =
    case ( acc.todoL, acc.todoR ) of
        ( [], _ ) ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulate2Help f state { top | done = node :: top.done } rest

        ( _, [] ) ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulate2Help f state { top | done = node :: top.done } rest

        ( (Tree a xs) :: restL, (Tree b ys) :: restR ) ->
            let
                ( state_, label_ ) =
                    f state a b
            in
            mapAccumulate2Help f
                state_
                { todoL = xs
                , todoR = ys
                , done = []
                , label = label_
                }
                ({ acc | todoL = restL, todoR = restR } :: stack)


type alias Map2Acc a b c =
    { todoL : List (Tree a)
    , todoR : List (Tree b)
    , done : List (Tree c)
    , label : c
    }


{-| Counts the number of levels in a tree (where the root is 0).

    depth (tree 2 [ tree 1 [tree 0 []]])
    --> 2

-}
depth : Tree a -> Int
depth t =
    depthFirstFold
        (\s a _ c ->
            case c of
                [] ->
                    Continue (max s (List.length a))

                _ ->
                    Continue s
        )
        0
        t


{-| Sorts all children of each node based on the comparator function (the function recieves a list of ancestors).

    tree 1
        [ tree 3
            [ singleton 5
            , singleton 4
            ]
        , singleton 2
        , singleton 6
        ]
        |> sortWith (\_ a b -> compare (label a) (label b))
    --> tree 1
    -->   [ singleton 2
    -->    , tree 3
    -->        [ singleton 4
    -->        , singleton 5
    -->        ]
    -->    , singleton 6
    -->    ]

-}
sortWith : (List a -> Tree a -> Tree a -> Order) -> Tree a -> Tree a
sortWith compareFn t =
    depthFirstTraversal defaultTopDown (\s a l c -> ( s, tree l (List.sortWith (compareFn a) c) )) () t
        |> Tuple.second


{-| Finds a subtree whose label matches the predicate.

Searches the tree in a breadth-first manner.

    tree 1
        [ tree 3
            [ singleton 5
            , singleton 4
            ]
        , singleton 2
        , singleton 6
        ]
        |> findBfs (\a -> label a == 3)
    --> Just (tree 3 [ singleton 5, singleton 4 ])

-}
findBfs : (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
findBfs predicate t =
    breadthFirstFold
        (\s _ l c ->
            if predicate (tree l c) then
                Stop (Just (tree l c))

            else
                Continue s
        )
        Nothing
        t


{-| Restructure a `Tree` into another type of structure.

Imagine you have a `Tree String` and you can to turn it into nested `<ul>`s.
This function can help!

    import Html exposing (Html)


    labelToHtml : String -> Html msg
    labelToHtml l =
        Html.text l


    toListItems : Html msg -> List (Html msg) -> Html msg
    toListItems label children =
        case children of
            [] ->
                Html.li [] [ label ]
            _ ->
                Html.li []
                    [ label
                    , Html.ul [] children
                    ]


    tree "root"
        [ tree "folder"
            [ singleton "foo"
            , singleton "bar"
            ]
        , singleton "yeah"
        ]
        |> restructure labelToHtml toListItems
        |> \root -> Html.ul [] [ root ]
    --> Html.ul []
    -->     [ Html.li []
    -->         [ Html.text "root"
    -->         , Html.ul []
    -->             [ Html.li []
    -->                 [ Html.text "folder"
    -->                 , Html.ul []
    -->                     [ Html.li [] [ Html.text "foo" ]
    -->                     , Html.li [] [ Html.text "bar" ]
    -->                     ]
    -->                 ]
    -->             , Html.li [] [ Html.text "yeah" ]
    -->             ]
    -->         ]
    -->     ]

Or perhaps you have your own tree datastructure and you want to convert to it:

    type MyTree a = MyTree a (List (MyTree a))


    tree "root"
        [ tree "folder"
            [ singleton "foo"
            , singleton "bar"
            ]
        , singleton "yeah"
        ]
        |> restructure identity MyTree
    --> MyTree "root"
    -->     [ MyTree "folder"
    -->         [ MyTree "foo" []
    -->         , MyTree "bar" []
    -->         ]
    -->     , MyTree "yeah" []
    -->     ]

-}
restructure : (a -> b) -> (b -> List c -> c) -> Tree a -> c
restructure convertLabel convertTree t =
    depthFirstTraversal (\s _ l c -> ( s, convertLabel l, c )) (\s _ l c -> ( s, convertTree l c )) () t
        |> Tuple.second


{-| The first callback to this function is used when going down the tree, effectively guiding and transforming it.
The second callback is used to reconstruct the tree when going back up.

This is similar like `restructure`, except this function:

  - has an accumulator argument
  - the first function can also change the children, not just the label (this allows us to implement `unfold` in terms of this function for instance)

-}
depthFirstTraversal :
    (s -> List b -> a -> List (Tree a) -> ( s, b, List (Tree a) ))
    -> (s -> List b -> b -> List c -> ( s, c ))
    -> s
    -> Tree a
    -> ( s, c )
depthFirstTraversal convertLabel convertTree s (Tree l c) =
    let
        ( state_, label_, children_ ) =
            convertLabel s [] l c
    in
    depthFirstTraversalHelp convertLabel
        convertTree
        state_
        { todo = children_
        , label = label_
        , done = []
        }
        []


type alias Acc a b c =
    { todo : List (Tree a)
    , done : List c
    , label : b
    }


defaultTopDown : s -> List a -> a -> List (Tree a) -> ( s, a, List (Tree a) )
defaultTopDown s _ l c =
    ( s, l, c )


defaultBottomUp : s -> List b -> b -> List (Tree b) -> ( s, Tree b )
defaultBottomUp s _ l c =
    ( s, tree l c )


depthFirstTraversalHelp :
    (s -> List b -> a -> List (Tree a) -> ( s, b, List (Tree a) ))
    -> (s -> List b -> b -> List c -> ( s, c ))
    -> s
    -> Acc a b c
    -> List (Acc a b c)
    -> ( s, c )
depthFirstTraversalHelp fLabel fTree state acc stack =
    case acc.todo of
        [] ->
            let
                ( state_, node ) =
                    fTree state (List.map .label stack) acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state_, node )

                top :: rest ->
                    depthFirstTraversalHelp
                        fLabel
                        fTree
                        state_
                        { top | done = node :: top.done }
                        rest

        (Tree l chs) :: rest ->
            let
                ancestors =
                    acc.label :: List.map .label stack

                ( state0, label_, children_ ) =
                    fLabel state ancestors l chs
            in
            case children_ of
                [] ->
                    let
                        ( state_, newTree ) =
                            fTree state0 ancestors label_ []
                    in
                    depthFirstTraversalHelp
                        fLabel
                        fTree
                        state_
                        { acc
                            | todo = rest
                            , done = newTree :: acc.done
                        }
                        stack

                cs ->
                    depthFirstTraversalHelp
                        fLabel
                        fTree
                        state0
                        { todo = cs
                        , done = []
                        , label = label_
                        }
                        ({ acc | todo = rest } :: stack)


{-| Controls if the fold should continue traversing the tree, or should abort immediately.
-}
type Step a
    = Continue a
    | Stop a


{-| Traverses the tree by "levels".

     tree 1
        [ tree 3
            [ singleton 5
            , singleton 4
            ]
        , singleton 2
        , singleton 6
        ]
    |> breadthFirstFold (\s a l c -> Continue (l :: s)) []
    --> [4, 5, 6, 2, 3, 1]

(The list is reversed here due to how `::` works)

-}
breadthFirstFold :
    (s -> List a -> a -> List (Tree a) -> Step s)
    -> s
    -> Tree a
    -> s
breadthFirstFold f acc t =
    breadthFirstFoldHelp f acc [] [ t ] []


breadthFirstFoldHelp : (s -> List a -> a -> List (Tree a) -> Step s) -> s -> List a -> List (Tree a) -> List ( List a, List (Tree a) ) -> s
breadthFirstFoldHelp f acc parents trees nextSets =
    case trees of
        [] ->
            case nextSets of
                ( p, set ) :: sets ->
                    breadthFirstFoldHelp f acc p set sets

                [] ->
                    acc

        (Tree d ch) :: rest ->
            case f acc parents d ch of
                Continue a ->
                    case ch of
                        [] ->
                            breadthFirstFoldHelp f a parents rest nextSets

                        xs ->
                            breadthFirstFoldHelp f a parents rest (( d :: parents, xs ) :: nextSets)

                Stop a ->
                    a


{-| Traverses the tree by by going down as far as possible before trying
out any siblings.

     tree 1
        [ tree 3
            [ singleton 5
            , singleton 4
            ]
        , singleton 2
        , singleton 6
        ]
    |> depthFirstFold (\s a l c -> Continue (l :: s)) []
    --> [6, 2, 4, 5, 3, 1]

(The list is reversed here due to how `::` works)

-}
depthFirstFold :
    (s -> List a -> a -> List (Tree a) -> Step s)
    -> s
    -> Tree a
    -> s
depthFirstFold f acc t =
    depthFirstFoldHelp f acc [] [ t ] []


depthFirstFoldHelp : (s -> List a -> a -> List (Tree a) -> Step s) -> s -> List a -> List (Tree a) -> List ( List a, List (Tree a) ) -> s
depthFirstFoldHelp f acc parents trees nextSets =
    case trees of
        [] ->
            case nextSets of
                ( p, set ) :: sets ->
                    depthFirstFoldHelp f acc p set sets

                [] ->
                    acc

        (Tree d ch) :: rest ->
            case f acc parents d ch of
                Continue a ->
                    case ch of
                        [] ->
                            depthFirstFoldHelp f a parents rest nextSets

                        xs ->
                            depthFirstFoldHelp f a (d :: parents) xs (( parents, rest ) :: nextSets)

                Stop a ->
                    a
