module Tree
    exposing
        ( Tree
        , andMap
        , appendChild
        , children
        , count
        , flatten
        , foldl
        , foldr
        , indexedMap
        , indexedMap2
        , label
        , map
        , map2
        , mapAccumulate
        , mapAccumulate2
        , mapChildren
        , mapLabel
        , prependChild
        , replaceChildren
        , replaceLabel
        , restructure
        , singleton
        , tree
        , unfold
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


# Folds

@docs foldl, foldr, count, flatten


# Mapping and traversing

@docs map, indexedMap, mapAccumulate, map2, indexedMap2, mapAccumulate2, andMap


# Fancy stuff

@docs unfold, restructure

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
    foldlHelp f acc [ t ] []


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


foldlHelp : (a -> b -> b) -> b -> List (Tree a) -> List (List (Tree a)) -> b
foldlHelp f acc trees nextSets =
    case trees of
        [] ->
            case nextSets of
                set :: sets ->
                    foldlHelp f acc set sets

                [] ->
                    acc

        (Tree d []) :: rest ->
            foldlHelp f (f d acc) rest nextSets

        (Tree d xs) :: rest ->
            foldlHelp f (f d acc) xs (rest :: nextSets)


{-| Flattens the tree into a list. This is equivalent to `foldr (::) []`
-}
flatten : Tree a -> List a
flatten t =
    foldr (::) [] t


{-| Create a tree from a seed.

Running the function on the seed should return a label and a list of seeds to
use for the children.

For example, this function takes and int, and uses the string representation of
that int as the label, with its children representing the integers from 0 up to
but not including the value. The expected result is a tree in which each label
has the number of children mentioned in the label, recursively.

    unfolder : Int -> (String, List Int)
    unfolder x =
        ( toString x, List.range 0 (x - 1) )


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
    let
        ( v, next ) =
            f seed
    in
    unfoldHelp f { todo = next, label = v, done = [] } []


unfoldHelp :
    (b -> ( a, List b ))
    -> UnfoldAcc a b
    -> List (UnfoldAcc a b)
    -> Tree a
unfoldHelp f acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    node

                top :: rest ->
                    unfoldHelp f
                        { top | done = node :: top.done }
                        rest

        x :: xs ->
            case f x of
                ( label, [] ) ->
                    unfoldHelp f
                        { acc
                            | todo = xs
                            , done = singleton label :: acc.done
                        }
                        stack

                ( label, todo ) ->
                    unfoldHelp f
                        { todo = todo
                        , label = label
                        , done = []
                        }
                        ({ acc | todo = xs } :: stack)


type alias UnfoldAcc a b =
    { todo : List b
    , done : List (Tree a)
    , label : a
    }


{-| Run a function on every label in the tree.

    tree 1
        [ singleton 2
        , tree 3 [ singleton 4 ]
        , singleton 5
        ]
        |> map (\x -> toString (x * 2))
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
        |> indexedMap (\idx val -> toString idx ++ " - " ++ val)
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


{-| Map a function over every note while accumulating some value.

    tree 1
        [ singleton 2
        , tree 3 [ singleton 4 ]
        ]
        |> mapAccumulate (\acc label -> ( acc + label, toString label)) 0
    --> ( 10
    --> , tree "1"
    -->     [ singleton "2"
    -->     , tree "3" [ singleton "4" ]
    -->     ]
    --> )

-}
mapAccumulate : (s -> a -> ( s, b )) -> s -> Tree a -> ( s, Tree b )
mapAccumulate f s (Tree d cs) =
    let
        ( s_, d_ ) =
            f s d
    in
    mapAccumulateHelp f
        s_
        { todo = cs
        , done = []
        , label = d_
        }
        []


mapAccumulateHelp :
    (s -> a -> ( s, b ))
    -> s
    -> MapAcc a b
    -> List (MapAcc a b)
    -> ( s, Tree b )
mapAccumulateHelp f state acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulateHelp f state { top | done = node :: top.done } rest

        (Tree d []) :: rest ->
            let
                ( state_, label ) =
                    f state d
            in
            mapAccumulateHelp f
                state_
                { acc
                    | todo = rest
                    , done = Tree label [] :: acc.done
                }
                stack

        (Tree d cs) :: rest ->
            let
                ( state_, label ) =
                    f state d
            in
            mapAccumulateHelp f
                state_
                { todo = cs
                , done = []
                , label = label
                }
                ({ acc | todo = rest } :: stack)


type alias MapAcc a b =
    { todo : List (Tree a)
    , done : List (Tree b)
    , label : b
    }


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
                ( state_, label ) =
                    f state a b
            in
            mapAccumulate2Help f
                state_
                { todoL = xs
                , todoR = ys
                , done = []
                , label = label
                }
                ({ acc | todoL = restL, todoR = restR } :: stack)


type alias Map2Acc a b c =
    { todoL : List (Tree a)
    , todoR : List (Tree b)
    , done : List (Tree c)
    , label : c
    }


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
restructure convertLabel convertTree (Tree l c) =
    restructureHelp convertLabel
        convertTree
        { todo = c
        , label = convertLabel l
        , done = []
        }
        []


restructureHelp :
    (a -> b)
    -> (b -> List c -> c)
    -> ReAcc a b c
    -> List (ReAcc a b c)
    -> c
restructureHelp fLabel fTree acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    fTree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    node

                top :: rest ->
                    restructureHelp
                        fLabel
                        fTree
                        { top | done = node :: top.done }
                        rest

        (Tree l []) :: rest ->
            restructureHelp
                fLabel
                fTree
                { acc
                    | todo = rest
                    , done = fTree (fLabel l) [] :: acc.done
                }
                stack

        (Tree l cs) :: rest ->
            restructureHelp
                fLabel
                fTree
                { todo = cs
                , done = []
                , label = fLabel l
                }
                ({ acc | todo = rest } :: stack)


type alias ReAcc a b c =
    { todo : List (Tree a)
    , done : List c
    , label : b
    }
