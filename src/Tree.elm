module Tree
    exposing
        ( Tree
        , children
        , count
        , flatten
        , foldl
        , foldr
        , indexedMap
        , label
        , map
        , singleton
        , traverse
        , tree
        )

{-| TODO: docs


# Structure

@docs Tree, singleton, tree, label, children


# Folds

@docs foldl, foldr, count, flatten


# Mapping and traversing

@docs map, indexedMap, traverse

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


{-| TODO: docs
-}
tree : a -> List (Tree a) -> Tree a
tree =
    Tree


{-| TODO: docs
-}
label : Tree a -> a
label (Tree v _) =
    v


{-| TODO: docs
-}
children : Tree a -> List (Tree a)
children (Tree _ c) =
    c


{-| TODO: docs
-}
count : Tree a -> Int
count t =
    foldl (\_ x -> x + 1) 0 t


{-| TODO: docs
-}
foldl : (a -> b -> b) -> b -> Tree a -> b
foldl f acc t =
    foldlHelp f acc [ t ] []


{-| TODO: docs
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

        (Tree d (x :: xs)) :: rest ->
            foldlHelp f acc (x :: xs) ((Tree d [] :: rest) :: nextSets)


{-| TODO: docs
-}
flatten : Tree a -> List a
flatten t =
    foldr (::) [] t


{-| TODO: make tail recursive
-}
unfold : (b -> ( a, List b )) -> b -> Tree a
unfold f seed =
    let
        ( v, next ) =
            f seed
    in
    Tree v (List.map (unfold f) next)


{-| TODO: docs
-}
map : (a -> b) -> Tree a -> Tree b
map f t =
    traverse (\_ e -> ( (), f e )) () t
        |> Tuple.second


{-| TODO: docs
-}
indexedMap : (Int -> a -> b) -> Tree a -> Tree b
indexedMap f t =
    traverse (\idx elem -> ( idx + 1, f idx elem )) 0 t
        |> Tuple.second


{-| TODO: docs
-}
traverse : (s -> a -> ( s, b )) -> s -> Tree a -> ( s, Tree b )
traverse f s (Tree d cs) =
    let
        ( s_, d_ ) =
            f s d
    in
    traverseHelp f
        s_
        { todo = cs
        , done = []
        , self = d_
        , stack = Top
        }


traverseHelp : (s -> a -> ( s, b )) -> s -> Acc a b -> ( s, Tree b )
traverseHelp f state acc =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.self (List.reverse acc.done)
            in
            case acc.stack of
                Top ->
                    ( state, node )

                Stack t ->
                    { t | done = node :: t.done }
                        |> traverseHelp f state

        (Tree d []) :: rest ->
            let
                ( state_, label ) =
                    f state d
            in
            { acc
                | todo = rest
                , done = Tree label [] :: acc.done
            }
                |> traverseHelp f state_

        (Tree d cs) :: rest ->
            let
                ( state_, label ) =
                    f state d
            in
            { todo = cs
            , done = []
            , self = label
            , stack = Stack { acc | todo = rest }
            }
                |> traverseHelp f state_


type alias Acc a b =
    { todo : List (Tree a)
    , done : List (Tree b)
    , self : b
    , stack : Stack a b
    }


type Stack a b
    = Stack (Acc a b)
    | Top
