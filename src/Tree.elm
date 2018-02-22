module Tree
    exposing
        ( Tree(..)
        , children
        , count
        , datum
        , foldl
        , foldr
        , indexedMap
        , map
        , singleton
        , traverse
        )


type Tree a
    = Tree a (List (Tree a))


singleton : a -> Tree a
singleton v =
    Tree v []


datum : Tree a -> a
datum (Tree v _) =
    v


children : Tree a -> List (Tree a)
children (Tree _ c) =
    c


count : Tree a -> Int
count t =
    foldl (\_ x -> x + 1) 0 t


foldl : (a -> b -> b) -> b -> Tree a -> b
foldl f acc t =
    foldlHelp f acc [ t ] []


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


toList : Tree a -> List a
toList t =
    foldr (::) [] t


map : (a -> b) -> Tree a -> Tree b
map f t =
    traverse (\_ e -> ( (), f e )) () t
        |> Tuple.second


indexedMap : (Int -> a -> b) -> Tree a -> Tree b
indexedMap f t =
    traverse (\idx elem -> ( idx + 1, f idx elem )) 0 t
        |> Tuple.second


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
                ( state_, datum ) =
                    f state d
            in
            { acc
                | todo = rest
                , done = Tree datum [] :: acc.done
            }
                |> traverseHelp f state_

        (Tree d cs) :: rest ->
            let
                ( state_, datum ) =
                    f state d
            in
            { todo = cs
            , done = []
            , self = datum
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
