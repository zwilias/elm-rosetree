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
map f (Tree d cs) =
    mapHelp f
        { todo = cs
        , done = []
        , self = f d
        , stack = Top
        }


type alias Acc a b =
    { todo : List (Tree a)
    , done : List (Tree b)
    , self : b
    , stack : Stack a b
    }


type Stack a b
    = Stack (Acc a b)
    | Top


mapHelp : (a -> b) -> Acc a b -> Tree b
mapHelp f acc =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.self (List.reverse acc.done)
            in
            case acc.stack of
                Top ->
                    node

                Stack t ->
                    { t | done = node :: t.done }
                        |> mapHelp f

        (Tree d []) :: rest ->
            { acc
                | todo = rest
                , done = Tree (f d) [] :: acc.done
            }
                |> mapHelp f

        (Tree d cs) :: rest ->
            { todo = cs
            , done = []
            , self = f d
            , stack = Stack { acc | todo = rest }
            }
                |> mapHelp f


indexedMap : (Int -> a -> b) -> Tree a -> Tree b
indexedMap f (Tree d cs) =
    indexedMapHelp f
        1
        { todo = cs
        , done = []
        , self = f 0 d
        , stack = Top
        }


indexedMapHelp : (Int -> a -> b) -> Int -> Acc a b -> Tree b
indexedMapHelp f idx acc =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.self (List.reverse acc.done)
            in
            case acc.stack of
                Top ->
                    node

                Stack t ->
                    { t | done = node :: t.done }
                        |> indexedMapHelp f idx

        (Tree d []) :: rest ->
            { acc
                | todo = rest
                , done = Tree (f idx d) [] :: acc.done
            }
                |> indexedMapHelp f (idx + 1)

        (Tree d cs) :: rest ->
            { todo = cs
            , done = []
            , self = f idx d
            , stack = Stack { acc | todo = rest }
            }
                |> indexedMapHelp f (idx + 1)
