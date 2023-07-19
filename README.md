# Multiway trees and a zipper

This library provides a multiway tree (also known as rosetree) datastructure,
and a zipper to navigate through it.

A multiway tree is a non-empty tree where every node has a `label` and a list of
children, each of which is a tree.

The basic structure looks like this:

```elm
type Tree a = Tree a (List (Tree a))
```

You can think of it as a multidimensional non-empty list. The root always has a
label and may branch out in multiple directions.

A tree zipper is a datastructure that allows navigating through a tree. It
represents a waypoint along walking the tree. It can focus on an arbitratry part
of the tree, be used to modify and transform that part of the tree, and rebuild
the entire tree back up to the root.

# Fork of zwilias/elm-rosetree

This is a fork of [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).
This library should be mostly a drop-in replacement, with the following changes:

1. zwilias/elm-rosetree is the only library I could find that defines
   `andMap : Tree (a -> b) -> Tree a -> Tree b`. This fork uses the standard
   `andMap : Tree a -> Tree (a -> b) -> Tree b`.

2. A number of functions have been renamed from `mapX` to `updateX`. `map` in
   my mind represents a transformation that may change type and affects the whole
   datastructure. Whereas `mapLabel` and `mapChildren` confusingly only affected
   the root of the tree and could not change the type contained.

3. `flatten` has been renamed `toList`. In my mind `flatten : Tree (Tree a) -> Tree a`.

4. `count` is now `length`, since that's what pretty much every other datastructure calls it.

5. `Tree` now exposes it's constructor. I don't think this can break anything, but
   Elm considers it a breaking change...

Furthermore there are some improvements in this library you may find helpful:

- we expose some very powerful new functions like `depthFirstTraversal`
  and `breadthFirstFold`. These are flexible enough that most of the library is
  implemented on top of these. This should have benefits for code size and performance.

- we've added a bunch of useful utilities like `depth`, `links`, `sortWith` and `find`.

- we've added useful ways to help deserialize trees from flat file formats with
  `stratify` and `stratifyWithPath`.

# Performance and stack-safety

Since trees can grow pretty large, a lot of effort has gone into making all the
functions in this library work stack-safely. All the iteration functions in
this library are written to make use of Elm's support for tail call elimination
(we enforce this via elm-review).

---

Released under the BSD-3 license.
