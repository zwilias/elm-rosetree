# Multiway trees and a zipper [![Build Status](https://travis-ci.org/zwilias/elm-rosetree.svg?branch=master)](https://travis-ci.org/zwilias/elm-rosetree)

This library provides a multiway tree (also known as rosetree) datastructure, 
and a zipper to navigate through it.

A multiway tree is a non-empty tree where every node has a `label` and a list of
children, each of which is a tree.

The basic structure looks like this:

```elm
type Tree a = Tree { label : a, children : List (Tree a) }
```

You can think of it as a multidimensional non-empty list. The root always has a 
label and may branch out in multiple directions.

A tree zipper is a datastructure that allows navigating through a tree. It 
represents a waypoint along walking the tree. It can focus on an arbitratry part
of the tree, be used to modify and transform that part of the tree, and rebuild
the entire tree back up to the root.

# Performance and stack-safety

Since trees can grow pretty large, a lot of effort has gone into making all the
functions in the `Tree` module work stack-safely. All the iteration functions in 
that module are written to make use of Elm's support for tail call elimination.

Making these functions highly performant without losing out on the safety is a
continuous effort.


# Contributing

Contributions are preferred to take the form of collaboration. Questions, bug 
reports and feature requests are welcome as GitHub issues. Before starting any
concrete work, opening a discussion is very much appreciated.

---

Made with love and released under the BSD-3 license.
