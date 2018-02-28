# Multiway trees and a zipper

This library provides a multiway tree (also known as rosetree) datastructure, 
and a zipper to navigate through it.

A multiway tree is a non-empty tree where every node has a `label` and a list of
children, each of which is a tree.

The basic structure looks like this:

```elm
type Tree a = Tree { label : a, children : List (Tree a) }
```

You can think of it as a multidimensional non-empty list. The root always has a 
label and branches of in multiple directions.

A tree zipper is a datastructure that allows navigating through a tree. It 
represents a waypoint along walking the tree. It can focus on an arbitratry part
of the tree, be used to modify and transform that part of the tree, and rebuild
the entire tree back up to the root.

---

Made with love and released under the BSD-3 license.
