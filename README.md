# hs-tree

Create Trees and show their different traversals

## example
```
❯ stack run avl 0 1 2 3 4 5
Tree
========================================
Rotating left
Rotating left
Rotating left
3 (0)
⌞ 1 (0)
| ⌞ 0 (0)
| | ⌞ ○
| | ⌞ ○
| ⌞ 2 (0)
|   ⌞ ○
|   ⌞ ○
⌞ 4 (1)
  ⌞ ○
  ⌞ 5 (0)
    ⌞ ○
    ⌞ ○
Traversals
========================================
Traverse in order (left, root, right)
[0,1,2,3,4,5]
Traverse pre order (root, left right)
[3,1,0,2,4,5]
Traverse post order (left, right, root)
[0,2,1,5,4,3]
Traverse level order (levels, top to bottom)
[3,1,4,0,2,5]
```

