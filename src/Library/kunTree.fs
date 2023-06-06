// 'a代表泛型
// Node 和 PosNode 是代表树节点的数据类型构造器，由 of 关键字来定义构造器
// real ~= float
type 'a Tree = Node of 'a * ('a Tree list)
// 'a * real * ('a PosTree list)
//  - 'a => value of node
//  - real => positioin
//  - ('a PosTree list) => sub trees
type 'a PosTree = PosNode of 'a * real * ('a PosTree list)

// transforms a tree into a positioned tree
// 问题在于怎么构建这个距离？？？？
let moveTree (tree: 'a Tree) : 'a PosTree = 
    // todo:

// extent
type Extent = (real*real) list
// move extent
let moveExtent e x = List.map (fun (p, q) => (p + x, q + x)) e

