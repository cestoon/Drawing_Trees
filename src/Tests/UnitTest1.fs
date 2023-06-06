module Tests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    printfn "123"
    Assert.Pass()


type TreeDesignTests() = 

    [<Property>]
    let ``movetree should not change the tree label`` (label: string, x: float, y: float, subtrees: list<Tree<'a>>) =
        let tree = Node((label, x), subtrees)
        let movedTree = movetree (tree, y)
        match movedTree with
        | Node((movedLabel, _), _) -> movedLabel = label

    [<Property>]
    let ``movetree should add y to the tree x position`` (label: string, x: float, y: float, subtrees: list<Tree<'a>>) =
        let tree = Node((label, x), subtrees)
        let movedTree = movetree (tree, y)
        match movedTree with
        | Node((_, movedX), _) -> movedX = x + y
