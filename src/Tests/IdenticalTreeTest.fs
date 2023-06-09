module Tests.IdenticalTreeTest

open FsCheck.NUnit
open Library.TreeDesign

let makeroot (Node((l,_), ts)) = Node((l,0.), ts)

let rec isStructurallyEqual (Node(_, ts)) (Node(_, ts')) =
    List.length ts = List.length ts'
    && List.forall2 (isStructurallyEqual) ts ts'

let rec isPositionallyEqual (Node((_, p), ts)) (Node((_, p'), ts')) =
    List.length ts = List.length ts'
    && p = p'
    && List.forall2 (isPositionallyEqual) ts ts'

let rec unfoldTree (Node(_, ts) as t) = 
    seq {
        yield t
        for child in ts do 
            yield! unfoldTree child
    }


[<Property>]
let ``Structurally equal subtrees renders the same`` (t: Tree<string>) =
    let dt = design t
    let subTrees = unfoldTree dt

    Seq.allPairs subTrees subTrees
    |> Seq.filter (fun (t, t') -> isStructurallyEqual t t')
    |> Seq.map (fun (t, t') -> (makeroot t, makeroot t'))
    |> Seq.forall (fun (t, t') -> isPositionallyEqual t t')
