module Tests.TreeIsomorphicTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign
open Lines

let rec checkPositionalEquality (Node((l, p), ts)) (Node((l', p'), ts')) =
    let notEqual t1 t2 = not <| checkPositionalEquality t1 t2

    l = l'
    && p = p'
    && not <| List.exists2 (notEqual) (List.sort ts) (List.sort ts')


// Isomorphic subtrees have isomorphic renderings. => Isomorphic
[<Property>]
let ``Isomorphic subtrees have isomorphic renderings`` (t: Tree<string>) =

    let t = Node("", [])
    let tt = Node("root", [ t; t ])

    let designedTree = design tt

    let getSubTrees' (Node((l, p), subTrees)) =
        match subTrees with
        | [ l; r ] -> (l, r)
        | _ -> failwith "too many children"

    let (x, y) = getSubTrees' designedTree

    let x' = movetree (x, 0.5)
    let y' = movetree (y, -0.5)

    checkPositionalEquality x' y'
