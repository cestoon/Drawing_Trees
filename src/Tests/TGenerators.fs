module Tests.TGenerators

open FsCheck
open Library.TreeDesign
open System

// --------------- NormalFloat Generator ---------------
let normalFloatGenerator =
    Gen.map NormalFloat.op_Explicit
            Arb.generate<NormalFloat>

type NormalFloatGenerators =
    static member float() = Arb.fromGen normalFloatGenerator

let safeSymmetricTreeGenerator() =
    let rec treeGen (n: int) =
        match n with
        | 0 ->
            let label = Arb.generate<int>
            Gen.map (fun (label) -> Node(label, [])) label
        | _ ->  
            let rnd = new Random()
            let nNext = 
                rnd.Next(0, n)
            let label = Gen.constant nNext
            let subtree = 
                treeGen nNext
                |> Gen.sample 0 nNext
                |> Gen.constant
            Gen.map2 (fun label subtree -> Node(label,subtree)) label subtree
    treeGen 20

let rec subtreeSeq (t:Tree<int>) = 
    match t with 
    | Node(label, subtreeList) -> seq subtreeList

type SymmetricTreeGenerator =
    static member Tree() =
        {new Arbitrary<Tree<int>>() with
           override x.Generator = safeSymmetricTreeGenerator()
           override x.Shrinker (t: Tree<int>) = subtreeSeq t
        }

Arb.register<NormalFloatGenerators>() |> ignore
Arb.register<SymmetricTreeGenerator>() |> ignore
