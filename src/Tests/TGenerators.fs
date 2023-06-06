module TGenerators

open FsCheck
open Library.TreeDesign

// --------------- NormalFloat Generator ---------------
let normalFloatGenerator =
    Gen.map NormalFloat.op_Explicit
            Arb.generate<NormalFloat>

type NormalFloatGenerators =
    static member float() = Arb.fromGen normalFloatGenerator

// --------------- Tree Generator ---------------
let tree<'a> =
    let rec tree' s =
        match s with
        | 0 -> Gen.map (fun v -> Node(v, [])) Arb.generate<'a>
        | n when n>0 ->
            let subtrees = tree' (n/2)  |> Gen.sample 0 5 |> Gen.constant 
            Gen.map2 (fun v ts -> Node(v, ts)) Arb.generate<'a> subtrees 
        | _ -> invalidArg "s" "Only positive args are allowed"
    Gen.sized tree'

type TreeGenerator =
    static member Tree() =
        {new Arbitrary<Tree<char>>() with
            override x.Generator = tree<char>
            override x.Shrinker t = Seq.empty }

Arb.register<TreeGenerator>() |> ignore