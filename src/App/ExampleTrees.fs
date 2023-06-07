module ExampleTrees
    open Library.TreeDesign
    open System

    let t1 = Node ("t1", [
            Node("a", [Node("aa",[])])
            Node("b", [])
            Node("c", [Node("ca", [Node("caa", [])])])
        ])
    let t2 = Node ("t2", [
        Node("a", [])
        Node("b", [])
        Node("c", [])
        Node("d", [])
    ])

    let t3 = Node("t3", [t1; t2])

    let makeTreeFromList list =
        let rec makeTreeFromList' childCount xs=
            let children = 
                (childCount, xs)
                |> List.unfold (function 
                    | (0, _) -> None
                    | (count, []) -> Some(Node("empty", []), (count-1, []))
                    | (count, y::ys) -> Some((makeTreeFromList' y ys), (count-1, ys)))
            Node (string childCount, children) 
        match list with
        | [] -> makeTreeFromList' 0 []
        | head::rest -> makeTreeFromList' head rest

    let getRandomListOfLength n = 
        let random = Random n
        let getRandomInt () = (random.Next 5) + 1
        [for _ in 0..n do getRandomInt()]  //List comprehension generates list of random values
    
    let getRandomList () = 
        let random = Random() 
        let length = random.Next 10
        getRandomListOfLength length

    let getRandomTreeOfDepth n =
        n
        |> getRandomListOfLength
        |> makeTreeFromList

    let getRandomTree () =
        getRandomList()
        |> makeTreeFromList 