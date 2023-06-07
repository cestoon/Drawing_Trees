namespace Library

module TreeDesign =

    type Tree<'a> = Node of 'a * (Tree<'a> list)

    type Extent = (float * float) list

    let rec movetree (Node((label, x: float), subtrees), y: float) = 
        Node((label, x + y), subtrees)

    let moveextent (e: Extent, x) : Extent = 
        List.map (fun (p, q) -> p + x, q + x) e

    // Merge two non-overlapping extents by
    // picking the leftmost positions of the ﬁrst
    // extent and the rightmost positions of the second

    let rec merge (ps, qs) =
        match (ps, qs) with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge (ps, qs)


    // Extend merge to a list of extents by the following function:

    let mergelist es =
        List.fold (fun acc ex -> merge (acc, ex)) [] es

    // Fitting extents

    let rmax (p: float, q: float) = if p > q then p else q

    // The function accepts two extents as arguments
    // and returns the minimum possible distance between the two root nodes

    let rec fit =
        function
        | ((p, _) :: ps, (_, q) :: qs) -> rmax (fit (ps, qs), p - q + 1.0)
        | _ -> 0.0

    // Now we extend this function to a list of subtrees, calculating a list of positions
    // for each subtree relative to the leftmost subtree which has position zero. It works
    // by accumulating an extent, repeatedly ﬁtting subtrees against it. This produces an
    // asymmetric eﬀect because trees are ﬁtted together from the left.

    let fitlistl es =
        let rec fitlistl' acc =
            function
            | [] -> []
            | (e :: es) ->
                let x = fit (acc, e)
                x :: fitlistl' (merge (acc, moveextent (e, x))) es

        fitlistl' [] es


    // The opposite eﬀect is produced from the following function which calculates posi-
    // tions relative to the rightmost subtree, which has position zero. The function rev
    // is ordinary list reversal, and - is negation.
    let fitlistr es =
        let rec fitlistr' acc =
            function
            | [] -> []
            | (e :: es) ->
                let x = - fit(e, acc)
                x :: fitlistr' (merge (moveextent (e, x), acc)) es

        List.rev (fitlistr' [] (List.rev es))


    // In order to obtain a symmetric layout we calculate for each subtree the mean of
    // these two positionings:

    let mean (x, y) = (x + y) / 2.0

    let fitlist es =
        List.map mean (List.zip (fitlistl es) (fitlistr es))


    // Now combining all the functions

    let design tree =
        let rec design' (Node(label, subtrees)) =
            let (trees, extents) = 
                subtrees 
                |> List.map design' 
                |> List.unzip
            let positions = fitlist extents
            let ptrees = 
                positions
                |> List.zip trees  
                |> List.map movetree
            let pextents = 
                positions
                |> List.zip extents  
                |> List.map moveextent
            let resultextent = (0.0, 0.0)::mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)
        let (resulttree, _) = design' tree
        resulttree

    // Functions to reflect the tree to be used later

    /// Reflect a non-positioned tree
    let rec reflect (Node(v, subtrees)) =
        Node(v, List.map reflect (List.rev subtrees))
        
    /// Reflect a positioned tree
    let rec reflectpos (Node((v, x: float), subtrees)) =
        Node((v, -x), List.map reflectpos subtrees)

