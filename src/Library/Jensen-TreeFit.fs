namespace Library.Jensen

module TreeFit = 
    // datatype ’a Tree = Node of ’a * (’a Tree list)
    type Tree<'a> = Node of 'a * 'a Tree list
    type RelativePosTree<'a> = Tree<'a * float>

    /// A helperfunction meant for testing implemented by Jensen
    let rec toPosTree n (Node (l, ts)): RelativePosTree<_> = 
        let len = List.length ts
        let ts' = 
            match len % 2 = 0 with
            | true -> List.mapi (fun i -> float (i - len / 2) + 0.5 |> toPosTree) ts
            | _ -> List.mapi (fun i -> i - len / 2 |> float |> toPosTree ) ts
        Node ((l, n), ts')

    //  fun movetree (Node((label, x), subtrees), x’ : real) =
    //      Node((label, x+x’), subtrees)
    let movetree ((Node((label, x), subtrees): RelativePosTree<_>), x') = 
        Node((label, x+x'), subtrees)

    
    /// An uncurried version of its namesake    
    let movetree' (Node((label, x), subtrees): RelativePosTree<_>) x' = 
        Node((label, x+x'), subtrees)

    //  type Extent = (real*real) list
    type Extent = (float * float) list

    //  fun moveextent (e : Extent, x) = map (fn (p,q) => (p+x,q+x)) e
    let moveextent ((e: Extent), x) = List.map (fun (p, q) -> (p+x,q+x)) e
    
    /// An uncurried version of its namesake
    let moveextent' (e: Extent) x = List.map (fun (p, q) -> (p+x,q+x)) e

    //  fun merge ([], qs)               = qs
    //    | merge (ps, [])               = ps
    //    | merge ((p,_)::ps, (_,q)::qs) = (p,q) :: merge (ps, qs)
    let rec merge ps qs =
        match (ps, qs) with
        | ([], _) -> qs
        | (_, []) -> ps
        | ((p,_)::pTail, (_,q)::qTail) -> (p,q)::merge pTail qTail

    //  fun mergelist es = fold merge es []
    let mergelist es  = List.fold (merge) [] es


    // fun rmax (p : real, q : real)   = if p > q then p else q
    // fun fit ((_,p)::ps) ((q,_)::qs) = rmax(fit ps qs, p - q + 1.0)
    //   | fit _           _           = 0.0

    let rmax p q = if p > q then p else q
    let rec fit (e1: Extent) (e2: Extent) = 
        match e1, e2 with
        | ((_, p)::ps, (q,_)::qs) -> rmax (fit ps qs) ((p - q) + 1.0)
        | _ -> 0.0

    /// Implementation by Jensen to allow any ordering of arguments
    let fit' e1 e2 = 
        match e1, e2 with 
        | ((_, p)::_, (q,_)::_) when p > q -> fit e1 e2
        |  _ -> fit e2 e1

    
    // fun fitlistl es =
    // let 
    //     fun fitlistl’ acc [] = []
    //       | fitlistl’ acc (e::es) =
    //       let val x = fit acc e
    //       in
    //           x :: fitlistl’ (merge (acc, moveextent (e,x))) es
    //       end
    // in
    //     fitlistl’ [] es
    // end
    let fitlistl es = 
        let rec fitlistl' acc = function
            | [] -> []
            | e::es -> 
                let x = fit acc e 
                x::fitlistl' (merge acc (moveextent (e, x))) es
        fitlistl' [] es

    // fun fitlistr es =
    // let
    //     fun fitlistr’ acc [] = []
    //       | fitlistr’ acc (e::es) =
    //       let val x = ~(fit e acc)
    //       in
    //         x :: fitlistr’ (merge (moveextent (e,x), acc)) es
    //       end
    // in
    //     rev (fitlistr’ [] (rev es))
    // end
    let fitlistr es = 
        let rec fitlistr' acc = function
            | [] -> []
            | e::es -> 
                let x = -(fit e acc)
                x::fitlistr' (merge (moveextent (e, x)) acc) es
        List.rev (fitlistr' [] (List.rev es))
    
    // val flipextent : Extent -> Extent = map (fn (p,q) => (~q,~p))
    // val fitlistr = rev o map ~ o fitlistl o map flipextent o rev

    let flipextent (e: Extent): Extent = List.map (fun (p, q) -> (-q, -p)) e
    /// A fitlistr defined in terms of fitlistl
    let fitlistr' (es: Extent list) = 
        es
        |> List.rev 
        |> List.map flipextent
        |> fitlistl
        |> List.map (fun p -> -p)
        |> List.rev

    // fun mean (x,y) = (x+y)/2.0
    // fun fitlist es = map mean (zip (fitlistl es, fitlistr es))
    let mean (x, y) = (x+y) / 2.0
    let fitlist (es: Extent list) = 
        es 
        |> fun xs -> List.zip (fitlistl xs) (fitlistr' xs)
        |> List.map (mean) 

    // fun design tree =
    // let
    //     fun design’ (Node(label, subtrees)) =
    //     let
    //         val (trees, extents)    = unzip (map design’ subtrees)
    //         val positions           = fitlist extents
    //         val ptrees              = map movetree (zip (trees, positions))
    //         val pextents            = map moveextent (zip (extents, positions))
    //         val resultextent        = (0.0, 0.0) :: mergelist pextents
    //         val resulttree          = Node((label, 0.0), ptrees)
    //     in
    //         (resulttree, resultextent)
    //     end
    // in
    //     fst (design’ tree)
    // end
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

    /// A possibly more performant/idiomatic implementation by Jensen
    /// TODO: Test its performance
    let design' tree =
        let addToAcc (x, y) (xs, ys) = (x::xs, y::ys)
        let rec design'' (Node(label, subtrees)) = 
            let (trees, extents) = 
                subtrees
                |> fun ts -> 
                    List.foldBack (design'' >> addToAcc) ts ([], [])
            let positions = fitlist extents
            let ptrees = 
                positions
                |> List.map2 (movetree') trees 
            let pextents = 
                positions
                |> List.map2 (moveextent') extents 
            let resultextent = (0.0, 0.0)::mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)
        let (resulttree, _) = design'' tree
        resulttree
