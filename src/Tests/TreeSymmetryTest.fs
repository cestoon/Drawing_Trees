module Tests.TreeSymmetryTest


open NUnit.Framework
open Library.TreeDesign
open FsCheck
open FsCheck.NUnit


// Drawings must be symmetric wrt. reflection. => symmetryProperty
// relative tree
// reflect once

let rec checkRelativeReflection (Node((l, p: float), ts)) (Node((l', p'), ts')) =
    let notReflected t1 t2 = not <| checkRelativeReflection t1 t2
    let toComparable (Node((_, p), _)) = p

    l = l' 
    && p = -p' 
    && not <| List.exists2 (notReflected) 
        (List.sortBy (toComparable) ts) 
        (List.sortByDescending (toComparable) ts') 

let rec checkAbsoluteReflection  (Node((l, (x, y)), ts)) (Node((l', (x', y')), ts')) =
    let notReflected t1 t2 = not <| checkAbsoluteReflection t1 t2
    let toComparable (Node((_, (x, _)), _)) = x

    l = l' 
    && x = -x' 
    && y = y 
    && not <| List.exists2 (notReflected) 
        (List.sortBy (toComparable) ts) 
        (List.sortByDescending (toComparable) ts')  

let rec checkPositionalEquality (Node((l, p), ts)) (Node((l', p'), ts')) =
    let notEqual t1 t2 = not <| checkPositionalEquality t1 t2    

    l = l'
    && p = p'
    && not <| List.exists2 (notEqual) (List.sort ts) (List.sort ts')

[<Property>]
let ``reflect relative tree once to check is position negative`` (t:Tree<char>) = 
    let dt = design t
    let rt = reflectpos dt
    checkRelativeReflection dt rt 

// flect twice

[<Property>]
let ``reflect relative tree twice to check is position the same`` (t:Tree<char>) = 
    let dt = design t
    let rrt = reflectpos <| reflectpos dt
    dt = rrt

[<Property>]
let ``reflect absolute tree once to check is position negative`` (t:Tree<char>) = 
    let dt = absDesign t
    let rt = reflectAbs dt
    checkAbsoluteReflection dt rt 
// flect twice

[<Property>]
let ``reflect absolute tree twice to check is position the same`` (t:Tree<char>) = 
    let dt = absDesign t
    let rrt = reflectAbs <| reflectAbs dt
    dt = rrt

[<Property>]
let ``reflect used before or after design is equivalent`` (t: Tree<char>) =
    let dt1 = t |> reflect |> design
    let dt2 = t |> design |> reflectpos
    checkPositionalEquality dt1 dt2

[<Property>]
let ``reflect used before or after absDesign is equivalent`` (t: Tree<char>) =
    let dt1 = t |> reflect |> absDesign
    let dt2 = t |> absDesign |> reflectAbs
    checkPositionalEquality dt1 dt2

// In the paper by Kennedy it is posited that:
//     For all trees t, design t = reflect(reflectpos(design(t)))
// This seems to be incorect as that would mean that a tree will always be equal to its 
// own reflection. It is likely that they meant to say that
//     For all trees t, design t = reflectpos(design(reflect(t))),    with respect to positional equality
// This would mean that any tree would be positionally equivalent to the inverted inverted tree after design
// when the invertions design tree is positionally inverted. This would make sense as the property is meant 
// to represent that a tree and its inversion are fitted in the same way, only mirroring around the root node.

[<Property>]
let ``Relative tree symetrical with regards to reflection`` (t: Tree<char>) =
    let t1 = design t
    let t2 = t |> reflect |> design |> reflectpos 
    checkPositionalEquality t1 t2

[<Property>]
let ``Absolute tree symetrical with regards to reflection`` (t: Tree<char>) =
    let t1 = absDesign t
    let t2 = t |> reflect |> absDesign |> reflectAbs 
    checkPositionalEquality t1 t2