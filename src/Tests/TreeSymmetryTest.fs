// #############################################
// # Authors: Mads                             #
// # Contributors: Jinsong                     #
// # Date: June 6th                            #
// # Last edit: June 10th                      #
// #############################################
module Tests.TreeSymmetryTest


open Library.TreeDesign
open FsCheck.NUnit
open Tests.TGenerators


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

let floatEqual precision f1 f2 = (abs ((abs f1) - (abs f2))) < precision

let rec checkPositionalEquality (Node((_, p), ts)) (Node((_, p'), ts')) =
    let (.=) f1 f2 = floatEqual 0.1 f1 f2   
    let notEqual t1 t2 = not <| checkPositionalEquality t1 t2   
    let pos (Node((_,p),_)) = p

    p .= p' 
    && not <| List.exists2 (notEqual) 
        (List.sortBy (pos) ts) 
        (List.sortBy (pos) ts')

let rec checkAbsPositionalEquality (Node((_, p), ts)) (Node((_, p'), ts')) =
    let (.=) (x,y) (x',y') = 
        floatEqual 0.1 x x' 
        && floatEqual 0.1 y y'   

    let notEqual t1 t2 = not <| checkAbsPositionalEquality t1 t2    
    let xCord (Node((_, (x,_)), _)) = x

    p .= p' 
    && not <| List.exists2 (notEqual) 
        (List.sortBy xCord ts) 
        (List.sortBy xCord ts')

[<Property>]
let ``reflect relative tree once to check is position negative`` (t:Tree<char>) = 
    let dt = design t
    let rt = reflect t |> design
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
    checkAbsPositionalEquality dt1 dt2

// In the paper by Kennedy it is posited that:
//     For all trees t, design t = reflect(reflectpos(design(t)))
// This seems to be incorect as that would mean that a tree would always be equal to its 
// own reflection. It is likely that they meant to restrict t to only symmetric trees
// and to perform a positional equality only, with no respect to which node is placed 
// at a given point.
[<Property(Arbitrary=[|typeof<SymmetricTreeGenerator>|])>]
let ``Relative tree symetrical with regards to reflection`` (t: Tree<int>) =
    let t1 = design t
    let t2 = t |> design |> reflectpos |> reflect 
    checkPositionalEquality t1 t2 

[<Property(Arbitrary=[|typeof<SymmetricTreeGenerator>|])>]
let ``Absolute tree symetrical with regards to reflection`` (t: Tree<int>) =
    let t1 = absDesign t
    let t2 = t |> absDesign |> reflectAbs |> reflect 
    checkAbsPositionalEquality t1 t2 
    
[<Property(Arbitrary=[|typeof<SymmetricTreeGenerator>|])>]
let ``symmetric trees will be rendered symmetrically`` (t: Tree<int>) =
    let rt = design t
    let rdt = t |> design |> reflectpos
    checkPositionalEquality rt rdt
