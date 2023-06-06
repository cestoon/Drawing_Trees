module TreeSymmetryTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library

// Drawings must be symmetric wrt. reflection. => symmetryProperty