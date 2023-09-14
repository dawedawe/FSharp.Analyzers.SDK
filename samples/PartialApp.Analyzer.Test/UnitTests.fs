module PartialApp.Analyzer.Test

open FSharp.Compiler.CodeAnalysis
open NUnit.Framework
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TestHelpers

let mutable projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! opts =
            mkOptionsFromProject
                // Todo: changing this to net8.0 makes "dotnet test" fail when run inside the repo
                // from outside the repo or in the IDE (but not in a debug session) the tests work fine with net8
                "net7.0"
                [
                    {
                        Name = "Newtonsoft.Json"
                        Version = "13.0.3"
                    }
                    {
                        Name = "Fantomas.FCS"
                        Version = "6.2.0"
                    }
                ]

        projectOptions <- opts
    }

let assertCountOfWarnings msgs n =
    Assert.AreEqual(n, Seq.length msgs, "Actual count and expected count of warnings differed")

let assertWarningsInLines (msgs: Message list) (expectedLines: Set<int>) =
    let msgLines = msgs |> List.map (fun m -> m.Range.StartLine) |> Set.ofList
    Assert.IsTrue((msgLines = expectedLines), "Actual lines and expected lines with warnings differed")

[<Test>]
let ``warnings are emitted for partial app in match case`` () =

    let source =
        """
module M

let myFunc x y = x + y

let myFuncWithMatch x =
    match x with
    | 1 ->
        let a = myFunc 23 // should warn
        true
    | _ ->
        let a = myFunc 23 42 // should not warn
        false
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 9 ])

[<Test>]
let ``warnings are emitted for DU members`` () =

    let source =
        """
module M

let myFunc x y = x + y

type MyU =
    | Case1
    | Case2

    static member MyUnionMember x = myFunc 23 x // should not warn
    static member MyUnionMember2 x = myFunc x // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 11 ])

[<Test>]
let ``warnings are emitted for record members`` () =

    let source =
        """
module M

let myFunc x y = x + y

type MyRec =
    {
        Field1: int
        Field2: int -> int
    }

    static member MyRecMember x = myFunc 23 x // should not warn
    static member MyRecMember2 x = myFunc x // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 13 ])

[<Test>]
let ``warnings are emitted for generic apps`` () =

    let source =
        """
module M

let myGenFunc<'t> x y z = x + y + z

let _ = myGenFunc<int> 23 42 99 // should not warn
let _ = (myGenFunc<int> 23) 42 // should warn
let _ = (myGenFunc<int> 23 42) // should warn
let _ = myGenFunc<int> 23 // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 3
    assertWarningsInLines msgs (set [ 7; 8; 9 ])

[<Test>]
let ``warnings are emitted for generic apps in if-else`` () =

    let source =
        """
module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ = (if true then myGenFunc<int> 23 42 else myFunc 23) 88 // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 7 ])

[<Test>]
let ``warnings are emitted for funcs returned by match exprs`` () =

    let source =
        """
module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ =
        (match 123 with
         | 1 -> myGenFunc<int> 1 2 // should warn
         | 2 -> myFunc 1) // should warn
            22

let _ =
    (match 123 with
        | 1 -> myGenFunc<int> 1 2 3
        | 2 -> myFunc 1 2) // should not warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 9; 10 ])

[<Test>]
let ``warnings are emitted for parameters from if-else exprs`` () =

    let source =
        """
module M

let myFunc x y = x + y

let _ = myFunc (if true then 1 else 0) // should warn
let _ = myFunc (if true then 1 else 0) (if true then 1 else 0) // should not warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 6 ])

[<Test>]
let ``warnings are emitted for parameters from match-lambda exprs`` () =

    let source =
        """
module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ =
    myFunc
        (23
            |> function
                | 1 -> 23
                | _ -> 42)
        111 // should not warn

let _ =
    myFunc (    // should warn
        23
        |> function
            | 1 -> 23
            | _ -> 42
    )

let _ =
    (23
        |> function
            | 1 -> myFunc   // should not warn because no app
            | _ -> myGenFunc<int> 23)   // should warn
        42

let _ =
    (23
        |> function
            | 1 -> myFunc 23 42
            | _ -> myGenFunc<int> 23 42 101) // should not warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 16; 27 ])

[<Test>]
let ``warnings are emitted for record fields`` () =

    let source =
        """
module M

let myFunc x y = x + y

type MyRec =
    {
        Field1: int
        Field2: int -> int
    }

let xxx =
    let r =
        {
            Field1 = 23
            Field2 = myFunc 2 // should warn
        }

    r
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 16 ])

[<Test>]
let ``warnings are emitted in sequential exprs`` () =

    let source =
        """
module M

let myFunc x y = x + y

let partapp1 =
    myFunc 1 // should warn
    let y = myFunc 23 // should warn
    myFunc 3 44 // should not warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 7; 8 ])

[<Test>]
let ``warnings are emitted in simple bindings`` () =

    let source =
        """
module M

let myFunc x y = x + y

let partapp2 = myFunc 4 // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 6 ])

[<Test>]
let ``warnings are emitted for operators`` () =

    let source =
        """
module M

let partapp3 = (+) 4 55 // should not warn
let partapp4 = (+) 4 // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 5 ])

[<Test>]
let ``warnings are emitted for module functions`` () =

    let source =
        """
module M

let partapp5: (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 1
    assertWarningsInLines msgs (set [ 4 ])

[<Test>]
let ``warnings are emitted for top level exprs`` () =

    let source =
        """
module M

let myFunc x y = x + y

myFunc 123 // shoud warn

module SubMod =

    myFunc 123 // shoud warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 6; 10 ])

[<Test>]
let ``warnings are emitted for let bindings in classes`` () =

    let source =
        """
module M

type MyClass() =
    let mutable x = 0
    let myFunc x y = x + y

    let partapp1 =
        myFunc 1 // should warn
        let y = myFunc 23 // should warn
        myFunc 3 44 // should not warn

    let partapp2 = myFunc 4 // should warn
    let partapp3 = (+) 4 55 // should not warn
    let partapp4 = (+) 4 // should warn
    let partapp5: (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 5
    assertWarningsInLines msgs (set [ 9; 10; 13; 15; 16 ])

[<Test>]
let ``warnings are emitted for class members`` () =

    let source =
        """
module M

type MyClass() =
    let mutable x = 0
    let myFunc x y = x + y

    member _.MyMember x =
        myFunc 1 // should warn
        let y = myFunc 23 // should warn
        myFunc 3 44 // should not warn

    member _.MyMember2 x = myFunc 3 // should warn

    member this.X
        with set parameter =
            myFunc 1 // should warn
            let y = myFunc 23 // should warn
            x <- parameter
        and get () = x
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 5
    assertWarningsInLines msgs (set [ 9; 10; 13; 17; 18 ])

[<Test>]
let ``warnings are emitted for interface impls`` () =

    let source =
        """
module M

type MyClass() =
    let mutable x = 0
    let myFunc x y = x + y
    
    interface System.ICloneable with
        member this.Clone() =
            myFunc 1 // should warn
            let y = myFunc 23 // should warn
            failwith "todo"
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 10; 11 ])

[<Test>]
let ``warnings are emitted for do exprs`` () =

    let source =
        """
module M
    module S =
        let myFunc x y = x + y

        do
            let f = myFunc 1 // should warn
            myFunc 1 // should warn
            ()
"""

    let ctx = getContext projectOptions source
    let msgs = PartialAppAnalyzer.partialAppAnalyzer ctx
    assertCountOfWarnings msgs 2
    assertWarningsInLines msgs (set [ 7; 8 ])
