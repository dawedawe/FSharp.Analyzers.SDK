module PartialApp.Analyzer.Test

open FSharp.Compiler.CodeAnalysis
open NUnit.Framework
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing
open FSharp.Analyzers.SDK.Testing.Assert

let mutable projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! opts = mkOptionsFromProject "net7.0" []

        projectOptions <- opts
    }

let assertCountOfWarnings msgs n =
    Assert.AreEqual(n, Seq.length msgs, "Actual count and expected count of warnings differed")

let assertWarningsInLines (msgs: Message list) (expectedLines: Set<int>) =
    Assert.IsTrue(hasWarningsInLines expectedLines msgs, "Actual lines and expected lines with warnings differed")

let assertMessageContains (expectedContent: string) (msg: Message) =
    Assert.IsTrue(
        messageContains expectedContent msg,
        $"Message does not contain expected content `{expectedContent}`, but was '{msg.Message}'"
    )

let assertAllMessagesContain (expectedContent: string) (msgs: Message list) =
    Assert.IsTrue(
        allMessagesContain expectedContent msgs,
        $"Messages do not contain expected content `{expectedContent}`"
    )

let assertAllMessagesContainAny (expectedContents: string list) (msgs: Message list) =
    let s = String.concat ", " expectedContents
    Assert.IsTrue(messagesContainAny expectedContents msgs, $"Messages do not contain any of {s}")

[<Test>]
let ``warnings are emitted for partial app in match case`` () =
    async {

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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 9 ])
        assertMessageContains "myFunc" msgs[0]
    }

[<Test>]
let ``warnings are emitted for DU members`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 11 ])
        assertMessageContains "myFunc" msgs[0]
    }

[<Test>]
let ``warnings are emitted for record members`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 13 ])
        assertMessageContains "myFunc" msgs[0]
    }

[<Test>]
let ``warnings are emitted for generic apps`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 3
        assertWarningsInLines msgs (set [ 7; 8; 9 ])
        assertAllMessagesContain "myGenFunc" msgs
    }

[<Test>]
let ``warnings are emitted for generic apps in if-else`` () =
    async {
        let source =
            """
module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ = (if true then myGenFunc<int> 23 42 else myFunc 23) 88 // should warn
"""

        let ctx = getContext projectOptions source
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 7 ])
        assertAllMessagesContainAny [ "myFunc"; "myGenFunc" ] msgs
    }

[<Test>]
let ``warnings are emitted for funcs returned by match exprs`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 9; 10 ])
        assertAllMessagesContainAny [ "myFunc"; "myGenFunc" ] msgs
    }

[<Test>]
let ``warnings are emitted for parameters from if-else exprs`` () =
    async {
        let source =
            """
module M

let myFunc x y = x + y

let _ = myFunc (if true then 1 else 0) // should warn
let _ = myFunc (if true then 1 else 0) (if true then 1 else 0) // should not warn
"""

        let ctx = getContext projectOptions source
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 6 ])
        assertMessageContains "myFunc" msgs[0]
    }

[<Test>]
let ``warnings are emitted for parameters from match-lambda exprs`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 16; 27 ])
        assertAllMessagesContainAny [ "myFunc"; "myGenFunc" ] msgs
    }

[<Test>]
let ``warnings are emitted for record fields`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 16 ])
        assertMessageContains "myFunc" msgs[0]
    }

[<Test>]
let ``warnings are emitted in sequential exprs`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 7; 8 ])
        assertAllMessagesContain "myFunc" msgs
    }

[<Test>]
let ``warnings are emitted in simple bindings`` () =
    async {
        let source =
            """
module M

let myFunc x y = x + y

let partapp2 = myFunc 4 // should warn
"""

        let ctx = getContext projectOptions source
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 6 ])
        assertMessageContains "myFunc" msgs[0]
    }

[<Test>]
let ``warnings are emitted for operators`` () =
    async {
        let source =
            """
module M

let partapp3 = (+) 4 55 // should not warn
let partapp4 = (+) 4 // should warn
"""

        let ctx = getContext projectOptions source
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 5 ])
        assertMessageContains "op_Addition" msgs[0]
    }

[<Test>]
let ``warnings are emitted for module functions`` () =
    async {
        let source =
            """
module M

let partapp5: (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
"""

        let ctx = getContext projectOptions source
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 1
        assertWarningsInLines msgs (set [ 4 ])
        assertMessageContains "map" msgs[0]
    }

[<Test>]
let ``warnings are emitted for top level exprs`` () =
    async {
        let source =
            """
module M

let myFunc x y = x + y

myFunc 123 // shoud warn

module SubMod =

    myFunc 123 // shoud warn
"""

        let ctx = getContext projectOptions source
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 6; 10 ])
        assertAllMessagesContain "myFunc" msgs
    }

[<Test>]
let ``warnings are emitted for let bindings in classes`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 5
        assertWarningsInLines msgs (set [ 9; 10; 13; 15; 16 ])
        assertAllMessagesContainAny [ "myFunc"; "op_Addition"; "map" ] msgs
    }

[<Test>]
let ``warnings are emitted for class members`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 5
        assertWarningsInLines msgs (set [ 9; 10; 13; 17; 18 ])
        assertAllMessagesContain "myFunc" msgs
    }

[<Test>]
let ``warnings are emitted for interface impls`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 10; 11 ])
        assertAllMessagesContain "myFunc" msgs
    }

[<Test>]
let ``warnings are emitted for do exprs`` () =
    async {
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
        let! msgs = PartialAppAnalyzer.partialAppCliAnalyzer ctx
        assertCountOfWarnings msgs 2
        assertWarningsInLines msgs (set [ 7; 8 ])
        assertAllMessagesContain "myFunc" msgs
    }
