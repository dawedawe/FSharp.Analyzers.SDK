module JsonSerializerOptionsAnalyzer.Test

open FSharp.Compiler.CodeAnalysis
open NUnit.Framework
open FSharp.Analyzers.SDK.Testing

let mutable projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! opts =
            mkOptionsFromProject
                "net7.0"
                [
                    {
                        Name = "System.Text.Json"
                        Version = "7.0.3"
                    }
                ]

        projectOptions <- opts
    }

[<Test>]
let ``warning is emitted for Deserialize calls with new ctor`` () =
    async {
        let source =
            """
module M

open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Deserialize<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsync<string>(jsonStream, new JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsyncEnumerable<string>(jsonStream, new JsonSerializerOptions ())
    ()
    """

        let ctx = getContext projectOptions source
        let! msgs = jsonSerializerOptionsAnalyzer ctx
        Assert.IsNotEmpty msgs
        Assert.IsTrue(Assert.hasWarningsInLines (set [7 .. 9]) msgs)
        Assert.IsTrue(Assert.allMessagesContain "JsonSerializerOptions instances should be cached" msgs)
    }

[<Test>]
let ``warning is emitted for Serialize calls with new ctor`` () =
    async {
        let source =
            """
module M

open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Serialize<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeAsync<string>(jsonStream, json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToDocument<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToElement<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToNode<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToUtf8Bytes<string>(json, new JsonSerializerOptions ())
    ()
    """

        let ctx = getContext projectOptions source
        let! msgs = jsonSerializerOptionsAnalyzer ctx
        Assert.IsNotEmpty msgs
        Assert.IsTrue(Assert.hasWarningsInLines (set [7 .. 12]) msgs)
        Assert.IsTrue(Assert.allMessagesContain "JsonSerializerOptions instances should be cached" msgs)
    }


[<Test>]
let ``warning is emitted for Deserialize call with ctor`` () =
    async {
        let source =
            """
module M

open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Deserialize<string>(json, JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsync<string>(jsonStream, JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsyncEnumerable<string>(jsonStream, JsonSerializerOptions ())
    ()
    """

        let ctx = getContext projectOptions source
        let! msgs = jsonSerializerOptionsAnalyzer ctx
        Assert.IsNotEmpty msgs
        Assert.IsTrue(Assert.hasWarningsInLines (set [7 .. 9]) msgs)
        Assert.IsTrue(Assert.allMessagesContain "JsonSerializerOptions instances should be cached" msgs)
    }

[<Test>]
let ``warning is emitted for Serialize call with ctor`` () =
    async {
        let source =
            """
module M

open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Serialize<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeAsync<string>(jsonStream, json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToDocument<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToElement<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToNode<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToUtf8Bytes<string>(json, new JsonSerializerOptions ())
    ()
    """

        let ctx = getContext projectOptions source
        let! msgs = jsonSerializerOptionsAnalyzer ctx
        Assert.IsNotEmpty msgs
        Assert.IsTrue(Assert.hasWarningsInLines (set [7 .. 12]) msgs)
        Assert.IsTrue(Assert.allMessagesContain "JsonSerializerOptions instances should be cached" msgs)
    }

[<Test>]
let ``no warning is emitted for Serialize call with cached options`` () =
    async {
        let source =
            """
module M

open System.Text.Json

let cachedOptions = new JsonSerializerOptions ()

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Serialize<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeAsync<string>(jsonStream, json, cachedOptions)
    let _ = JsonSerializer.SerializeToDocument<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeToElement<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeToNode<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeToUtf8Bytes<string>(json, cachedOptions)
    ()
    """

        let ctx = getContext projectOptions source
        let! msgs = jsonSerializerOptionsAnalyzer ctx
        Assert.IsEmpty msgs
    }
