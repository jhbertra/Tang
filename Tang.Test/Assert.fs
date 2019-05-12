module Tang.Test.Assert
open NUnit.Framework
open Tang

let isOk = function
| Ok a -> a
| Error e -> raise (AssertionException(sprintf "Expected an Ok result, got %A" e))

let isError = function
| Ok a -> raise (AssertionException(sprintf "Expected an Error result, got %A" a))
| Error e -> e

let performsEffects effects = function
| Effect.Pure a -> raise (AssertionException(sprintf "Expected effects, got pure result %A" a))
| Effect.Free (performed, f) ->
    Assert.AreEqual (Set.ofSeq effects, performed)
    f

let isPure = function
| Effect.Pure a -> a
| Effect.Free (fx, _) -> raise (AssertionException(sprintf "Expected pure result, got effects %A" fx))