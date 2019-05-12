namespace Tang.Tests

open NUnit.Framework
open Tang
open Tang.Domain
open Tang.Services
open Tang.Test

type IdentityTests () =
    
    [<Test>]
    member this.createNew_createsNewIdentity () =
        let result =
            Identity.createNew
            |> Service.run ()
            |> Reader.run 12
            |> Assert.isOk
            |> Assert.performsEffects [CreateIdentity (Identity 12)]
            |> (fun next -> next { CommitEffectResults.empty with identitiesCreated = Map.ofList [(12, Identity 1)] })
            |> Assert.isPure

        Assert.AreEqual (Identity 1, result)