namespace Tang.Tests

open System
open NUnit.Framework
open Tang
open Tang.Domain
open Tang.Services
open Tang.Services.Profile
open Tang.Test

type ProfileTests () =
    
    [<Test>]
    member this.createNew_invalidProfile_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable ()
                profileId = Nullable ()
                email = null
                name = null
                phone = null
                username = null
                }
            |> Reader.run CreateProfileContext.empty
            |> Assert.isError

        Assert.AreEqual
            ( ProfileInvalid
                  [
                      ("identity", "required")
                      ("email", "required")
                      ("phone", "required")
                      ("username", "required")
                  ]
            , result
            )
    
    [<Test>]
    member this.createNew_identityNotFound_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run CreateProfileContext.empty
            |> Assert.isError

        Assert.AreEqual
            ( ProfileIdentityNotFound, result )
    
    [<Test>]
    member this.createNew_emailTaken_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run {
                CreateProfileContext.empty with
                    existingIdentities = Set.ofList [Identity 1]
                    existingEmails = Set.ofList ["test@test.com"]
            } 
            |> Assert.isError

        Assert.AreEqual
            ( ProfileEmailTaken, result )
    
    [<Test>]
    member this.createNew_phoneTaken_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run {
                CreateProfileContext.empty with
                    existingIdentities = Set.ofList [Identity 1]
                    existingPhones = Set.ofList ["123456789"]
            } 
            |> Assert.isError

        Assert.AreEqual
            ( ProfilePhoneTaken, result )
    
    [<Test>]
    member this.createNew_usernameTaken_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run {
                CreateProfileContext.empty with
                    existingIdentities = Set.ofList [Identity 1]
                    existingUsernames = Set.ofList ["test"]
            } 
            |> Assert.isError

        Assert.AreEqual
            ( ProfileUsernameTaken, result )
    
    [<Test>]
    member this.createNew_emailNotVerified_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run {
                CreateProfileContext.empty with
                    existingIdentities = Set.ofList [Identity 1]
            } 
            |> Assert.isError

        Assert.AreEqual
            ( ProfileEmailNotVerified, result )
    
    [<Test>]
    member this.createNew_phoneNotVerified_returnsError () =
        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run {
                CreateProfileContext.empty with
                    existingIdentities = Set.ofList [Identity 1]
                    verifiedEmails = Set.ofList ["test@test.com"]
            } 
            |> Assert.isError

        Assert.AreEqual
            ( ProfilePhoneNotVerified, result )
    
    [<Test>]
    member this.createNew_validRequest_createsNewProfile () =
        let expectedProfile =
            Profile
                (42
                , Identity 1
                , {
                    email = Email.create "test@test.com" |> Assert.isOk
                    name = Some "Testy Tester"
                    phone = Phone.create "123456789" |> Assert.isOk
                    username = Username.create "test" |> Assert.isOk }
                )

        let result =
            Profile.createNew
            |> Service.run { 
                identity = Nullable 1
                profileId = Nullable ()
                email = "test@test.com"
                name = "Testy Tester"
                phone = "123456789"
                username = "test"
                }
            |> Reader.run {
                CreateProfileContext.empty with
                    existingIdentities = Set.ofList [Identity 1]
                    verifiedEmails = Set.ofList ["test@test.com"]
                    verifiedPhones = Set.ofList ["123456789"]
                    nextTrackingId = 1
            } 
            |> Assert.isOk
            |> Assert.performsEffects
                [CreateProfile (Profile (1, Identity 1, {
                    email = Email.create "test@test.com" |> Assert.isOk
                    name = Some "Testy Tester"
                    phone = Phone.create "123456789" |> Assert.isOk
                    username = Username.create "test" |> Assert.isOk
                }))]
            |> (fun next -> next {
                    CommitEffectResults.empty with
                        profilesCreated = Map.ofList [(1, expectedProfile)] })
            |> Assert.isPure

        Assert.AreEqual (expectedProfile, result)