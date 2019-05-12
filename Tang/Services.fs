module Tang.Services

open System
open Tang
open Tang.Domain

type Service<'request, 'context, 'response> =
    Service of ('request -> Reader<'context, Result<Effect<'response>, Error>>)

let private createService f = Service (fun req -> Reader (fun con -> f req con))

module Service =

    let run req (Service f) = f req

module Identity =

    let private createNew' () nextTrackingId =
        [CreateIdentity (Identity nextTrackingId)]
        |> Effect.perform 
        |> Effect.map (fun committed -> Map.find nextTrackingId committed.identitiesCreated)
        |> Ok
    
    let createNew = createService createNew'

module Profile =

    type CreateProfileContext = {
        existingIdentities : Set<Identity>
        existingEmails : Set<string>
        existingPhones : Set<string>
        existingUsernames : Set<string>
        nextTrackingId : int
        verifiedEmails : Set<string>
        verifiedPhones : Set<string>
    }
    
    module CreateProfileContext =
        
        let empty = {
            existingIdentities = Set.empty
            existingEmails = Set.empty
            existingPhones = Set.empty
            existingUsernames = Set.empty
            nextTrackingId = 0
            verifiedEmails = Set.empty
            verifiedPhones = Set.empty
        }
    
    let private createNew' profileDto context =
        Dto.Profile.toDomain { profileDto with profileId = Nullable<int> context.nextTrackingId }
            |> Validator.toResult
            |> Result.mapError (List.ofSeq >> ProfileInvalid)
            |> Result.filter
                   (fun x -> Set.contains (Profile.identity x) context.existingIdentities)
                   ProfileIdentityNotFound
            |> Result.filter
                   (fun x -> Set.contains (Email.value (Profile.fields x).email) context.existingEmails |> not)
                   ProfileEmailTaken
            |> Result.filter
                   (fun x -> Set.contains (Phone.value (Profile.fields x).phone) context.existingPhones |> not)
                   ProfilePhoneTaken
            |> Result.filter
                   (fun x -> Set.contains (Username.value (Profile.fields x).username) context.existingUsernames |> not)
                   ProfileUsernameTaken
            |> Result.filter
                   (fun x -> Set.contains (Email.value (Profile.fields x).email) context.verifiedEmails)
                   ProfileEmailNotVerified
            |> Result.filter
                   (fun x -> Set.contains (Phone.value (Profile.fields x).phone) context.verifiedPhones)
                   ProfilePhoneNotVerified
            |> Result.map
                   (fun p ->
                        [CreateProfile p]
                        |> Effect.perform
                        |> Effect.map (fun committed -> Map.find context.nextTrackingId committed.profilesCreated)
                   )

    let createNew = createService createNew'