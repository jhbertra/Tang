module Tang.Dto
open Domain
open System
open Tang
open Validator

type Profile = {
    identity : Nullable<int>
    profileId : Nullable<int>
    email : string
    name : string
    phone : string
    username : string
}
module Profile =
    let ofDomain (Domain.Profile (profileId, Identity identity, fields)) = {
        identity = Nullable identity
        profileId = Nullable profileId
        email = Email.value fields.email
        name = Option.toObj fields.name
        phone = Phone.value fields.phone
        username = Username.value fields.username
    }
    let toDomain profile =
        Profile.create
            <!> Validator.required "profileId" profile.profileId
            <*> (Domain.Identity <!> Validator.required "identity" profile.identity)
            <*> (
                    ProfileFields.create
                        <!> (Email.create "email" profile.email |> Validator.ofResult)
                        <*> (Option.ofObj profile.name |> Valid)
                        <*> (Phone.create "phone" profile.phone |> Validator.ofResult)
                        <*> (Username.create "username" profile.username |> Validator.ofResult)
                )
            


type Follow = {
    followId : Nullable<int>
    followedById : Nullable<int>
    followingId : Nullable<int>
}
module Follow =
    let ofDomain (Domain.Follow (followId, Identity followedById, Identity followingId)) = {
        followId = Nullable followId
        followedById = Nullable followedById
        followingId = Nullable followingId
    }
    let toDomain follow =
        Follow.create
            <!> Validator.required "followId" follow.followId
            <*> (Domain.Identity <!> Validator.required "followedById" follow.followedById)
            <*> (Domain.Identity <!> Validator.required "followingId" follow.followingId)
