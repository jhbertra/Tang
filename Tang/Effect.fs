namespace Tang
open Domain

type Effect =
    | ClearProfileName of Profile
    | CreateFollow of Follow
    | CreateIdentity of Identity
    | CreateProfile of Profile
    | RemoveFollow of Follow
    | RemoveProfile of Profile
    | SetProfileEmail of Profile * Email
    | SetProfileName of Profile * string
    | SetProfilePhone of Profile * Phone
    | SetProfileUsername of Profile * Username

type CommitEffectResults = {
    followsCreated : Map<int, Profile>
    identitiesCreated : Map<int, Identity>
    profilesCreated : Map<int, Profile>
}

type Effect<'a> =
    | Pure of 'a
    | Free of Effect seq * (CommitEffectResults -> Effect<'a>)

type CommitEffects<'a> = CommitEffects of (Effect<'a> -> 'a)

module Effect =
    let inline commit (CommitEffects commit) effect = commit effect
    let inline perform fx = Free (fx, Pure)
    let inline result a = Pure a
    let rec bind f = function
    | Pure a -> f a
    | Free (fx, next) -> Free (fx, next >> (bind f))
    let inline map f = bind (f >> Pure)