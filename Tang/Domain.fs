module Tang.Domain
    
type Identity = Identity of int
module Identity =
    let root = Identity 0
    let create = Identity


type Email = private Email of string
module Email =
    let create email : Result<Email, string> = Email email |> Ok
    let value (Email x) = x


type Phone = private Phone of string
module Phone =
    let create phone : Result<Phone, string> = Phone phone |> Ok
    let value (Phone x) = x


type Username = private Username of string
module Username =
    let create username : Result<Username, string> = Username username |> Ok
    let value (Username x) = x


type ProfileFields = {
    email : Email
    name : string option
    phone : Phone
    username : Username
}
module ProfileFields =
    let create email name phone username = {
        email = email
        name = name
        phone = phone
        username = username
    }


type Profile = Profile of int * Identity * ProfileFields
module Profile =
    let create profileId identity fields = Profile (profileId, identity, fields)
    let profileId (Profile (x, _, _)) = x
    let identity (Profile (_, x, _)) = x
    let fields (Profile (_, _, x)) = x

type Follow = Follow of int * Identity * Identity
module Follow =
    let create followId followedBy following = Follow (followId, followedBy, following)

type Error =
    | ProfileEmailTaken
    | ProfileEmailNotVerified
    | ProfileIdentityNotFound
    | ProfileInvalid of (string * string) list
    | ProfilePhoneTaken
    | ProfilePhoneNotVerified
    | ProfileUsernameTaken