module Tang.Services

open Tang.Domain

type Service<'request, 'context, 'response> = Service of ('request -> Reader<'context, Effect<'response>>)

let private createService f = Service (fun req -> Reader (fun con -> f req con))

module Identity =

    let private createNew' () nextTrackingId =
        [CreateIdentity (Identity nextTrackingId)]
        |> Effect.perform 
        |> Effect.map (fun committed -> Map.find nextTrackingId committed.identitiesCreated)
    
    let createNew = createService createNew'

module Profile =

    let createNew' profileDto  =