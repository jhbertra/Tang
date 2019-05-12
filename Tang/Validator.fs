namespace Tang
open System

type Validator<'a, 'e> =
    | Valid of 'a
    | Invalid of 'e seq
module Validator =
    let (<*>) vfab va =
        match (vfab, va) with
        | (Valid fab, Valid a) -> fab a |> Valid
        | (Valid _, Invalid e) -> Invalid e
        | (Invalid e, Valid _) -> Invalid e
        | (Invalid e1, Invalid e2) -> Seq.append e1 e2 |> Invalid
    let map fab va = Valid fab <*> va
    let (<!>) fab va = map fab va
    let (<*) va vb = map (fun a -> fun _ -> a) va <*> vb
    let ( *>) va vb = map (fun _ -> fun b -> b) va <*> vb  
    let ofResult id = function
    | Ok a -> Valid a
    | Error e -> Invalid [(id, e)]
    let toResult = function
    | Valid a -> Ok a
    | Invalid e -> Error e
    
    let required id (nullable : Nullable<'a>) = 
        if nullable.HasValue then
            Valid nullable.Value
        else    
            Invalid [(id, "required")]
    
    let mapIds f = function
    | Valid x -> Valid x
    | Invalid es -> Seq.map (fun (id, msg) -> (f id, msg)) es |> Invalid 