namespace Tang

module Result =
    
    let filter f e = function
    | Ok a when f a -> Ok a
    | Ok _ -> Error e
    | x -> x

    let notNull = function
    | null -> Error "required"
    | x -> Ok x