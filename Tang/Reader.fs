namespace Tang

type Reader<'c, 'a> = Reader of ('c -> 'a)
module Reader =
    let id = Reader id
    let inline run context (Reader r) = r context
    let inline konst a = Reader (fun _ -> a)
    let inline map f (Reader r) = Reader (r >> f)
    let inline bind f (Reader r) = Reader (fun c -> (r c |> f |> run) c)
    let inline apply (Reader fcab) (Reader fca) = Reader (fun c -> fca c |> fcab c)
