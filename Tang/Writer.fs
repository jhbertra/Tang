namespace Tang

type Writer<'a, 'w> = Writer of 'a * 'w seq
module Writer =
    let inline run (Writer (a, w)) = (a, w)
    let inline eval (Writer (a, w)) = a
    let inline exec (Writer (a, w)) = w
    let inline wrap a = Writer (a, [])
    let inline map f (Writer (a, w)) = Writer (f a, w)
    let inline bind f (Writer (a, w)) =
        let (a', w') = f a |> run
        Writer (a', [yield! w; yield! w'])
    let inline apply (Writer (fab, w)) (Writer (a, w')) = Writer (fab a, [yield! w; yield! w'])
