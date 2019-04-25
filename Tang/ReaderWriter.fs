namespace Tang

type ReaderWriter<'c, 'a, 'w> = ReaderWriter of Reader<'c, Writer<'a, 'w>>
module ReaderWriter =
    let inline run context (ReaderWriter rw) = Reader.run context rw |> Writer.run
    let inline eval context (ReaderWriter rw) = Reader.run context rw |> Writer.eval
    let inline exec context (ReaderWriter rw) = Reader.run context rw |> Writer.exec
    let inline wrap a = ReaderWriter (Reader (fun _ -> Writer (a, [])))
    let inline map f (ReaderWriter rw) = Reader.map (Writer.map f) rw |> ReaderWriter
    let inline bind f rw =
        ReaderWriter
            (Reader
                (fun c ->
                    let (rw', w) = run c rw
                    let (a', w') = run c (f rw')
                    Writer (a', [yield! w; yield! w'])
                )
            )
    let inline apply rwfab rwa = bind (fun fab -> map fab rwa) rwfab
