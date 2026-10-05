open System

let InputUserString n =
    seq {
        for i=1 to n do
            printf "Введите строку: "
            let s = Console.ReadLine()
            yield s
    } |> Seq.toList

[<EntryPoint>]
let main _ =
    printf "Введите размер последовательности: "
    let size = int(Console.ReadLine())
    if size <= 0 then
        failwith "ОШИБКА: неккоректный размер последовательности"
    printfn "Заполните последовательность"
    let input = InputUserString size
    printf "Введите символ, который добавить к строкам\n
    (вводится 1 символ, все остальные символы после первого читаться не будут)\n: "
    let str = Console.ReadLine()
    let symb = 
        if String.IsNullOrEmpty(str) then ""
        else string(str[0])
    let new_seq = Seq.map (fun s -> symb + s) input
    printfn "Новая последовательность: %A" new_seq
    0