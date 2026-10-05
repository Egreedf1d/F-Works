open System

let InputUserString n =
    printfn "Заполните список"
    let ls = 
        [
            for i=1 to n do
                printf "Введите строку: "
                let s = Console.ReadLine()
                yield s
        ]
    ls

let count_strings_of_length (ls: string list) (len: int) =
    List.fold (fun (acc:int) (s:string) -> 
        if s.Length = len then acc + 1 else acc) 
        0 ls

[<EntryPoint>]
let main _ =
    printf "Введите размер списка: "
    let size = int(Console.ReadLine())
    if size <= 0 then
        failwith "ОШИБКА: неккоректный размер списка"
    let ls = InputUserString size
    printf "Введите искомую длину строки: "
    let len = int(Console.ReadLine())
    printfn "Количество строк длины %d: %A" len (count_strings_of_length ls len)
    0