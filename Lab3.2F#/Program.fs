open System

let rec inputSize () =
    printf "Введите количество элементов: "
    match Int32.TryParse(Console.ReadLine()) with
    | (true, value) when value > 0 -> value
    | _ ->
        printfn "ОШИБКА: некорректный ввод. Введите целое положительное число."
        inputSize ()

let rec inputLength () =
    printf "Введите искомую длину строки: "
    match Int32.TryParse(Console.ReadLine()) with
    | (true, value) when value >= 0 -> value
    | _ ->
        printfn "ОШИБКА: некорректный ввод. Введите целое неотрицательное число."
        inputLength ()

let inputStrings (n: int) =
    seq {
        for i = 1 to n do
            printf "Введите строку %d: " i
            yield Console.ReadLine()
    } |> Seq.cache

let countStringsOfLength (strings: seq<string>) (length: int) =
    Seq.fold
        (fun (acc: int) (s: string) ->
            if s.Length = length then acc + 1 else acc)
        0
        strings

[<EntryPoint>]
let main _ =
    let n = inputSize ()
    printfn "Заполните последовательность"
    let strings = inputStrings n
    let length = inputLength ()
    printfn "Последовательность: %A" strings
    printfn "Количество строк длины %d: %d" length (countStringsOfLength strings length)
    0