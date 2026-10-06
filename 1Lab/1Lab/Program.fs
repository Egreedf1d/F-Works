let rec getValidCount () =
    printf "Введите количество элементов (целое положительное число): "
    match System.Console.ReadLine() |> System.Int32.TryParse with
    | (true, count) when count > 0 -> count
    | _ ->
        printfn "Ошибка: введите целое положительное число!"
        getValidCount ()

let rec getValidNumber index =
    printf "Введите элемент %d: " index
    match System.Console.ReadLine() |> System.Int32.TryParse with
    | (true, num) -> num
    | _ ->
        printfn "Ошибка: введите целое число!"
        getValidNumber index

let isOdd number = number % 2 <> 0

[<EntryPoint>]
let main argv =
    printfn "Введите количество элементов: "
    let count = getValidCount ()
    printfn "\nВведите числа:"
    let numbers = 
        [ for i in 1 .. count do
            let number = getValidNumber i
            yield number ]
    let booleanList = numbers |> List.map isOdd 
    printfn "\nРезультат: "
    printfn "Список булевых значений: %A" booleanList
    0
