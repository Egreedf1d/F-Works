// Проверка корректности диапазона (количество элементов должно быть > 0)
let rec getValidCount () =
    printf "Введите количество элементов (целое положительное число): "
    match System.Console.ReadLine() |> System.Int32.TryParse with
    | (true, count) when count > 0 -> count
    | _ ->
        printfn "Ошибка: введите целое положительное число!"
        getValidCount ()

// Проверка корректности ввода числа
let rec getValidNumber index =
    printf "Введите элемент %d: " index
    match System.Console.ReadLine() |> System.Int32.TryParse with
    | (true, num) -> num
    | _ ->
        printfn "Ошибка: введите целое число!"
        getValidNumber index

// Проверка на нечетность
let isOdd number = number % 2 <> 0

[<EntryPoint>]
let main argv =
    // Получаем количество элементов с проверкой
    printfn "Введите количество элементов: "
    let count = getValidCount ()
    
    // Вводим числа один раз и сохраняем их
    printfn "\nВведите числа:"
    let numbers = 
        [ for i in 1 .. count do
            let number = getValidNumber i
            yield number ]
    
    // Формируем список из булевых значений на основе введенных чисел
    let booleanList = numbers |> List.map isOdd
    
    // Выводим результат
    printfn "\nРезультат: "
    printfn "Список булевых значений: %A" booleanList
    
    0
// Уелд выучить(Ленивые вычисления), убрать листмап через нейронку т.к. лишняя памяТь (перенести проверку изодд в намберс)