let rec getValidNumber prompt =
    printf "%s" prompt
    match System.Console.ReadLine() |> System.Int32.TryParse with
    | (true, num) -> num
    | _ ->
        printfn "Ошибка: введите целое число!"
        getValidNumber prompt

let getDigitsList number =
    let rec loop n acc =
        if n = 0 then
            match acc with
            | [] -> [0]
            | _ -> acc
        else
            loop (n / 10) ((n % 10) :: acc)
    
    loop (abs number) []

[<EntryPoint>]
let main argv =
    let number = getValidNumber "Введите целое число: "
    let digits = getDigitsList number
    printfn "Список цифр числа %d: %A" number digits
    0