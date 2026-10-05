open System
open System.IO

let getDirectoryPath () =
    printf "Введите путь к каталогу: "
    Console.ReadLine()

let validateDirectory (path: string) =
    if not (Directory.Exists(path)) then
        failwith "ОШИБКА: каталог не существует"

let getFirstChar () =
    printf "Введите символ, с которого должно начинаться имя файла: "
    let input = Console.ReadLine()
    if String.IsNullOrEmpty(input) then
        failwith "ОШИБКА: символ не введён"
    else
        input[0]

let getFiles (path: string) =
    Directory.EnumerateFiles(path)
    |> Seq.map Path.GetFileName
    |> Seq.sort

let countFilesStartingWith (files: seq<string>) (ch: char) =
    Seq.fold
        (fun (acc: int) (fileName: string) ->
            if fileName.StartsWith(string ch) then acc + 1 else acc)
        0
        files

[<EntryPoint>]
let main _ =
    let path = getDirectoryPath ()
    validateDirectory path
    let files = getFiles path
    let ch = getFirstChar ()
    printfn "Количество файлов, начинающихся с '%c': %d" ch (countFilesStartingWith files ch)
    0
