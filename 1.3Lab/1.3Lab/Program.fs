// Модуль с собственными функциями для работы со списками
module MyListFunctions =
    
    /// 1. Добавление элемента в конец списка
    let addElement list element =
        let rec append lst acc =
            match lst with
            | [] -> List.rev (element :: acc)
            | head :: tail -> append tail (head :: acc)
        append list []
    
    /// 2. Добавление элемента в начало списка
    let addFirst list element = element :: list
    
    /// 3. Добавление элемента по индексу
    let addAtIndex list index element =
        let rec insertAt lst idx acc =
            match lst with
            | [] when idx = 0 -> List.rev (element :: acc)
            | [] -> failwith "Индекс выходит за пределы списка"
            | head :: tail when idx = 0 -> List.rev (element :: head :: acc) @ tail
            | head :: tail -> insertAt tail (idx - 1) (head :: acc)
        
        if index < 0 then
            failwith "Индекс не может быть отрицательным"
        elif index > List.length list then
            failwith "Индекс выходит за пределы списка"
        else
            insertAt list index []
    
    /// 4. Удаление элемента по значению (первое вхождение)
    let removeByValue list value =
        let rec removeFirst lst acc =
            match lst with
            | [] -> List.rev acc
            | head :: tail ->
                if head = value then
                    List.rev acc @ tail
                else
                    removeFirst tail (head :: acc)
        removeFirst list []
    
    /// 5. Удаление элемента по индексу
    let removeAtIndex list index =
        let rec removeAt lst idx acc =
            match lst with
            | [] -> failwith "Индекс выходит за пределы списка"
            | head :: tail when idx = 0 -> List.rev acc @ tail
            | head :: tail -> removeAt tail (idx - 1) (head :: acc)
        
        if index < 0 then
            failwith "Индекс не может быть отрицательным"
        elif index >= List.length list then
            failwith "Индекс выходит за пределы списка"
        else
            removeAt list index []
    
    /// 6. Удаление всех вхождений значения
    let removeAllByValue list value =
        let rec removeAll lst acc =
            match lst with
            | [] -> List.rev acc
            | head :: tail ->
                if head = value then
                    removeAll tail acc
                else
                    removeAll tail (head :: acc)
        removeAll list []
    
    /// 7. Поиск элемента (возвращает индекс первого вхождения)
    let findElement list value =
        let rec find lst idx =
            match lst with
            | [] -> None
            | head :: tail ->
                if head = value then
                    Some idx
                else
                    find tail (idx + 1)
        find list 0
    
    /// 8. Проверка наличия элемента в списке
    let contains list value =
        match findElement list value with
        | Some _ -> true
        | None -> false
    
    /// 9. Сцепка (конкатенация) двух списков
    let concatenate list1 list2 =
        let rec concat lst acc =
            match lst with
            | [] -> acc
            | head :: tail -> concat tail (head :: acc)
        List.rev (concat list1 (concat list2 []))
    
    /// 10. Получение элемента по индексу
    let getElement list index =
        let rec get lst idx =
            match lst with
            | [] -> failwith "Индекс выходит за пределы списка"
            | head :: tail when idx = 0 -> head
            | head :: tail -> get tail (idx - 1)
        
        if index < 0 then
            failwith "Индекс не может быть отрицательным"
        else
            get list index

// Модуль для проверки ввода
module InputValidation =
    
    let rec readInt prompt =
        printf "%s" prompt
        match System.Console.ReadLine() |> System.Int32.TryParse with
        | (true, value) -> value
        | _ ->
            printfn "Ошибка: введите целое число!"
            readInt prompt
    
    let rec readIntInRange prompt minValue maxValue =
        printf "%s" prompt
        let value = readInt ""
        if value >= minValue && value <= maxValue then
            value
        else
            printfn "Ошибка: число должно быть в диапазоне от %d до %d!" minValue maxValue
            readIntInRange prompt minValue maxValue
    
    let readIntList () =
        printfn "Введите элементы списка (по одному)."
        printfn "Для завершения введите 'stop':"
        
        let rec readElements acc =
            printf "Элемент %d: " (List.length acc + 1)
            let input = System.Console.ReadLine()
            
            if input.ToLower() = "stop" then
                List.rev acc
            else
                match System.Int32.TryParse(input) with
                | (true, value) -> readElements (value :: acc)
                | _ ->
                    printfn "Ошибка: введите целое число или 'stop' для завершения!"
                    readElements acc
        
        readElements []

[<EntryPoint>]
let main argv =
    // Ввод исходного списка
    let initialList = InputValidation.readIntList ()
    let mutable currentList = initialList
    
    printfn "\nИсходный список: %A" currentList
    printfn "Длина списка: %d" (List.length currentList)
    
    let mutable continueProgram = true
    
    while continueProgram do
        printfn "\n========== МЕНЮ ОПЕРАЦИЙ =========="
        printfn "1. Добавить элемент в конец"
        printfn "2. Добавить элемент в начало"
        printfn "3. Добавить элемент по индексу"
        printfn "4. Удалить элемент по значению (первое вхождение)"
        printfn "5. Удалить элемент по индексу"
        printfn "6. Удалить все вхождения значения"
        printfn "7. Найти индекс элемента"
        printfn "8. Проверить наличие элемента"
        printfn "9. Сцепить с другим списком"
        printfn "10. Получить элемент по индексу"
        printfn "11. Показать текущий список"
        printfn "0. Выход"
        printfn "===================================="
        
        printf "Выберите операцию: "
        let choice = 
            match System.Console.ReadLine() |> System.Int32.TryParse with
            | (true, value) -> value
            | _ -> -1
        
        if choice = 0 then
            continueProgram <- false
            printfn "\nПрограмма завершена."
        
        elif choice = 1 then
            printf "Введите элемент для добавления в конец: "
            let element = InputValidation.readInt ""
            currentList <- MyListFunctions.addElement currentList element
            printfn "Результат: %A" currentList
        
        elif choice = 2 then
            printf "Введите элемент для добавления в начало: "
            let element = InputValidation.readInt ""
            currentList <- MyListFunctions.addFirst currentList element
            printfn "Результат: %A" currentList
        
        elif choice = 3 then
            let maxIndex = List.length currentList
            printfn "Индекс может быть от 0 до %d" maxIndex
            printf "Введите индекс: "
            let index = InputValidation.readInt ""
            printf "Введите элемент для добавления: "
            let element = InputValidation.readInt ""
            try
                currentList <- MyListFunctions.addAtIndex currentList index element
                printfn "Результат: %A" currentList
                printfn "Элемент %d вставлен на позицию %d" element index
            with
            | ex -> printfn "Ошибка: %s" ex.Message
        
        elif choice = 4 then
            printf "Введите значение для удаления: "
            let value = InputValidation.readInt ""
            let newList = MyListFunctions.removeByValue currentList value
            if List.length newList = List.length currentList then
                printfn "Элемент %d не найден в списке" value
            else
                currentList <- newList
                printfn "Результат: %A" currentList
        
        elif choice = 5 then
            if List.length currentList = 0 then
                printfn "Список пуст, невозможно удалить элемент"
            else
                let maxIndex = List.length currentList - 1
                printfn "Индекс может быть от 0 до %d" maxIndex
                printf "Введите индекс для удаления: "
                let index = InputValidation.readInt ""
                try
                    currentList <- MyListFunctions.removeAtIndex currentList index
                    printfn "Результат: %A" currentList
                with
                | ex -> printfn "Ошибка: %s" ex.Message
        
        elif choice = 6 then
            printf "Введите значение для удаления всех вхождений: "
            let value = InputValidation.readInt ""
            currentList <- MyListFunctions.removeAllByValue currentList value
            printfn "Результат: %A" currentList
        
        elif choice = 7 then
            printf "Введите значение для поиска: "
            let value = InputValidation.readInt ""
            match MyListFunctions.findElement currentList value with
            | Some index -> printfn "Элемент %d найден на индексе %d" value index
            | None -> printfn "Элемент %d не найден в списке" value
        
        elif choice = 8 then
            printf "Введите значение для проверки: "
            let value = InputValidation.readInt ""
            let found = MyListFunctions.contains currentList value
            printfn "Элемент %d %s в списке" value (if found then "присутствует" else "отсутствует")
        
        elif choice = 9 then
            printfn "\nВведите второй список:"
            let list2 = InputValidation.readIntList ()
            let result = MyListFunctions.concatenate currentList list2
            printfn "Результат сцепки: %A" result
            printfn "Хотите сохранить результат как текущий список? (да/нет)"
            let save = System.Console.ReadLine()
            if save.ToLower() = "да" then
                currentList <- result
                printfn "Текущий список обновлен: %A" currentList
        
        elif choice = 10 then
            if List.length currentList = 0 then
                printfn "Список пуст, невозможно получить элемент"
            else
                let maxIndex = List.length currentList - 1
                printfn "Индекс может быть от 0 до %d" maxIndex
                printf "Введите индекс: "
                let index = InputValidation.readInt ""
                try
                    let element = MyListFunctions.getElement currentList index
                    printfn "Элемент на индексе %d: %d" index element
                with
                | ex -> printfn "Ошибка: %s" ex.Message
        
        elif choice = 11 then
            printfn "\nТекущий список: %A" currentList
            printfn "Длина списка: %d" (List.length currentList)
        
        else
            printfn "Неверный выбор! Пожалуйста, выберите номер от 0 до 11"
    
    0