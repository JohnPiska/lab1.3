open System

let addElement element list =
    element :: list

let rec removeElement element list =
    match list with
    | [] -> []                            
    | head :: tail when head = element -> tail 
    | head :: tail -> head :: removeElement element tail  

let rec findElement element list =
    match list with
    | [] -> false                            
    | head :: tail when head = element -> true  
    | _ :: tail -> findElement element tail   

let rec concatLists list1 list2 =
    match list1 with
    | [] -> list2                             
    | head :: tail -> head :: concatLists tail list2  

let rec getElementByIndex index list =
    match list with
    | [] -> failwith "Индекс выходит за границы списка" 
    | head :: tail when index = 0 -> head                
    | _ :: tail -> getElementByIndex (index - 1) tail    


let showMenu () =
    printfn "\nВыберите операцию:"
    printfn "1 - Добавить элемент"
    printfn "2 - Удалить элемент"
    printfn "3 - Найти элемент"
    printfn "4 - Сцепить два списка"
    printfn "5 - Получить элемент по индексу"
    printfn "0 - Выход"
    printf "Ваш выбор: "
    
let rec main list =
    showMenu()
    let choice = Console.ReadLine() |> int
    
    match choice with
    | 1 -> 
        printf "Введите элемент для добавления: "
        let element = Console.ReadLine() |> int
        let newList = addElement element list
        printfn "Обновленный список: %A" newList
        main newList
        
    | 2 ->
        printf "Введите элемент для удаления: "
        let element = Console.ReadLine() |> int
        let newList = removeElement element list
        printfn "Обновленный список: %A" newList
        main newList
        
    | 3 ->
        printf "Введите элемент для поиска: "
        let element = Console.ReadLine() |> int
        let isFound = findElement element list
        printfn "Элемент найден: %b" isFound
        main list
        
    | 4 ->
        printf "Введите элементы второго списка через пробел: "
        let input = Console.ReadLine()
        let list2 = input.Split(' ') |> Array.toList |> List.map int
        let concatenatedList = concatLists list list2
        printfn "Результат сцепки списков: %A" concatenatedList
        main concatenatedList
        
    | 5 ->
        printf "Введите индекс элемента: "
        let index = Console.ReadLine() |> int
        try
            let element = getElementByIndex index list
            printfn "Элемент на индексе %d: %d" index element
        with
            | ex -> printfn "Ошибка: %s" ex.Message
        main list
        
    | 0 -> 
        printfn "Выход из программы."
        
    | _ -> 
        printfn "Некорректный выбор. Попробуйте снова."
        main list

let inputInitialList () =
    printf "Введите начальные элементы списка через пробел: "
    let input = Console.ReadLine()
    let initialList = input.Split(' ') |> Array.toList |> List.map int
    initialList

let initialList = inputInitialList()
printfn "Начальный список: %A" initialList

main initialList



