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


let list1 = [1; 2; 3]
let list2 = [4; 5; 6]
    
printfn "Оригинальный список: %A" list1

let addedList = addElement 0 list1
printfn "После добавления 0: %A" addedList

let removedList = removeElement 2 list1
printfn "После удаления 2: %A" removedList

let isFound = findElement 3 list1
printfn "Элемент 3 найден: %b" isFound

let concatenatedList = concatLists list1 list2
printfn "Сцепка двух списков: %A" concatenatedList

let elementAtIndex = getElementByIndex 1 list1
printfn "Элемент на индексе 1: %d" elementAtIndex



