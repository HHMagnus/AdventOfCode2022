let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day1.txt" |> List.ofSeq

let rec d acc list =
 match list with
 | head :: tail when head = "" -> d ([] :: acc) tail
 | head :: tail -> 
    match acc with
    | [] -> d [[head]] tail
    | h :: t -> d ((head :: h) :: t) tail
 | [] -> acc

let totals = lines |> d [] |> List.map (List.map int) |> List.map List.sum

totals |> List.max |> printf "Day 1 (1): %i\n"

totals |> List.sortDescending |> List.take 3 |> List.sum |> printf "Day 1 (2): %i\n"