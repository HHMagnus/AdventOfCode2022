let readLines filePath = System.IO.File.ReadLines(filePath);;

let line = readLines "day6.txt" |> List.ofSeq |> List.head

line.ToCharArray()
 |> List.ofSeq
 |> List.windowed 4
 |> List.mapi (fun i x -> i, (List.length x) = (List.length (List.distinct x)))
 |> List.filter snd
 |> List.head
 |> fst
 |> (+) 4
 |> printf "Day 6 (1): %i\n"

line.ToCharArray()
 |> List.ofSeq
 |> List.windowed 14
 |> List.mapi (fun i x -> i, (List.length x) = (List.length (List.distinct x)))
 |> List.filter snd
 |> List.head
 |> fst
 |> (+) 14
 |> printf "Day 6 (2): %i\n"