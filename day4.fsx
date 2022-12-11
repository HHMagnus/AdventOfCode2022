let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day4.txt" |> List.ofSeq

let range (x: string) = x.Split '-' |> List.ofSeq |> List.map int

let elfs (x:string) = x.Split ',' |> List.ofSeq |> List.map range

let conflict1 (x: int list list) = (x[0][0] <= x[1][0] && x[0][1] >= x[1][1]) || (x[0][0] >= x[1][0] && x[0][1] <= x[1][1])

lines |> List.map elfs |> List.map conflict1 |> List.filter id |> List.length |> printf "Day 4 (1): %i\n"

// in range = x1 <= y2 && y1 <= x2
let conflict2 (x: int list list) = x[0][0] <= x[1][1] && x[1][0] <= x[0][1]

lines |> List.map elfs |> List.map conflict2 |> List.filter id |> List.length |> printf "Day 4 (2): %i\n"