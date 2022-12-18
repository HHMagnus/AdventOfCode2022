let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day18.txt" |> List.ofSeq

let parse (txt: string) =
 let split = txt.Split(",")
 int split[0], int split[1], int split[2]

let coords = List.map parse input |> Set.ofList

let sides (x,y,z) = [(x-1,y,z); (x+1,y,z); (x,y-1,z); (x,y+1,z); (x,y,z-1); (x,y,z+1)]

coords
|> List.ofSeq
|> List.collect sides
|> List.filter (fun x -> Set.contains x coords |> not)
|> List.length
|> printf "Day 18 (1): %A\n"