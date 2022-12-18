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

let minX = Set.map (fun (x, _, _) -> x) coords |> Set.minElement
let minY = Set.map (fun (_, y, _) -> y) coords |> Set.minElement
let minZ = Set.map (fun (_, _, z) -> z) coords |> Set.minElement
let maxX = Set.map (fun (x, _, _) -> x) coords |> Set.maxElement
let maxY = Set.map (fun (_, y, _) -> y) coords |> Set.maxElement
let maxZ = Set.map (fun (_, _, z) -> z) coords |> Set.maxElement

let rec airpocket seen l =
 match l with
 | (x, y, z) :: _ when x < minX || y < minY || z < minZ || x > maxX || y > maxY || z > maxZ -> false
 | (x, y, z) :: xs->
   let sidesLeft =
     sides (x,y,z)
     |> List.filter (fun p -> Set.contains p seen |> not)
     |> List.filter (fun p -> Set.contains p coords |> not)
   airpocket (Set.union seen (sidesLeft |> Set.ofList)) (xs @ sidesLeft)
 | [] -> true

coords
|> List.ofSeq
|> List.collect sides
|> List.filter (fun x -> Set.contains x coords |> not)
|> List.filter (fun x -> airpocket (Set.ofList [x]) [x] |> not)
|> List.length
|> printf "Day 18 (2): %A\n"