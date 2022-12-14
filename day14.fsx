let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day14.txt" |> List.ofSeq

let lines =
 input
 |> List.map (fun x -> x.Split(" -> ") |> List.ofArray |> List.map (fun y -> y.Split(",") |> List.ofArray |> List.map int) |> List.map (fun y -> (y[0], y[1])))
 |> List.map (List.windowed 2)
 |> List.collect (fun x -> x |> List.map (fun y -> y[0], y[1]))

let rec coords ((x1, y1), (x2, y2)) =
 match x1 = x2 with
 | true -> [(min y1 y2) .. (max y1 y2)] |> List.map (fun y -> (x1, y))
 | false -> [(min x1 x2) .. (max x1 x2)] |> List.map (fun x -> (x, y1))

let c =
 lines
 |> List.collect coords

let rec endPos coords (x,y) =
 if y > 1000 then Some(-1, -1) else
 match List.contains (x,y+1) coords with
 | false -> endPos coords (x, y+1)
 | true ->
   match List.contains (x-1, y+1) coords with
   | false -> endPos coords (x-1, y+1)
   | true ->
      match List.contains (x+1, y+1) coords with
      | true -> Some(x,y)
      | false -> endPos coords (x+1, y+1)

let rec d1 coords (x,y) total =
 let pos = endPos coords (x,y)
 match pos with
 | None -> failwith "Hit none before end"
 | Some(x,y) when x = -1 && y = -1 -> total
 | Some(p) -> d1 (p::coords) (x,y) (total+1)

d1 c (500, 0) 0 |> printf "%A\n"