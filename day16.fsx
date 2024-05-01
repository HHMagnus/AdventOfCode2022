let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day16.txt" |> List.ofSeq

let parse (txt: string) =
 let split = txt.Split("; tunnels lead to valves ");
 let flowRate = split[0].Substring("Valve TU has flow rate=".Length);
 let outgoing = split[1].Split(", ") |> List.ofArray
 let coord = split[0].Substring("Valve ".Length, 2)
 (coord, (int flowRate, outgoing))

let inputlist = input |> List.map parse
let rates = List.filter (fun x -> (fst (snd x)) > 0) inputlist |> List.map (fun x -> fst x)
let coords = Map.ofList inputlist

//printf "%A\n" coords

let rec shortest coord goal visited =
 let curr = coords[coord]
 let negs = snd curr |> List.filter (fun x -> List.contains x visited |> not)
 if List.length negs = 0 then 10000000 else
 if List.contains goal negs then 1 else
 List.map (fun x -> shortest x goal (coord::visited)) negs |> List.min |> (+) 1

let shortMap = 
 List.collect (fun x -> List.map (fun y -> (x, y)) rates) ("AA"::rates)
 |> List.filter (fun (x,y) -> x = y |> not)
 |> List.map (fun (x, y) -> ((x, y), shortest x y []))
 |> Map.ofList

let fullSearch (coord, minute, score, opens) =
 let possible = List.filter (fun x -> List.contains x opens |> not) rates
 let dist = List.map (fun x -> (x, shortMap[(coord, x)])) possible
 let inrange = List.filter (fun x -> minute+(snd x)+1 < 30) dist
 List.map (fun (x : string * int) -> 
  let newCoord = fst x
  let newOpens = newCoord :: opens
  let arrivingMinute = minute + (snd x)
  let newMinute = arrivingMinute + 1
  let flowRate = coords[newCoord] |> fst
  let newScore = (29 - arrivingMinute) * flowRate
  (newCoord, newMinute, score + newScore, newOpens)) inrange

let third (_, _, c, _) = c

let rec exhaust (queue: (string * int * int * string list) list) =
 if List.length queue = 0 then 0 else
 let b = List.map (fun x -> third x) queue |> List.max
 max b (List.collect (fun x -> fullSearch x) queue |> exhaust)

let initial: string * int * int * string list = "AA", 0, 0, []

exhaust [initial] |> printf "%A\n"