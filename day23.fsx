
let readLines filePath = System.IO.File.ReadLines(filePath);;

let raw = readLines "day23.txt" |> List.ofSeq

let input =
 raw
 |> List.mapi (fun i x -> x.ToCharArray() |> Array.toList |> List.mapi (fun j c -> (j, i), c))
 |> List.collect id
 |> List.filter (fun (_, c) -> c = '#')
 |> List.map fst
 |> List.fold (fun state pos -> Set.add pos state) Set.empty

let north (x, y) = [(x - 1, y - 1); (x, y - 1); (x + 1, y - 1)]
let south (x, y) = [(x - 1, y + 1); (x, y + 1); (x + 1, y + 1)]
let west (x, y) = [(x - 1, y - 1); (x - 1, y); (x - 1, y + 1)]
let east (x, y) = [(x + 1, y - 1); (x + 1, y); (x + 1, y + 1)]
let all pos = north pos |> (@) (south pos) |> (@) (west pos) |> (@) (east pos) |> List.distinct

type Direction = North | South | West | East

let print (state: Set<(int * int)>) =
 let minX = Set.toList state |> List.map fst |> List.min
 let maxX = Set.toList state |> List.map fst |> List.max
 let minY = Set.toList state |> List.map snd |> List.min
 let maxY = Set.toList state |> List.map snd |> List.max
 for y in minY..maxY do
  for x in minX..maxX do
   printf "%s" (if (Set.contains (x, y) state) then "#" else ".")
  printf "\n"

let rec suggestMove (x, y) state dirs =
 if List.exists (fun x -> Set.contains x state) (all (x,y)) |> not then (x, y) else
 match dirs with
 | [] -> (x, y)
 | North::xs ->
  if List.exists (fun x -> Set.contains x state) (north (x, y)) |> not then (x, y - 1) else
  suggestMove (x, y) state xs
 | South::xs ->
  if List.exists (fun x -> Set.contains x state) (south (x, y)) |> not  then (x, y + 1) else
  suggestMove (x, y) state xs
 | West::xs ->
  if List.exists (fun x -> Set.contains x state) (west (x, y)) |> not  then (x - 1, y) else
  suggestMove (x, y) state xs
 | East::xs ->
  if List.exists (fun x -> Set.contains x state) (east (x, y)) |> not  then (x + 1, y) else
  suggestMove (x, y) state xs

let round (state: Set<(int * int)>) dirs =
 let suggestions = List.map (fun x -> (x, suggestMove x state dirs)) (Set.toList state)
 List.map (fun (i, n) -> if (List.filter (fun (_, o) -> o = n) suggestions |> List.length) = 1 then n else i) suggestions
 |> Set.ofList

let part1 state =
 let rec run i state dirs =
  if i = 10 then state else
  let nState = round state dirs
  let nDirs = dirs |> List.mapi (fun i x -> (if i = 0 then 4 else i), x) |> List.sortBy fst |> List.map snd
  run (i+1) nState nDirs
 run 0 state [North; South; West; East]

let calc (state: Set<(int * int)>) =
 let minX = Set.toList state |> List.map fst |> List.min
 let maxX = Set.toList state |> List.map fst |> List.max
 let minY = Set.toList state |> List.map snd |> List.min
 let maxY = Set.toList state |> List.map snd |> List.max
 (maxX - minX + 1) * (maxY - minY + 1) - (Set.count state)

let part1res = part1 input |> calc

printf "Day 23 part 1: %A\n" part1res

let part2 state =
 let rec run i state dirs =
  let nState = round state dirs
  if nState = state then i+1 else 
  let nDirs = dirs |> List.mapi (fun i x -> (if i = 0 then 4 else i), x) |> List.sortBy fst |> List.map snd
  run (i+1) nState nDirs
 run 0 state [North; South; West; East]

let part2res = part2 input

printf "Day 23 part 2: %A\n" part2res