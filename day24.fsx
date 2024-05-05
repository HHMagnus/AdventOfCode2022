
let readLines filePath = System.IO.File.ReadLines(filePath);;

let raw = readLines "day24.txt" |> List.ofSeq

type Tile = Wall | Ground
type Blizzard = Up of int * int | Down of int * int | Right of int * int | Left of int * int

let input =
 raw
 |> List.mapi (fun y s -> s.ToCharArray() |> Array.toList |> List.mapi (fun x c -> (x, y), c)) |> List.collect id

let tileconstructor c =
 match c with
 | '#' -> Wall
 | _ -> Ground

let tiles =
 input
 |> List.map (fun ((x,y),c) -> (x, y), tileconstructor c)
 |> List.fold (fun state ((x, y), t) -> Map.add (x, y) t state) Map.empty

let blizzardconstructor ((x, y), c) =
 match c with
 | '<' -> Some(Left (x, y))
 | '>' -> Some(Right (x, y))
 | '^' -> Some(Up (x, y))
 | 'v' -> Some(Down (x, y))
 | _ -> None

let blizzards =
 input
 |> List.map (blizzardconstructor)
 |> List.filter Option.isSome
 |> List.map (fun x -> x.Value)

let mapHeight = raw.Length - 2
let mapWidth = raw[0].Length - 2

let coord x m l =
 let t = (x - 1 + m) % l
 if t < 0 then t + l + 1 else t + 1

let blizzardPos minute blizzard =
 match blizzard with
 | Up (x, y) -> (x, coord y (-minute) mapHeight)
 | Down (x, y) -> (x, coord y minute mapHeight)
 | Left (x, y) -> (coord x (-minute) mapWidth, y)
 | Right (x, y) -> (coord x minute mapWidth, y)

let goal = (mapWidth, mapHeight+1)

let print blizzards =
 for minute in 0..10 do
  printf "Minute: %A\n" minute
  let blizz = List.map (blizzardPos minute) blizzards
  for y in 0..mapHeight+1 do
   for x in 0..mapWidth+1 do
    printf "%s" (if List.contains (x, y) blizz then "*" else if tiles[(x, y)] = Wall then "#" else if (x, y) = goal then "G" else ".")
   printf "\n"
  printf "\n"
// print blizzards

let moves (x, y) =
 [(x, y); (x + 1, y); (x - 1, y); (x, y - 1); (x, y + 1)]
 |> List.filter (fun x -> Map.containsKey x tiles && tiles[x] = Ground)

let part1 =
 let rec part1 positions minute =
  let blizz = List.map (blizzardPos minute) blizzards
  let valids: (int * int) list = List.filter (fun x -> List.contains x blizz |> not) positions
  if List.contains goal valids then minute else
  let nPositions = List.collect moves valids |> List.distinct
  part1 nPositions (minute+1)
 part1 [(1, 0)] 0

printf "Day 24 part 1: %A\n" part1