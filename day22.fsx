
let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day22.txt" |> List.ofSeq

let mapLength = input.Length - 2

let mapinput = input |> List.take mapLength

type Inst = Right of int | Left of int | Move of int

let rec instructor inst =
 let rec i inst last =
  match inst with
  | 'R'::xs -> Right (int last) :: i xs ""
  | 'L'::xs -> Left (int last) :: i xs ""
  | x::xs -> i xs (last + x.ToString())
  | [] -> 
   match last with
   | "" -> []
   | x -> [Move(int x)]
 i inst ""

let instructions =
 input[mapLength+1].ToCharArray()
 |> Array.toList
 |> instructor

type M = Rock | Empty

let coords list =
 let translate c =
  match c with
  | '.' -> Some(Empty)
  | '#' -> Some(Rock)
  | _ -> None
 let rec coord y (list: string list) set =
  match list with
  | [] -> set
  | l::ls ->
   let s = 
    l.ToCharArray()
    |> Array.toList
    |> List.mapi (fun x c -> (x, translate c))
    |> List.filter (fun x -> Option.isSome (snd x))
    |> List.fold (fun state (x, c) -> Map.add (x, y) c.Value state) set
   coord (y+1) ls s
 coord 0 list Map.empty

let map = coords mapinput

let positions: (int * int) list = Map.keys map |> Seq.cast |> List.ofSeq

let startX = positions |> List.filter (fun x -> (fst x) = 0) |> List.minBy fst |> fst

type Dir = Up | Down | Left | Right

let turnLeft dir =
 match dir with
 | Up -> Left
 | Left -> Down
 | Down -> Right
 | Right -> Up

let turnRight dir =
 match dir with
 | Up -> Right
 | Right -> Down
 | Down -> Left
 | Left -> Up

let start = (startX, 0)

let translate pos dir =
 let (x, y) = pos
 match dir with
 | Up ->
  let nPos = (x, y - 1)
  if Map.containsKey nPos map then nPos else
  let maxY = List.filter (fun (x1, _) -> x1 = x) positions |> List.map snd |> List.max
  (x, maxY)
 | Down -> 
  let nPos = (x, y + 1)
  if Map.containsKey nPos map then nPos else
  let minY = List.filter (fun (x1, _) -> x1 = x) positions |> List.map snd |> List.min
  (x, minY)
 | Right ->
  let nPos = (x + 1, y)
  if Map.containsKey nPos map then nPos else
  let minX = List.filter (fun (_, y1) -> y1 = y) positions |> List.map fst |> List.min
  (minX, y)
 | Left ->
  let nPos = (x - 1, y)
  if Map.containsKey nPos map then nPos else
  let maxY = List.filter (fun (_, y1) -> y1 = y) positions |> List.map fst |> List.max
  (maxY, y)

let rec move pos dir amount =
 if amount = 0 then pos else
 let nPos = (translate pos dir)
 if map[nPos] = Rock then pos else
 move nPos dir (amount-1)

let rec part1 pos dir inst =
 match inst with
 | Inst.Right(x)::xs -> part1 (move pos dir x) (turnRight dir) xs
 | Inst.Left(x)::xs -> part1 (move pos dir x) (turnLeft dir) xs
 | Move(x)::xs -> part1 (move pos dir x) dir xs
 | [] -> pos, dir

let rec part1calc (pos, dir) =
 let (x, y) = pos
 let dirPart =
  match dir with
  | Right -> 0
  | Down -> 1
  | Left -> 2
  | Up -> 3
 1000 * (y+1) + 4 * (x+1) + dirPart

let part1res = part1 start Right instructions |> part1calc

printf "Day 22 part 1: %A\n" part1res