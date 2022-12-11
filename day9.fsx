let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day9.txt" |> List.ofSeq

type M = R of int | L of int | U of int | D of int

let map (x: string) =
 match x with
 | y when y.Contains("R") -> int (y.Substring(2)) |> R
 | y when y.Contains("L") -> int (y.Substring(2)) |> L
 | y when y.Contains("U") -> int (y.Substring(2)) |> U
 | y when y.Contains("D") -> int (y.Substring(2)) |> D
 | _ -> failwith "error parsing"

let cmds = lines |> List.map map

let tail (hx, hy) (tx, ty) =
 match abs (hx - tx) > 1 with
 | true -> (if hx - tx > 0 then hx - 1 else hx + 1), hy
 | false ->
   match abs (hy - ty) > 1 with
   | true -> hx, (if hy - ty > 0 then hy - 1 else hy + 1)
   | false -> tx, ty

let rec move cmd (s: Set<int * int>) (hx, hy) (tx, ty) =
 match cmd with
 | R(x) when x = 0 -> s, (hx, hy), (tx, ty)
 | R(x) ->
   let head = (hx-1, hy)
   let tail = tail head (tx, ty)
   let sn = Set.add tail s
   move (R(x-1)) sn head tail
 | L(x) when x = 0 -> s, (hx, hy), (tx, ty)
 | L(x) ->
   let head = (hx+1, hy)
   let tail = tail head (tx, ty)
   let sn = Set.add tail s
   move (L(x-1)) sn head tail
 | U(x) when x = 0 -> s, (hx, hy), (tx, ty)
 | U(x) ->
   let head = (hx, hy-1)
   let tail = tail head (tx, ty)
   let sn = Set.add tail s
   move (U(x-1)) sn head tail
 | D(x) when x = 0 -> s, (hx, hy), (tx, ty)
 | D(x) ->
   let head = (hx, hy+1)
   let tail = tail head (tx, ty)
   let sn = Set.add tail s
   move (D(x-1)) sn head tail

let d1 = cmds |> List.fold (fun (s,h,t) cmd -> move cmd s h t) (Set.empty, (0,0), (0,0))

d1 |> (fun (x,_,_) -> x) |> Set.count |> printf "Day 9 (1): %A\n"

let taila (hx, hy) (tx, ty) =
 match (abs (hx - tx) > 1, abs (hy - ty) > 1) with
 | true, true -> (if hx - tx > 0 then hx - 1 else hx + 1), (if hy - ty > 0 then hy - 1 else hy + 1)
 | true, false -> (if hx - tx > 0 then hx - 1 else hx + 1), hy
 | false,true -> hx, (if hy - ty > 0 then hy - 1 else hy + 1)
 | false, false -> tx, ty

let rec tail10 s (hx, hy) ts =
 match ts with
 | x :: xs ->
   let hn = taila (hx, hy) x
   let (sn, tn) = tail10 s hn xs
   sn, hn :: tn
 | [] -> (Set.add (hx, hy) s), []

let rec move2 cmd (s: Set<int * int>) (hx, hy) ts =
 match cmd with
 | R(x) when x = 0 -> s, (hx, hy), ts
 | R(x) ->
   let head = (hx-1, hy)
   let (sn, tail) = tail10 s head ts
   move2 (R(x-1)) sn head tail
 | L(x) when x = 0 -> s, (hx, hy), ts
 | L(x) ->
   let head = (hx+1, hy)
   let (sn, tail) = tail10 s head ts
   move2 (L(x-1)) sn head tail
 | U(x) when x = 0 -> s, (hx, hy), ts
 | U(x) ->
   let head = (hx, hy-1)
   let (sn, tail) = tail10 s head ts
   move2 (U(x-1)) sn head tail
 | D(x) when x = 0 -> s, (hx, hy), ts
 | D(x) ->
   let head = (hx, hy+1)
   let (sn, tail) = tail10 s head ts
   move2 (D(x-1)) sn head tail

let longtail = [0 .. 8] |> List.map (fun _ -> (0,0))

let d2 = cmds |> List.fold (fun (s,h,t) cmd -> move2 cmd s h t) (Set.empty, (0,0), longtail)

d2 |> (fun (x,_,_) -> x) |> Set.count |> printf "Day 9 (2): %A\n"