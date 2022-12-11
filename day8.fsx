let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day8.txt" |> List.ofSeq

let nums = lines |> List.map (fun (x: string) -> x.ToCharArray() |> List.ofSeq |> List.map int)

let height = nums.Length-1
let width = nums[0].Length-1

let tj (m, s) (x, y) =
 match nums[x][y] with
 | z when z > m -> z, Set.add (x,y) s
 | _ -> m, s

let c1 = [0 .. height] |> List.map (fun x -> [0 .. width] |> List.map (fun y -> (x,y)))
let c2 = [0 .. height] |> List.map (fun x -> [0 .. width] |> List.rev |> List.map (fun y -> (x,y)))
let c3 = [0 .. width] |> List.map (fun y -> [0 .. height] |> List.map (fun x -> (x,y)))
let c4 = [0 .. width] |> List.map (fun y -> [0 .. height] |> List.rev |> List.map (fun x -> (x,y)))

let tc ls s = ls |> List.fold (fun s c -> tj s c) (-1, s)

let tu lss s = lss |> List.fold (fun s c -> snd (tc c s)) s

let d1 = Set.empty |> tu c1 |> tu c2 |> tu c3 |> tu c4 |> Set.count

printf "Day 8 (1): %A\n" d1

let rec length ls (m: int) =
 match ls with
 | x :: xs when nums[fst x][snd x] >= m -> 1
 | x :: xs -> 1 + length xs m
 | [] -> 0

let scenic (x, y) =
 let l1 = [0 .. x-1] |> List.rev |> List.map (fun fx -> (fx, y))
 let l2 = [0 .. y-1] |> List.rev |> List.map (fun fy -> (x, fy))
 let l3 = [x+1 .. height] |> List.map (fun fx -> (fx, y))
 let l4 = [y+1 .. width] |> List.map (fun fy -> (x, fy))

 let n = nums[x][y]

 (length l1 n) * (length l2 n) * (length l3 n) * (length l4 n)

let d2 = c1 |> List.collect id |> List.map scenic |> List.max

printf "Day 8 (2): %A\n" d2