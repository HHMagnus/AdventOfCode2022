let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day7.txt" |> List.ofSeq

type CMD = CD of string | LS | DIR of string | FILE of int * string

let file (x: string) = 
 let xs = x.Split " "
 int xs[0], xs[1]

let rec parse (ls : string list) =
 match ls with
 | x :: xs when x.Contains("$ cd ") -> CD (x.Substring 5) :: parse xs
 | x :: xs when x.Contains("$ ls") -> LS :: parse xs
 | x :: xs when x.Contains("dir ") -> DIR (x.Substring 4) :: parse xs
 | x :: xs -> (file x |> FILE) :: parse xs
 | [] -> []

let cmds = lines |> parse

type Tree = Leaf of int | Branch of string * Tree list

let rec b n t =
 match t with
 | Branch (x, ls) :: xs when x = n -> ls
 | _ :: xs -> b n xs
 | _ -> failwith n

let rec buildh (tree: Tree list) list =
 match list with
 | x :: xs ->
   match x with
   | CD x when x = ".." -> tree, xs
   | CD x -> 
     let fs = buildh [] xs
     buildh (Branch(x, fst fs) :: tree) (snd fs)
   | LS -> buildh tree xs
   | DIR (x) -> buildh (tree) xs
   | FILE (x, _) -> buildh (Leaf x :: tree) xs
 | [] -> tree, []

let build cmds = fst (buildh [] cmds) |> List.head

let tree = build cmds

let rec size tree =
 match tree with
 | Leaf (i) -> i
 | Branch (_ , xs) -> List.map size xs |> List.sum

let rec d1 tree =
 match tree with
 | Branch(x, xs) -> 
   let s = size (Branch(x, xs))
   let sub = xs |> List.map d1 |> List.sum
   match s with
   | x when x < 100000 -> x + sub
   | _ -> sub
 | _ -> 0

d1 tree |> printf "Day 7 (1): %i\n"

let free = 70000000 - size tree

let rec sizes tree = 
 match tree with
 | Branch(x, xs) ->
   let s = size (Branch(x, xs))
   List.map sizes xs |> List.fold (@) [s]
 | _ -> []

let d2 tree = sizes tree |> List.filter (fun x -> free + x > 30000000) |> List.min
 
d2 tree |> printf "Day 7 (2): %i\n"