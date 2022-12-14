let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day13.txt" |> List.ofSeq |> List.chunkBySize 3

type T = L of T list | N of int

let rec readNum txt b =
 match txt with
 | x :: xs when x = ',' -> b, xs
 | x :: _ when x = ']' -> b, txt
 | x :: xs -> readNum xs (b + x.ToString())
 | [] -> failwith "failed to read num"

let rec parseh txt l =
 match txt with
 | x :: xs when x = ']' -> l, xs
 | x :: xs when x = '[' ->
   let (n, rem) = parseh xs []
   let (t, rest) = parseh rem []
   l @ L(n) :: t, rest
 | x :: xs when x = ',' -> parseh xs l
 | x :: xs -> 
   let (num, rem) = readNum xs (x.ToString())
   parseh rem (l @ [N(int num)])
 | [] -> l, []

let parse txt = (parseh txt []) |> fst |> List.head

let parsed = input |> List.map (fun x -> (x[0].ToCharArray() |> List.ofArray |> parse, x[1].ToCharArray() |> List.ofArray |> parse))

let rec compare a b =
 match a, b with
 | N(x), N(y) -> if x = y then None else Some(x < y)
 | N(x), L(ys) -> compareList [N(x)] ys
 | L(xs), N(y) -> compareList xs [N(y)]
 | L(xs), L(ys) -> compareList xs ys
and compareList l1 l2 =
 match l1, l2 with
 | [], [] -> None
 | _, [] -> Some(false)
 | [], _ -> Some(true)
 | x :: xs, y :: ys -> 
   match compare x y with
   | None -> compareList xs ys
   | x -> x

parsed
 |> List.mapi (fun i (xs, ys) -> Option.get (compare xs ys), i)
 |> List.filter fst
 |> List.map snd
 |> List.map ((+) 1)
 |> List.sum
 |> printf "Day 13 (1): %A\n"

let intCompare a b = compare a b |> Option.get |> (fun x -> if x then 1 else -1)

parsed
 |> List.collect (fun (x, y) -> [false, x;false, y])
 |> (@) [(true, L([L([N(2)])])); (true, L([L([N(6)])]))]
 |> List.sortWith (fun (_,x) (_,y) -> intCompare x y)
 |> List.rev
 |> List.mapi (fun i (x, _) -> i, x)
 |> List.filter snd
 |> List.map fst
 |> List.map ((+) 1)
 |> List.reduce (*)
 |> printf "Day 13 (2): %A\n"
