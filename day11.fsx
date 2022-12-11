let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day11.txt" |> List.ofSeq |> List.chunkBySize 7

let num n x = if x = "old" then n else (int64 x)

let op (txt: string) =
 match txt with
 | x when x.Contains(" * ") -> fun a lcm ->
   let s = x.Split(" * ")
   let x1 = num a s[0]
   let x2 = num a s[1]
   ((x1 % lcm) * (x2 % lcm)) % lcm
 | x when x.Contains(" + ") -> fun a _ ->
   let s = x.Split(" + ")
   let x1 = num a s[0]
   let x2 = num a s[1]
   x1 + x2
 | _ -> failwith "Failed op parse!"

let parse (line: string list) =
 let monkey = line[0].Substring(7).Substring(0, 1) |> int64
 let items = (line[1].Substring(18)).Split(", ") |> List.ofArray |> List.map int64
 let op = op (line[2].Substring(19))
 let div = line[3].Substring(21) |> int64
 let tru = line[4].Substring(29) |> int
 let fals = line[5].Substring(30) |> int
 monkey, items, (op), div, tru, fals

let monkeys = List.map parse lines

let csnd = fun (_, x, _, _ , _ , _) -> x

let start = monkeys |> List.map csnd

// all numbers are primes anyway
let tlcm = monkeys |> List.map (fun (_, _, _, div, _ , _) -> div) |> List.fold (fun s t -> s * t) (int64 1)

let turn i item divisor =
 let (_, _, op, div, tru, fals) = monkeys[i]
 let worry = (op item tlcm) / (int64 divisor)
 match worry % div = 0 with
 | true -> tru, worry
 | false -> fals, worry

let rec monkeyDo i (all: int64 list list) (m: int list) div =
 let items = all[i]
 match items with
 | x :: xs -> 
   let (dest, worry) = turn i x div
   let res = all[..dest-1] @ [all[dest] @ [worry]] @ all[dest+1..]
   let nm = m[..i-1] @ [m[i]+1] @ m[i+1..]
   monkeyDo i (res[0.. i-1] @ [xs] @ res[i+1..]) nm div
 | [] -> all, m

let rec round monkeys start count div =
 [0 .. (List.length monkeys)-1]
 |> List.fold (fun (s, m) i -> monkeyDo i s m div) (start, count)

let countList = [0 .. (List.length monkeys)-1] |> List.map (fun x -> 0)

[1..20]
|> List.fold (fun (s, m) _ -> round monkeys s m 3) (start, countList)
|> snd
|> List.sortDescending
|> List.take 2
|> List.reduce (*)
|> printf "Day 11 (1): %A\n"

[1..10000]
|> List.fold (fun (s, m) _ -> round monkeys s m 1) (start, countList)
|> snd
|> List.sortDescending
|> List.take 2
|> List.map int64
|> List.reduce (*)
|> printf "Day 11 (2): %A\n"