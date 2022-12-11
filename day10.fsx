let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day10.txt" |> List.ofSeq

type CMD = NOOP | ADDX of int

let rec parse (lines: string list) =
 match lines with
 | x :: xs when x.Contains("addx ")-> ((x.Substring(5)) |> int |> ADDX)  :: parse xs
 | x :: xs when x.Contains("noop") -> NOOP :: parse xs
 | x :: _ -> failwith (sprintf "Unrecognized: %s" x)
 | [] -> []

let cmds = parse lines

let rec calc cmds x c =
 match cmds with
 | y :: cs ->
   match y with
   | NOOP -> (x, c) :: calc cs x (c+1)
   | ADDX (i) -> (x, c) :: (x, c+1) :: calc cs (x+i) (c+2)
 | [] -> [(x, c)]

let hist = calc cmds 1 1

let pickh hist n = hist |> List.filter (fun x -> snd x <= n) |> List.sortByDescending (snd) |> List.head |> (fun (x, _) -> x * n)

let d1 = (pickh hist 20) + (pickh hist 60) + (pickh hist 100) + (pickh hist 140) + (pickh hist 180) + (pickh hist 220)

printf "Day 10 (1): %A\n" d1

let rec t40 list = List.chunkBySize 40 list

let groups = (t40 hist)

groups |> List.map (List.mapi (fun i x -> if i-1 <= fst x && fst x <= i+1 then "#" else ".")) |> List.map (String.concat "") |> printf "%A\n"

