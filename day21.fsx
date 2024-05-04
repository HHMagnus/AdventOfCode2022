type Action = Add | Subtract | Multiply | Divide
type Op = A of Action * string * string | Number of int64

let readLines filePath = System.IO.File.ReadLines(filePath);;

let parseLine (line: string) =
 let split = line.Split(": ")
 let name = split[0]
 if split[1].Contains(" + ") then
  let s = split[1].Split(" + ")
  (name, A(Add, s[0], s[1]))
 else if split[1].Contains(" - ") then
  let s = split[1].Split(" - ")
  (name, A(Subtract, s[0], s[1]))
 else if split[1].Contains(" * ") then
  let s = split[1].Split(" * ")
  (name, A(Multiply, s[0], s[1]))
 else if split[1].Contains(" / ") then
  let s = split[1].Split(" / ")
  (name, A(Divide, s[0], s[1]))
 else (name, Number(int split[1] |> int64))

let input = readLines "day21.txt" |> List.ofSeq |> List.map parseLine

let map op knowns =
 let (name, value) = op
 match value with
 | Number (n) -> Some(name, n)
 | A(a, p1, p2) -> 
  if Map.containsKey p1 knowns && Map.containsKey p2 knowns then
   let op =
    match a with
    | Add -> (+)
    | Subtract -> (-)
    | Multiply -> fun x y -> x * y
    | Divide -> (/)
   let n = op (Map.find p1 knowns) (Map.find p2 knowns)
   Some(name, n)
  else None

let part1 ops =
 let rec run ops knowns =
  let mapped = List.map (fun op -> map op knowns) ops
  let moreops = List.zip ops mapped |> List.filter (fun (_, y) -> y = None) |> List.map (fun (x, _) -> x)
  let nKnowns =
   List.filter (fun x -> x = None |> not) mapped
   |> List.map (fun opt -> opt.Value)
   |> List.fold (fun state (x, y) -> Map.add x y state) knowns
  if List.length moreops = 0 then nKnowns["root"] else
  run moreops nKnowns
 run ops Map.empty

let part1res = part1 input

printf "Day 21 part 1: %A\n" part1res

type Tree = Op of Action * Tree * Tree | Human | Number of int64

let without = List.filter (fun (x, y) -> x = "humn" |> not) input

let so = List.fold (fun state (x1, x2) -> Map.add x1 x2 state) Map.empty without

let rec construct name =
 match name with
 | "humn" -> Human
 | x ->
  match so[x] with
  | Op.Number(n) -> Number(n)
  | A(act, p1, p2) -> Op(act, construct p1, construct p2)


let rootp = List.find (fun (x, y) -> x = "root") input
let root =
 match snd rootp with
 | A(_, p1, p2) -> (p1, p2)
 | _ -> failwith "root not matching"

let rec mintree tree =
 match tree with
 | Human -> Human
 | Number(x) -> Number(x)
 | Op (act, tree1, tree2) ->
  match mintree tree1 with
  | Op(a, t1, t2) -> Op (act, Op(a, t1, t2), mintree tree2)
  | Human -> Op (act, Human, mintree tree2)
  | Number(x1) ->
   match mintree tree2 with
   | Op(a, t1, t2) -> Op (act, Number(x1), Op(a, t1, t2))
   | Human -> Op (act, Number(x1), Human)
   | Number(x2) ->
    match act with
    | Multiply -> Number(x1 * x2)
    | Divide -> Number(x1 / x2)
    | Add -> Number(x1 + x2)
    | Subtract -> Number(x1 - x2)

let r1 = construct (fst root) |> mintree
let r2 = construct (snd root) |> mintree

let rec eq x =
 match x with
 | Op(act, p1, p2) ->
  let action =
   match act with
   | Add -> "+"
   | Subtract -> "-"
   | Multiply -> "*"
   | Divide -> "/"
  "(" + (eq p1) + action + (eq p2) + ")"
 | Number(x) -> x.ToString()
 | Human -> "x"

printf "Day 21 part 2 requires solving for x: %A\n" ((eq r1) + "=" + (eq r2))