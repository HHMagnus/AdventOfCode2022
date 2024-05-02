let readLine filePath = System.IO.File.ReadAllText(filePath);;

let input = readLine "day17.txt"

let jet = input.ToCharArray() |> Array.toList |> List.replicate 1000 |> List.concat

let print block state =
 let maxY = List.map snd block |> (@) (List.map snd state) |> List.max 
 for revY in 0..maxY do
  for x in 0..6 do
   let y = maxY - revY
   if List.contains (x, y) block then printf "@" else if List.contains (x, y) state then printf "#" else printf "."
  printf "\n"
 printf "\n"

let rec fly block state inf =
 match inf with
 | '<'::rest -> 
  let newBlock = List.map (fun (x, y) -> (x-1, y)) block
  if List.exists (fun (x, y) -> x < 0) newBlock then fall block state rest else
  if List.exists (fun x -> List.contains x state) newBlock then fall block state rest else
  fall newBlock state rest
 | '>'::rest -> 
  let newBlock = List.map (fun (x, y) -> (x+1, y)) block
  if List.exists (fun (x, y) -> x > 6) newBlock then fall block state rest else
  if List.exists (fun x -> List.contains x state) newBlock then fall block state rest else
  fall newBlock state rest
 | x::_ -> failwith "wtf is ${x}"
 | [] -> failwith "was empty"
and fall block state inf = 
 let newBlock = List.map (fun (x, y) -> (x, y-1)) block
 if List.exists (fun (x, y) -> y < 0) newBlock then (block @ state, inf) else
 if List.exists (fun x -> List.contains x state) newBlock then (block @ state, inf) else
 fly newBlock state inf

let lineBlock y = [(2, y); (3, y); (4, y); (5, y)]
let starBlock y = [(2, y+1); (3, y+1); (4, y+1); (3, y); (3, y+2)]
let loserBlock y = [(2, y); (3, y); (4, y); (4, y+1); (4, y+2)]
let rLineBlock y = [(2, y); (2, y+1); (2, y+2); (2, y+3)]
let cubeBlock y = [(2, y); (3, y); (2, y+1); (3, y+1)]

let rec part1 i state inf =
 let highestY = List.map snd (state @ [(0,-1)]) |> List.max
 if i = 2022 then (highestY+1) else
 let block =
  match i % 5 with
  | 0 -> lineBlock (highestY+4)
  | 1 -> starBlock (highestY+4)
  | 2 -> loserBlock (highestY+4)
  | 3 -> rLineBlock (highestY+4)
  | 4 -> cubeBlock (highestY+4)
  | _ -> failwith "not modulo 5 wtf?"
 //print block state
 let (nState, nInf) = fly block state inf
 //print [] nState
 part1 (i+1) nState nInf

let part1res = part1 0 [] jet

printf "Day 17 part 1: %A\n" part1res
