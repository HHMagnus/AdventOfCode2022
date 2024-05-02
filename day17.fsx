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
 if highestY = 243 || highestY = 2981 || highestY = 5719 then printf "1: %A\n" i
 if highestY = 244 || highestY = 2982 || highestY = 5720 then printf "2: %A\n" i
 if i = 165 + 845 then printf "3: %A\n" highestY
 if i = 2022 then state else
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

let state1 = part1 0 [] jet
let part1res = state1 |> List.map snd |> List.max |> (+) 1

printf "Day 17 part 1: %A\n" part1res

let group = List.groupBy (fun (x, y) -> y) state1 |> List.sortBy (fst) |> List.map (fun (x, y) -> (x, List.map fst y)) |> List.groupBy snd |> List.map (fun (x, y) -> (x, List.map fst y)) |> List.map (fun (x, y) -> (x, y, List.windowed 2 y |> List.map (fun a -> a[1]-a[0])))
printf "%A\n" group
// Used the printf to discover a repeatition of patterns every 2738x height.
// Picked on of the patterns and inputtet as ifs to get the rock
// Discovered that for my input 164, 165, 166 rocks all repeated every 1745 rock
// (1000000000000 - 165) % 1745 = 573.065.902 + remainder. The remainder times 1745 equaled 845 straight up
// Added the printout of 165 + 845 to discover a height of 1566 and added the 573.065.902 cycles times 2738
// This gave 1569054439676 + 1566 + 1 (for 0 index) = the correct result