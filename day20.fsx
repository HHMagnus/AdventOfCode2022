let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day20.txt" |> List.ofSeq |> List.map int64 |> List.mapi (fun i x -> i, i, x)

let length = List.length input

let idMover currId newId lookId =
 if lookId = currId then newId else
 if currId < newId && currId < lookId && lookId <= newId then lookId-1 else
 if currId > newId && newId <= lookId && lookId < currId then lookId+1 else
 lookId

let rec add index a =
 if a = 0 then index else
 if a > 0 then
  if index = length-1 then add 1 (a-1) else
  let nIndex = index + 1
  if nIndex = length then add 0 (a-1) else add nIndex (a-1)
 else
  if index = 0 then add (length-2) (a+1) else
  let nIndex = index - 1
  if nIndex = 0 then add (length-1) (a+1) else add nIndex (a+1)

let rec calcIndex index (value : int64) =
 let a = int (value % (int64 length |> (-) (int64 1)))
 add index a

let day20 (initial: list<int * int * int64>) =
 let rec doIt i (state: list<int * int * int64>) =
  if i = length then state else
  let (_, index, value) = List.find (fun (x, _, _) -> x = i) state
  let nIndex = calcIndex index value
  List.map (fun (sortIndex, i, x) -> (sortIndex, idMover index nIndex i, x)) state |> doIt (i+1)
 doIt 0 initial

let part1input = day20 input

let part1res = part1input |> List.sortBy (fun (_, x ,_) -> x) |> List.map (fun (_, _ ,x) -> x)

printf "%A\n" part1res

let zeroIndex = part1res |> List.mapi (fun i x -> (i, x)) |> List.find (fun (i, x) -> x = 0) |> fst

let part1 = part1res[(zeroIndex + 1000) % length] + part1res[(zeroIndex + 2000) % length] + part1res[(zeroIndex + 3000) % length]

printf "Day 20 part 1: %A\n" part1

let part2input = input |> List.map (fun (sortI, i, x) -> sortI, i, x * int64 811589153)

printf "%A\n" part2input

let part2list =
 part2input
  |> day20
  |> day20
  |> day20
  |> day20
  |> day20
  |> day20
  |> day20
  |> day20
  |> day20
  |> day20

let part2res = part2list |> List.sortBy (fun (_, x ,_) -> x) |> List.map (fun (_, _ ,x) -> x)

let zeroIndex2 = part2res |> List.mapi (fun i x -> (i, x)) |> List.find (fun (i, x) -> x = 0) |> fst

printf "%A, %A, %A\n" part2res[(zeroIndex2 + 1000) % length] part2res[(zeroIndex2 + 2000) % length] part2res[(zeroIndex2 + 3000) % length]
let part2 = part2res[(zeroIndex2 + 1000) % length] + part2res[(zeroIndex2 + 2000) % length] + part2res[(zeroIndex2 + 3000) % length]

printf "Day 20 part 2: %A\n" part2