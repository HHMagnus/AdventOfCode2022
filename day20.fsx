let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day20.txt" |> List.ofSeq |> List.map int |> List.mapi (fun i x -> i, x)

let length = List.length input

let idMover currId newId lookId =
 if lookId = currId then newId else
 if currId < newId && currId < lookId && lookId <= newId then lookId-1 else
 if currId > newId && newId <= lookId && lookId < currId then lookId+1 else
 lookId

let rec calcIndex nIndex =
 if nIndex >= length then calcIndex (nIndex - length + 1) else
 if nIndex < 0 then calcIndex (length - 1 + nIndex) else
 if nIndex = 0 then (length - 1) else nIndex

let rec doIt i (state: list<int * int>) =
 if i = length then state else
 let (index, value) = state[i]
 let nIndex = index + value
 let nIndex = calcIndex nIndex
 List.map (fun (i, x) -> (idMover index nIndex i, x)) state |> doIt (i+1)

let part1input = doIt 0 input

let part1res = part1input |> List.sortBy (fst) |> List.map snd

let zeroIndex = part1res |> List.mapi (fun i x -> (i, x)) |> List.find (fun (i, x) -> x = 0) |> fst

let part1 = part1res[(zeroIndex + 1000) % length] + part1res[(zeroIndex + 2000) % length] + part1res[(zeroIndex + 3000) % length]

printf "Day 20 part 1: %A\n" part1


