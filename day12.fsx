let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day12.txt" |> List.ofSeq

let startx = 20
let starty = 0
let matrix = lines |> List.map (fun x -> x.ToCharArray() |> List.ofArray |> List.map (fun y -> int y - int 'a'))
let endd = int 'E' - int 'a'

let rec bst (queue: (int * int * int) list) (seen: Set<int * int>) =
 match queue with
 | [] -> failwith "empty queue"
 | (x,y,l) :: xs ->
   let field = matrix[x][y]
   printf "%A %A\n" (x,y,l) (char (field + int 'a'))
   if field = endd then l else
   let rn =
     [(x+1, y); (x, y+1); (x-1, y); (x, y-1)]
     |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < matrix.Length && y < matrix[0].Length)
     |> List.filter (fun (x, y) -> (matrix[x][y] <= field+1 && (matrix[x][y] = int 'E' - int 'a' |> not)) || (field = int 'z' - int 'a' && matrix[x][y] = int 'E' - int 'a'))
     |> List.filter (fun x -> Set.contains x seen |> not)
   let seenn = rn |> List.fold (fun s a -> Set.add a s) seen
   bst (xs @ (rn |> List.map (fun (x,y) -> (x,y, l+1)))) seenn

bst [(startx,starty,0)] ([(startx,starty)] |> Set.ofList) |> printf "Day 12 (1): %A\n"
// I hardcoded S to a for ease