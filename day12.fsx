let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day12.txt" |> List.ofSeq

let x = 0
let y = 0
let matrix = lines |> List.map (fun x -> x.ToCharArray() |> List.ofArray |> List.map (fun y -> int y - int 'a'))

type Pair = int * int
let dict = new System.Collections.Generic.Dictionary<Pair, int>()
let endd = int 'E' - int 'a'

let rec bst (matrix: int list list) (x, y) length =
 let pos = Pair (x,y)
 if dict.ContainsKey(pos) && dict[pos] <= length then None else
 dict.Remove(pos) |> ignore
 dict.Add(pos, length)
 let field = matrix[x][y]
 //printf "%A %A %A\n" x y field
 if field = endd then Some(length) else
   [(x+1, y); (x, y+1); (x-1, y); (x, y-1)]
   |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < matrix.Length && y < matrix[0].Length)
   |> List.filter (fun (x, y) -> (field-1 <= matrix[x][y] && matrix[x][y] <= field+1) || matrix[x][y] = endd)
   |> List.map (fun p -> bst matrix p (length+1))
   |> List.filter Option.isSome
   |> List.min

bst matrix (x,y) 0 |> printf "Day 12 (1): %A\n"
