let readLines filePath = System.IO.File.ReadLines(filePath);;

let raw = readLines "day25.txt" |> List.ofSeq

let decifer ns =
 let rec run bas nums =
  match nums with
  | [] -> int64 0
  | x::xs ->
   let num =
    match x with
    | '2' -> int64 2
    | '1' -> int64 1
    | '0' -> int64 0
    | '-' -> int64 -1
    | '=' -> int64 -2
    | y -> failwith (sprintf "Not understood: %A" y)
   num * bas + (run (bas * int64 5) xs)
 run (int64 1) ns

let translated =
 raw
 |> List.map (fun x -> x.ToCharArray() |> Array.toList |> List.rev |> decifer)
 |> List.sum

printf "Sum: %A\n" translated

let cifer (number: int64) =
 let rec m c n r t =
  if r >= c then (n,r)::t else m c (n * int64 5) (r + int64 2 * n * int64 5) ((n,r)::t)
 let rec encode num l =
  match l with
  | (n0, r0)::(n1, r1)::xs ->
   if (abs num) <= r1 || num = int64 0 then "0" + (encode num ((n1, r1)::xs)) else 
   if num > int64 0 && (abs (num - n0)) <= r1 then "1" + (encode (num - n0) ((n1, r1)::xs)) else
   if num > int64 0 then "2" + (encode (num - int64 2 * n0) ((n1, r1)::xs)) else
   if num < int64 0 && (abs (num + n0)) <= r1 then "-" + (encode (num + n0) ((n1, r1)::xs)) else
   if num < int64 0 then "=" + (encode (num + int64 2 * n0) ((n1, r1)::xs)) else
   "f"
  | (n0, r1)::[] -> if num = int64 2 * n0 then "2" else if num = n0 then "1" else "0"
  | _ -> failwith "Unrecognised pattern"
 encode number (m number (int64 1) (int64 2) [])

printf "Day 25: %A\n" (cifer translated)