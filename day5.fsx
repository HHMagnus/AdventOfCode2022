let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines =
 readLines "day5.txt"
 |> List.ofSeq
 |> List.skip 10
 |> List.map (fun (x: string) -> x.Substring(5))

(* 
        [H]     [W] [B]            
    [D] [B]     [L] [G] [N]        
[P] [J] [T]     [M] [R] [D]        
[V] [F] [V]     [F] [Z] [B]     [C]
[Z] [V] [S]     [G] [H] [C] [Q] [R]
[W] [W] [L] [J] [B] [V] [P] [B] [Z]
[D] [S] [M] [S] [Z] [W] [J] [T] [G]
[T] [L] [Z] [R] [C] [Q] [V] [P] [H]
 1   2   3   4   5   6   7   8   9 
*)

let data: char list list =
 [ 
    ['T'; 'D'; 'W'; 'Z'; 'V'; 'P'];
    ['L'; 'S'; 'W'; 'V'; 'F'; 'J'; 'D'];
    ['Z'; 'M'; 'L'; 'S'; 'V'; 'T'; 'B'; 'H'];
    ['R'; 'S'; 'J'];
    ['C'; 'Z'; 'B'; 'G'; 'F'; 'M'; 'L'; 'W'];
    ['Q'; 'W'; 'V'; 'H'; 'Z'; 'R'; 'G'; 'B'];
    ['V'; 'J'; 'P'; 'C'; 'B'; 'D'; 'N'];
    ['P'; 'T'; 'B'; 'Q'];
    ['H'; 'G'; 'Z'; 'R'; 'C']
 ] |> List.map List.rev

let move1 x y = 
 match x with
 | z :: zs -> zs, z::y
 | [] -> x, y

let rec mova1 a x y =
 match a with
 | 0 -> x, y
 | _ -> (move1 x y) ||> mova1 (a-1)

let uv1 (d: char list list) a x y =
 let xs = d[x]
 let ys = d[y]
 let res = mova1 a xs ys
 match x < y with
 | true -> d[..x-1] @ [fst res] @ d[x+1..y-1] @ [snd res] @ d[y+1..]
 | false -> d[..y-1] @ [snd res] @ d[y+1..x-1] @ [fst res] @ d[x+1..]

//printf "\n%A\n" data
//printf "\n%A\n" (uv data 2 1 8)
//printf "\n%A\n" (uv data 2 8 1)

let split (x: string) =
 let s1 = x.Split(" from ")
 let s2 = s1[1].Split(" to ")
 int s1[0], int s2[0]-1, int s2[1]-1

let cmds = lines |> List.map split

let day1Data = List.fold (fun s t -> t |||> uv1 s) data cmds

day1Data |> List.map List.head |> List.map (sprintf "%c") |> Seq.ofList |> String.concat "" |> printf "Day 5 (1): %s\n"

let rec mova2 a x y =
 List.skip a x, List.take a x @ y

let uv2 (d: char list list) a x y =
 let xs = d[x]
 let ys = d[y]
 let res = mova2 a xs ys
 match x < y with
 | true -> d[..x-1] @ [fst res] @ d[x+1..y-1] @ [snd res] @ d[y+1..]
 | false -> d[..y-1] @ [snd res] @ d[y+1..x-1] @ [fst res] @ d[x+1..]

let day2Data = List.fold (fun s t -> t |||> uv2 s) data cmds

day2Data |> List.map List.head |> List.map (sprintf "%c") |> Seq.ofList |> String.concat "" |> printf "Day 5 (2): %s\n"