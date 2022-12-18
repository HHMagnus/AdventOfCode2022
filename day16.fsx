let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day16.txt" |> List.ofSeq

let parse (txt: string) =
 let split = txt.Split("; tunnels lead to valves ");
 let flowRate = split[0].Substring("Valve TU has flow rate=".Length);
 let outgoing = split[1].Split(", ") |> List.ofArray
 let coord = split[0].Substring("Valve ".Length, 2)
 (coord, (int flowRate, outgoing))

let coords = input |> List.map parse |> Map.ofList

let rec fullSearch coord minute score =
 if minute = 30 then score else
 let curr = coords[coord]
 match fst curr with
 | 0 -> List.map (fun x -> fullSearch x (minute+1) score) (snd curr) |> List.max
 | x -> 
   let newScore = score + ((29 - minute) * x)
   List.map (fun x -> fullSearch x (minute+1) newScore) (snd curr) |> List.max

fullSearch "AA" 0 0 |> printf "%A\n"