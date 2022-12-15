let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day15.txt" |> List.ofSeq

let parse (txt: string) =
 let b = txt.Split(": closest beacon is at x=")
 let xy = b[0].Split(", y=")
 let x = xy[0].Substring("Sensor at x=".Length)
 let y = xy[1]
 let bxy = b[1].Split(", y=")
 let bx = bxy[0]
 let by = bxy[1]
 (int x, int y), (int bx, int by)

let sbs = input |> List.map parse

let dist (x1: int, y1: int) (x2, y2)= abs (x1 - x2) + abs (y1 - y2)

let ds = List.map (fun (a, b) -> dist a b) sbs

let beacons = sbs |> List.map snd

let minX = sbs |> List.collect (fun (x, y) -> [fst x; fst y]) |> List.min |> (fun x -> (-) x (List.max ds))
let maxX = sbs |> List.collect (fun (x, y) -> [fst x; fst y]) |> List.max |> (+) (List.max ds)

let dsb = List.zip ds (List.map fst sbs)

let inRangeOfABeacon (x: int, y: int) =
 List.fold (fun s (a, b) -> s || (dist (x,y) b) <= a ) false dsb

let inRangeButNotBeacon (x,y) =
 inRangeOfABeacon (x,y) && beacons |> List.contains (x,y) |> not

let y = 2000000
[minX .. maxX]
|> List.map (fun x -> (x, y))
|> List.map inRangeButNotBeacon
|> List.filter id
|> List.length
|> printf "Day 15 (1): %A\n"