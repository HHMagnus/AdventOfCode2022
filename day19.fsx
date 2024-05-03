let readLines filePath = System.IO.File.ReadLines(filePath);;

let input = readLines "day19.txt" |> List.ofSeq

let parse (txt: string) =
 let split = txt.Split(": ")
 let blueprint = split[0].Replace("Blueprint ", "") |> int
 let split2 = split[1].Split(". ")
 let ore = split2[0].Replace("Each ore robot costs ", "").Replace(" ore", "") |> int
 let clay = split2[1].Replace("Each clay robot costs ", "").Replace(" ore", "") |> int
 let obsplit = split2[2].Split(" ore and ")
 let obore = obsplit[0].Replace("Each obsidian robot costs ", "") |> int
 let obclay = obsplit[1].Replace(" clay", "") |> int
 let geodesplit = split2[3].Split(" ore and ")
 let geodeore = geodesplit[0].Replace("Each geode robot costs ", "") |> int
 let geodeobsidian = geodesplit[1].Replace(" obsidian.", "") |> int
 blueprint, ore, clay, (obore, obclay), (geodeore, geodeobsidian)

let reqs = List.map parse input

let step oldState state = 
 let (f1, f2, f3, f4) = fst state
 let (o1, o2, o3, o4) = fst oldState
 let (s1, s2, s3, s4) = snd state
 ((f1, f2, f3, f4), (o1+s1, o2+s2, o3+s3, o4+s4))

let result state req =
 let (x, _, _, _, _) = req
 let (_, (_, _, _, y)) = state
 x * y

let rec day19 i state req =
 if i = 24 then result state req else
 let x4 = geode i state req
 let x3 = if i > 20 then 0 else obsidian i state req
 let x2 = if i > 18 then 0 else clay i state req
 let x1 = if i > 18 then 0 else ore i state req
 let nothing = go i state state req
 x1 |> max x2 |> max x3 |> max x4 |> max nothing
and go i oldState newState req =
 let next = step oldState newState
 day19 (i+1) next req
and ore i state req =
 let (_, x, _, _, _) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x <= s1 then go i state ((f1+1, f2, f3, f4), (s1-x, s2, s3, s4)) req else
 0
and clay i state req =
 let (_, _, x, _, _) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x <= s1 then go i state ((f1, f2+1, f3, f4), (s1-x, s2, s3, s4)) req else
 0
and obsidian i state req =
 let (_, _, _, (x1, x2), _) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x1 <= s1 && x2 <= s2 then go i state ((f1, f2, f3+1, f4), (s1-x1, s2-x2, s3, s4)) req else
 0
and geode i state req =
 let (_, _, _, _, (x1, x2)) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x1 <= s1 && x2 <= s3 then go i state ((f1, f2, f3, f4+1), (s1-x1, s2, s3-x2, s4)) req else
 0

let part1 = Array.ofList reqs |> Array.Parallel.map (fun req -> day19 0 ((1, 0, 0, 0), (0, 0, 0, 0)) req) |> Array.toList |> List.sum

printf "Day 19 part 1: %A\n" part1 // 1038 too low