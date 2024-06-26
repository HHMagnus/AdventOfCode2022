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

let ore state req =
 let (_, x, _, _, _) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x <= s1 then Some(step state ((f1+1, f2, f3, f4), (s1-x, s2, s3, s4))) else
 None
let clay state req =
 let (_, _, x, _, _) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x <= s1 then Some(step state ((f1, f2+1, f3, f4), (s1-x, s2, s3, s4))) else
 None
let obsidian state req =
 let (_, _, _, (x1, x2), _) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x1 <= s1 && x2 <= s2 then Some(step state ((f1, f2, f3+1, f4), (s1-x1, s2-x2, s3, s4))) else
 None
let geode state req =
 let (_, _, _, _, (x1, x2)) = req
 let ((f1, f2, f3, f4), (s1, s2, s3, s4)) = state
 if x1 <= s1 && x2 <= s3 then Some(step state ((f1, f2, f3, f4+1), (s1-x1, s2, s3-x2, s4))) else
 None

let maxOre req =
 let (_, x1, x2, (x3, _), (x4, _)) = req
 x1 |> max x2 |> max x3 |> max x4

let prodOre state =
 let ((x, _, _, _), _) = state
 x

let maxClay req =
 let (_, _, _, (_, x), _) = req
 x

let prodClay state =
 let ((_, x, _, _), _) = state
 x

let maxObs req =
 let (_, _, _, _, (_, x)) = req
 x

let prodObs state =
 let ((_, _, x, _), _) = state
 x

let day19 maxI req =
 let rec run i ignoredOre ignoredClay ignoredObsidian state =
  if i = maxI then result state req else
  let optOre = ore state req
  let maxOr = maxOre req
  let prodOr = prodOre state
  let os =
   match (ignoredOre || maxOr <= prodOr, optOre) with
   | (false, Some(x)) -> run (i+1) false false false x
   | _ -> 0
  let optClay = clay state req
  let maxC = maxClay req
  let prodC = prodClay state
  let cs =
   match (ignoredClay || maxC <= prodC, optClay) with
   | (false, Some(x)) -> run (i+1) false false false x
   | _ -> 0
  let optObs = obsidian state req
  let maxOb = maxObs req
  let prodOb = prodObs state
  let ob =
   match (ignoredObsidian || maxOb <= prodOb, optObs) with
   | (false, Some(x)) -> run (i+1) false false false x
   | _ -> 0
  let g =
   match geode state req with
   | Some(x) -> run (i+1) false false false x
   | None -> 0
  let alt = run (i+1) (optOre = None |> not) (optClay = None |> not) (optObs = None |> not) (step state state)
  os |> max cs |> max ob |> max g |> max alt
 run 0 false false false ((1, 0, 0, 0), (0, 0, 0, 0))

let part1 =
 Array.ofList reqs
 |> Array.Parallel.map (fun req -> day19 24 req)
 |> Array.toList
 |> List.sum

printf "Day 19 part 1: %A\n" part1

let part2Reqs =
 List.take 3 reqs
 |> List.map (fun (_, x2, x3, x4, x5) -> (1, x2, x3, x4, x5))

let part2res =
 Array.ofList part2Reqs
 |> Array.Parallel.map (fun req -> day19 32 req)
 |> Array.toList

let part2 = part2res[0] * part2res[1] * part2res[2]

printf "Day 19 part 2: %A\n" part2