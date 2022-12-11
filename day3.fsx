let readLines filePath = System.IO.File.ReadLines(filePath);;

let lines = readLines "day3.txt" |> List.ofSeq

// http://fssnip.net/6A
// https://stackoverflow.com/a/5062440
module Seq =
  /// Iterates over elements of the input sequence and groups adjacent elements.
  /// A new group is started when the specified predicate holds about the element
  /// of the sequence (and at the beginning of the iteration).
  ///
  /// For example: 
  ///    Seq.groupWhen isOdd [3;3;2;4;1;2] = seq [[3]; [3; 2; 4]; [1; 2]]
  let groupWhen f (input:seq<_>) = seq {
    use en = input.GetEnumerator()
    let running = ref true
    
    // Generate a group starting with the current element. Stops generating
    // when it founds element such that 'f en.Current' is 'true'
    let rec group() = 
      [ yield en.Current
        if en.MoveNext() then
          if not (f en.Current) then yield! group() 
        else running := false ]
    
    if en.MoveNext() then
      // While there are still elements, start a new group
      while running.Value do
        yield group() |> Seq.ofList }

let rec common (y:string) (x: char list) =
 match x with
 | z :: tail when y.Contains(z) -> z
 | _ :: tail -> common y tail

let fc (x: string) = common x[.. x.Length/2-1] (x[x.Length/2 ..].ToCharArray() |> List.ofSeq)

let point (x: char) =
 match x with
 | x when int x < int 'a' -> int x - int 'A' + 27
 | x -> int x - int 'a' + 1

lines |> List.map fc |> List.map point |> List.sum |> printf "Day 3 (1): %i\n"

let groups =
 lines
 |> Seq.ofList
 |> Seq.mapi (fun i v -> i, v)
 |> Seq.groupWhen (fun (i, v) -> i%3 = 0)
 |> Seq.map (Seq.map snd)
 |> List.ofSeq
 |> List.map List.ofSeq

let rec common3 (x: string) (y: string) (z: char list) = 
 match z with
 | a :: tail when x.Contains(a) && y.Contains(a) -> a
 | _ :: tail -> common3 x y tail

groups |> List.map (fun x -> common3 x[0] x[1] (x[2].ToCharArray() |> List.ofSeq)) |> List.map point |> List.sum |> printf "Day 3 (2): %i\n"