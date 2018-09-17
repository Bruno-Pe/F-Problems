let rec powerset = function
    |[] -> [[]]
    //take the head of the list and make a recursive call with the rest of the list
    //and append it to the mapped list using the function defined within
    |x::xs -> let list = powerset xs in list @ List.map (fun ys -> x::ys) list

let rec transpose = function
    |[]::xs -> []
    //take heads of each list then cons the tails of that list on the recursive call
    |xs -> List.map List.head xs :: transpose (List.map List.tail xs)

(*
  Function output is incorrect
  Although each recursive call is done on smaller input
  and the base cases are correct, The non-base case does 
  not return the correct input. The function only takes the
  largest number in the list to the end. Otherwise, it does
  not actually sort the middle of the list consistently.
  For example, inputting list [3;2;1] returns [2;1;3]
*)
let rec sort = function
    |[] -> []
    |[x] -> [x] 
    |x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs) else x2 :: sort (x1::xs)

let rec merge = function
    |([], ys) -> ys
    |(xs, []) -> xs
    |(x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys) else y :: merge (x::xs, ys)

let rec split = function
    | [] -> ([], [])
    | [a] -> ([a], [])
    | a::b::cs -> let (M,N) = split cs in (a::M, b::N)
(*
  With respect to the recursive programming checklist,
  In the original function, the base case is incorrect.
  Normally in the mergesort algorithm, the recursion ends once
  the list is split into 1 length lists. Otherwise, the recursive
  calls are done on smaller input and return the correct answer.
*)
let rec originalmergesort = function
    | [] -> []
    | L -> let (M, N) = split L in merge (originalmergesort M, originalmergesort N)
(*
  Based on the return type of originalmergesort
  and the incorrect base case, all that needs to 
  be done is correct the base case
*)

let rec mergesort = function
    | [x] -> [x] //return single length list
    | L -> let (M, N) = split L in merge (mergesort M, mergesort N)