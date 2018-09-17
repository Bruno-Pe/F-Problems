//problem 1
type mixed =
    |Integer of int
    |Character of char

type 'a linkedlist=
    |NUL
    |NODE of 'a linkedlist * 'a

let rec generator = function
    |[] -> NUL
    |x::xs -> NODE(generator(xs), x)

//problem 5
let rec interleave_aux = function
    |([],[], zs) -> zs
    |([], y::ys, zs) -> y::zs
    |(x::xs, [], zs) ->  x::zs
    |(x::xs, y::ys, zs) -> interleave_aux(xs, ys, x::y::zs)

let interleave_tail (xs, ys) = interleave_aux(List.rev xs, List.rev ys, [])

let rec interleave = function
    |([],[]) -> []
    |([],y::ys) -> [y]
    |(x::xs, []) -> [x]
    |(x::xs, y::ys) -> x::y::interleave(xs,ys)

//problem 12
let fibonacci n =
    let prev1 = ref 0
    let prev2 = ref 1
    let ans = ref 1
    let count = ref 1
    while !count < n do
        ans := !prev2 + !prev1
        prev1 := !prev2
        prev2 := !ans
        count := !count + 1
    !ans

//problem 13
type Student = {getGpa : unit -> float; addCredits: int -> unit; addPoints: int -> unit};;

let st = let Credits = ref 0.0
         let gradePoints = ref 0.0
         {addCredits = fun c -> Credits := !Credits + float c;
          addPoints = fun p -> gradePoints := !gradePoints + float p;
          getGpa = fun () -> !gradePoints/ !Credits};;

//problem 6
let seqInfinite = Seq.initInfinite (fun index ->
     let n = float (index + 1) //start index at 1
     if n % 2.0 = 0.0 then -1.0 / (2.0** n) //make even indices negative and odd indices positive
     else 1.0 / (2.0** n))
let rec range (n,m) =
    match n with
    |n when n = m -> [Seq.item (m-1) seqInfinite]
    |n -> Seq.item (n-1) seqInfinite :: range(n+1, m)
    

type 'a stream = Cons of 'a * (unit -> 'a stream) //infinite stream

let rec alternating index = //index should be initialized to 1
    let n = index 
    if n % 2.0 = 0.0 then
        Cons(-1.0/(2.0**n), fun () -> alternating (n+1.0))
    else 
        Cons(1.0/(2.0**n), fun () -> alternating (n+1.0))

let startStream = alternating 1.0
let rec take n (Cons(x, xsf)) = if n = 0 then [] else x :: take (n-1) (xsf())
let rec drop n (Cons(x, xsf)) = if n = 0 then Cons (x, xsf) else drop (n-1) (xsf())

//problem 9
let rec expon_aux (n,acc) = 
    match n with
    |n when n = 0I-> acc
    |n -> expon_aux((n-1I), acc*2I)

let expon n = expon_aux(n, 1I)

//problem 7
let rec divisible n = 
    match n with
    |n when n%2 = 0 && n%3 = 0 && n%10 = 0 && n%21 = 0-> Cons(n, fun() -> divisible (n+1))
    |_ -> divisible (n+1)

let divs = divisible 1
let divRange (x,y) = take (y-x+1) (drop (x-1) divs)

let rec filter = function
    |n when n%2 = 0 && n%3 = 0 && n%10 = 0 && n%21 = 0-> true
    |n -> false

let rec divisibleBy x = if filter x then seq{yield x; yield! divisibleBy (x+1)} else divisibleBy (x+1)

let seqInfinite2 = divisibleBy 1
let rec range2 (n,m) =
    match n with
    |n when n = m -> [Seq.item (m-1) seqInfinite2]
    |n -> Seq.item (n-1) seqInfinite2 :: range2(n+1, m)

//problem 14
let mkstack init = 
    let stk = ref init
    ((fun x -> stk := x::!stk), //stack functions
     (fun() -> stk := List.tail !stk),
     (fun() -> List.head !stk),
     (fun() -> List.isEmpty !stk))

let (push, pop, top, isEmpty) = mkstack ( []: int list)

let factorial n =
    let ans = ref 1 //ref 1
    let count = ref 1
    while !count <= n do
        push(!count)
        count := !count + 1
    while not (isEmpty ()) do
        ans := !ans * top ()
        pop ()
    !ans

//problem 8
type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF
type tree =
    |LF of TERMINAL
    |BrIF of tree*tree*tree*tree*tree*tree
    |BrBEGIN of tree*tree*tree
    |BrPRINT of tree*tree
    |BrSEMI of tree*tree*tree

let eat token = function
    |[] -> failwith "Error in eat"
    |x::xs ->
        if x = token
        then xs
        else failwith (sprintf "Wrong token %A, wanted %A" x token)

let E = function
    |[] -> failwith "Syntax Error in E"
    |x::xs -> match x with
              |ID -> (xs, LF(ID))
              |_ -> failwith (sprintf"Syntax Error in E match with token %A" x)


let rec L = function
    |[] -> failwith "Syntax Error in L"
    |END::xs -> (xs, LF(END))
    |SEMICOLON::xs -> let(toks1,tree_S) = S xs
                      let(toks2, tree_L) = L toks1
                      (toks2, BrSEMI(LF(SEMICOLON),tree_S,tree_L))


and S = function
    |[] -> failwith "Syntax Error: Empty program in S"
    |IF::xs -> let (toks, tree_E) = E xs
               let toks2 = eat THEN toks
               let (toks3, tree_S1) = S toks2
               let toks4 = eat ELSE toks3
               let (toks5, tree_S2) = S toks4
               (toks5, BrIF(LF(IF), tree_E, LF(THEN),tree_S1,LF(ELSE),tree_S2))
    |BEGIN::xs -> let (toks, tree_S) = S xs
                  let (toks2, tree_L) = L toks
                  (toks2, BrBEGIN(LF(BEGIN),tree_S,tree_L))
    |PRINT::xs -> let (toks,tree_E) = E xs
                  (toks, BrPRINT(LF(PRINT),tree_E))
    |x::xs -> failwith (sprintf"Syntax Error in S match with token %A" x)

let accept() = sprintf "Program is syntactically correct"
let error() = sprintf "Program is not correct"

let rec print_tree = function
    |BrPRINT(lf1, lf2) -> print_tree lf1; print_tree lf2 //E evaluates to lf2 anyways since E will always be 
    |BrBEGIN(lf1, tree1, tree2) -> print_tree lf1; print_tree tree1; print_tree tree2
    |BrSEMI(lf1, tree1, tree2) -> print_tree lf1; print_tree tree1; print_tree tree2
    |BrIF(lf1, lf2, lf3, tree1, lf4, tree2) -> print_tree lf1; print_tree lf2; print_tree lf3 ; print_tree tree1; print_tree lf4; print_tree tree2
    |LF(t) -> printf "%A " t

let test_program program =
      let (result, tree1) = program |> S
      print_tree tree1

//problem 10
let twice f = (fun x -> f (f x));
let successor n = n+1;;
let a = (twice (twice (twice (twice successor)))) 0
let b = twice successor 0
let c = twice twice successor 0
let d = twice twice twice successor 0
let e = twice twice twice twice successor 0
//let f = twice twice twice twice twice successor 0 //Causes overflow

//problem 3
type Terminals2 = A|B|BAR|EOF2
type tree2 = 
    |LF2 of Terminals2
    |Br of tree2*tree2*tree2
(*
    S -> 'a'S'a' | 'b'S'b' | '|'
*)

let eat2 token = function
    |[] -> failwith "Error in eat"
    |x::xs ->
        if x = token
        then xs
        else failwith (sprintf "Wrong token %A, wanted %A" x token)

let rec parse = function
    |"" -> [EOF2]
    |s -> 
        match s.Chars 0 with
        |'a' -> A::parse(s.Substring 1)
        |'b' -> B::parse(s.Substring 1)
        |'|' -> BAR::parse(s.Substring 1)
        |c -> failwith (sprintf "Parsed invalid input %A" c)

let rec S1 = function
    |[] -> failwith "Early termination of input"
    |A::xs -> let(toks, tree_1) = S1 xs
              let toks2= eat2 A toks
              (toks2, Br(LF2(A), tree_1, LF2(A)))
    |B::xs -> let (toks, tree_1) = S1 xs
              let toks2 = eat2 B toks
              (toks2, Br(LF2(B),tree_1,LF2(B)))
    |BAR::xs -> (xs, LF2(BAR))

let rec print_tree2 = function
    |Br(lf1, tree_1, lf2) -> print_tree2 lf1; print_tree2 tree_1; print_tree2 lf2
    |LF2(t) -> printf "%A " t