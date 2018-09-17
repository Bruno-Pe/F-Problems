(*
    practice declaring functions,
    passing functions as parameters,
    using recursion for looping,
    and manipulating lists with recursion.
*)

//takes function as first parameter and 2 arguments
let basic1 x n1 n2 = (x n1 n2)

//declaration of above function with different syntax
let basic2 = fun x n1 n2 -> x n1 n2

//recursive function 
let rec fact a=
    match a with
    |0 -> 1
    |a -> a * fact (a-1);;


//used to have fact passed to "basic" functions
let factPlus a b = (fact a) + b

//Sums all elements in an array using recursion
let rec sum1 (xs,ys) = 
    match (xs,ys) with
    |([],[]) -> 0 //both lists empty exits recursive loop
    |(xs,[]) -> sum1 (xs, [0])
    |([], ys) -> sum1 ([0], ys)
    |(x::xs,y::ys) -> x + y + sum1(xs,ys);; //adds heads and sends in tails