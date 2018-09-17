//question 3
let uncurry f (a,b) = f a b
let curry f a b = f (a,b)

//question 4
let rec auxinner = function
    |([],[],a) -> a//return accumulator
    |([],ys,_) -> failwith "2nd list longer than 1st"
    |(xs,[],_) -> failwith "1st list longer than 2nd"
    |(x::xs, y::ys, a) -> auxinner(xs, ys, a + x * y)

let inner xs ys = auxinner(xs,ys, 0)
//question 5
let rec transpose = function
    |[]::xs -> []
    |xs -> List.map List.head xs :: transpose (List.map List.tail xs)

let rec multiply (xs,ys) = 
    let zs = transpose ys in match xs with
    |[] -> []
    |x::xs -> List.map (inner x) zs :: multiply (xs, ys)

//question 9
type Student = {Name: string; Credits: int; GPA: float};;
let student = {Name = "Jones"; Credits = 109; GPA = 3.85};;

//question 10
type 'a Coordinates =
    |Tuple of first: 'a * second: 'a
    |Threeple of first: 'a * second: 'a * third: 'a
    |Fourple of first: 'a * second: 'a * thid: 'a * fourth: 'a

let tuple = Tuple(5, 10)
let threeple = Threeple (1.1,2.6,3.3)
let fourple = Fourple("a","b","c","d")

let cordAdd f x=
    match x with
    |Tuple(a,b) -> f a b
    |Threeple(a,b,c) -> let y = f a b in f y c
    |Fourple(a,b,c,d) -> 
        let y = f c d in
        let z = f b y in
        f a z
