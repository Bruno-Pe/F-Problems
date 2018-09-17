//Going through the problems in the problem set 1
let rec gcd = function
    | (a,0) -> a
    | (a,b) -> gcd (b, a % b)

let (.*) (a,b) (c,d) = let e = gcd(a*c, b*d) in ((a*c)/e, (b*d)/e)
let (.+) (a,b) (c,d) = let e = gcd((a*d + c*b), (b*d)) in ((a*d + c*b)/e, (b*d)/e)

let revlists xs = List.map List.rev xs

//assuming both lists have same length
let rec interleave = function
    |([],[]) -> []
    |(x::xs, y::ys) -> x::y::interleave(xs,ys)

let gencut (n,xs) =
    //local function makes a 3-tuple with 2 lists to append values from original list
    let rec auxgencut = function
        //list must be reversed because of how the list is being built
        |(0,xs,ys) -> (List.rev xs, ys)
        |(n,xs,[]) -> (xs, [])
        |(n,xs,y::ys) -> auxgencut(n-1,y::xs, ys)
    auxgencut(n, [], xs);;

let cut xs =
    let l = (List.length xs)/2
    gencut(l,xs)

let shuffle xs=
    interleave (cut xs)

let countaux (deck, target) = 
    let rec auxcount = function
        |(n,deck,target) when deck = target -> n
        |(n,deck,target) when deck <> target -> auxcount(n+1, shuffle deck, target)
    auxcount(1, deck, target);;

let countshuffles n =
    let target = [1..n]
    let deck = target
    countaux(shuffle deck, target)

let rec cartesian = function
    |([],ys) -> []
    |(x::xs, ys) -> List.map(fun y -> (x,y)) ys @ cartesian (xs,ys)