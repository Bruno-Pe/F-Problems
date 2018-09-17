//question 1 CFG 1
type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF
type parse_tree = 
    |Null //if sub trees aren't filled
    |Lf of TERMINAL
    |BR of TERMINAL * parse_tree * parse_tree //terminal and children

let eat token = function
    |[] -> failwith "Error in eat"
    |x::xs ->
        if x = token
        then xs
        else failwith (sprintf "Wrong token %A, wanted %A" x token)

let E = function
    |[] -> failwith "Syntax Error in E"
    |xs -> eat ID xs

let rec L = function
    |[] -> failwith "Syntax Error in L"
    |x::xs -> match x with
        |END -> xs
        |SEMICOLON -> xs |> S |> L
        |_ -> failwith (sprintf"Syntax Error in L match with token %A" x)

and S = function
    |[] -> failwith "Syntax Error: Empty program in S"
    |x::xs -> match x with
        |IF -> xs |> E |> eat THEN |> S |> eat ELSE |> S
        |BEGIN -> xs |> S |> L
        |PRINT -> xs |> E
        |_ -> failwith (sprintf "Syntax Error in S match with token %A" x)

let accept() = sprintf "Program is syntactically correct"
let error() = sprintf "Program is not correct"

let test_program program =
      let result = program |> S
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs ->  if x = EOF then accept() else error()

//question 2 CFG 2
type TERMINAL2 = ID|PLUS|MINUS|TIMES|DIVIDES|OPENPAREN|ENDPAREN

let eat2 token = function
    |[] -> failwith "Error in eat"
    |x::xs ->
        if x = token
        then xs
        else failwith (sprintf "Wrong token %A, wanted %A" x token)

let rec E2 = function
    |[] -> failwith "Empty Program in E2"
    |xs -> let res = T xs in match res with
       |r::res -> match r with
            |PLUS -> res |> T
            |MINUS -> res |> T
            |ENDPAREN -> ENDPAREN :: res
            |_ -> failwith (sprintf "Wrong token in E2 %A" r)
       |[] -> []
and T = function
    |[] -> failwith "Empty Program in T"
    |xs -> let res = F xs in match res with
        |r::res -> match r with
            |TIMES -> res |> F
            |DIVIDES -> res |> F
            |PLUS -> PLUS::res
            |MINUS -> MINUS::res
            |_ -> failwith (sprintf "Wrong token in T %A" r)
        |[] -> []
and F = function
    |[] -> failwith "Empty program in F"
    |x::xs -> match x with
        |OPENPAREN -> xs |> E2 |> eat2 ENDPAREN
        |ID -> xs
        |_ -> failwith "Invalid token in F"
