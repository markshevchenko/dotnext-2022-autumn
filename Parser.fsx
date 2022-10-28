open System
open System.Globalization

let whereChar predicate = function
    | c::cs when predicate c -> Some (c.ToString()), cs
    | cs -> None, cs
    
let isChar c = whereChar ((=) c)

['c'; '1'; '2'] |> isChar 'c' |> printfn "%A"
['c'; '1'; '2'] |> isChar 'a' |> printfn "%A"
[] |> isChar 'c' |> printfn "%A"

let rec internal repeated parser chars =
    match parser chars with
    | Some result, cs -> let results, cs = repeated parser cs 
                         result::results, cs
    | None, _ -> [], chars
    
let repeated0 parser chars =
    let results, cs = repeated parser chars
    Some results, cs
    
let repeated1 parser chars =
    match parser chars with
    | Some result, cs -> let results, cs = repeated parser cs
                         Some (result::results), cs
    | None, chars -> None, chars
    
let map mapper parser chars =
    let result, cs = parser chars
    Option.map mapper result, cs
    
let digits = whereChar Char.IsDigit |> repeated1 |> map (String.concat "")

"321" |> List.ofSeq |> digits |> printfn "%A"

let (.>.) parser1 parser2 combinator chars =
    match parser1 chars with
    | Some s1, cs1 -> match parser2 cs1 with
                      | Some s2, cs2 -> Some (combinator s1  s2), cs2
                      | _ -> None, chars
    | _ -> None, chars

let optional defaultValue parser chars =
    match parser chars with
    | Some s, cs -> Some s, cs
    | None, cs -> Some defaultValue, cs
    
let number = (digits .>. ((isChar '.' .>. digits) (+) |> optional "")) (+)
          |> map (fun s -> Convert.ToDouble(s, CultureInfo.InvariantCulture))
          
"321" |> List.ofSeq |> number |> printfn "%A"
"3.14159265" |> List.ofSeq |> number |> printfn "%A"

let (>>.) parser1 parser2 chars =
    match parser1 chars with
    | Some _, cs1 -> match parser2 cs1 with
                     | Some result2, cs2 -> Some result2, cs2
                     | _ -> None, chars
    | _ -> None, chars

let (.>>) parser1 parser2 chars =
    match parser1 chars with
    | Some result1, cs1 -> match parser2 cs1 with
                           | Some _, cs2 -> Some result1, cs2
                           | _ -> None, chars
    | _ -> None, chars

let (<|>) parser1 parser2 chars =
    match parser1 chars with
    | Some result, cs -> Some result, cs
    | None, cs -> parser2 cs
    
let rec value chars = (number
                   <|> (isChar '(' >>. expression .>> isChar ')')
                   <|> (isChar 's' >>. isChar 'i' >>. isChar 'n' >>. value |> map sin)) chars
and star = isChar '*' >>. value
and slash = isChar '/' >>. value |> map ((/)1.0)
and term = (value .>. repeated0 (star <|> slash)) (fun v vs -> v * List.fold (*) 1.0 vs)
and plus = isChar '+' >>. term
and minus = isChar '-' >>. term |> map (~-)
and expression = (term .>. repeated0 (plus <|> minus)) (fun v vs -> v + List.sum vs) 

"+3.14159265" |> List.ofSeq |> plus |> printfn "%A"
"-3.14159265" |> List.ofSeq |> minus |> printfn "%A"
"*3.14159265" |> List.ofSeq |> star |> printfn "%A"
"/3.14159265" |> List.ofSeq |> slash |> printfn "%A"
"1*2*3/4" |> List.ofSeq |> expression |> printfn "%A"
"1*2+3*4" |> List.ofSeq |> expression |> printfn "%A"
"1*(2+3)*4" |> List.ofSeq |> expression |> printfn "%A"
"sin(3.1415/6)" |> List.ofSeq |> expression |> printfn "%A"
