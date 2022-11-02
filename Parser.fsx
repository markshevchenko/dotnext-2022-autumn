open System
open System.Globalization

let whereChar predicate = function
    | c::cs when predicate c -> Some (c.ToString()), cs
    | cs -> None, cs
    
let isChar c = whereChar ((=) c)

isChar 'c' ['c'; '1'; '2']
['c'; '1'; '2'] |> isChar 'c'
['c'; '1'; '2'] |> isChar 'a'
[] |> isChar 'c'

let rec repeated parser chars =
    match parser chars with
    | Some result, cs -> let results, cs = repeated parser cs 
                         result::results, cs
    | None, _ -> [], chars
    
repeated (whereChar Char.IsDigit) ['1'; '3'; '7']
    
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
    
let digits = whereChar Char.IsDigit
          |> repeated1
          |> map (String.concat "")

"321" |> List.ofSeq |> digits

let (.>.) parser1 parser2 combinator chars =
    match parser1 chars with
    | Some s1, cs1 -> match parser2 cs1 with
                      | Some s2, cs2 -> Some (combinator s1  s2), cs2
                      | _ -> None, chars
    | _ -> None, chars
    
"a3" |> List.ofSeq |> (whereChar Char.IsLetter .>. whereChar Char.IsDigit) (+)

let optional defaultValue parser chars =
    match parser chars with
    | Some s, cs -> Some s, cs
    | None, cs -> Some defaultValue, cs
    
"a" |> List.ofSeq |> optional "NONE" (whereChar Char.IsLetter) 
"1" |> List.ofSeq |> optional "NONE" (whereChar Char.IsLetter) 
    
let number = (digits .>. ((isChar '.' .>. digits) (+) |> optional "")) (+)
          |> map (fun s -> Convert.ToDouble(s, CultureInfo.InvariantCulture))
          
"321" |> List.ofSeq |> number
"3.14159265" |> List.ofSeq |> number

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

//

"+3.14159265" |> List.ofSeq |> plus
"-3.14159265" |> List.ofSeq |> minus
"*3.14159265" |> List.ofSeq |> star
"/3.14159265" |> List.ofSeq |> slash
"1*2*3/4" |> List.ofSeq |> expression
"1*2+3*4" |> List.ofSeq |> expression
"1*(2+3)*4" |> List.ofSeq |> expression
"sin(3.1415/6)" |> List.ofSeq |> expression
