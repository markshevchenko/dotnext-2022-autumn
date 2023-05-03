let inline square x = x * x // type inference thanks to inline

square 2
square 4.0

let inline cube x = x * x * x

cube 3
cube 5.0

// generic constants
let inline average a b = (a + b) / (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne) 

average 1 3
average 4.0 5.0

sqrt 2.0

let sqrt1 z =
    let rec iter x =
        if abs (z - square x) <= abs (0.001 * z) // parentheses
        then x
        else iter (average x (z / x)) // another parentheses

    iter 1.0 // type inference requires exact types

sqrt1 2.0

let cube_root1 z =
    let rec iter x =
        if abs (z - cube x) <= abs (0.001 * z)
        then x
        else iter (average x (z / square x))

    iter 1.0

cube_root1 8.0

let cube_root2 z =
    let next x = average x (z / square x)
    let enough x = abs (z - cube x) <= abs (0.001 * z)
    let xs = Seq.unfold (fun x -> Some (next x, next x)) 1.0 
    Seq.find enough xs // it may be done with syntax similar fluent

cube_root2 8.0

//

let fixed_point1 f x1 =
    let rec try_guess x =
        let x_next = f x
        if abs (x - x_next) < abs (0.001 * x)
        then x_next
        else try_guess x_next
    
    try_guess x1
    
fixed_point1 (fun x -> 1.0 + 1.0/x) 1.0

let fixed_point2 f x1 =
    x1 |> Seq.unfold (fun x -> Some (f x, f x)) // pipes similar to fluent syntax
       |> Seq.pairwise
       |> Seq.find (fun (x, x_next) -> abs (x - x_next) < abs (0.001 * x))
       |> snd

fixed_point2 (fun x -> 1.0 + 1.0/x) 1.0

//
    
// let sqrt2 z = fixed_point (fun x -> z / x) 1.0
let sqrt2 z = fixed_point2 (fun x -> average x (z / x)) 1.0

sqrt2 2.0

let cube_root3 z = fixed_point2 (fun x -> average x (z / square x)) 1.0

cube_root3 8.0

let inline average_damp f =
    fun x -> average x (f x)

(average_damp square) 10.0

let sqrt3 z = fixed_point2 (average_damp (fun x -> z / x)) 1.0

sqrt3 2.0

let cube_root4 z = fixed_point2 (average_damp (fun x -> z / square x)) 1.0

cube_root4 8.0

let forth_root1 z = fixed_point2 (average_damp (fun x -> z / cube x)) 1.0

forth_root1 16.0

//

let derive f =
    fun x -> let dx = min (abs x * 0.001) 0.000001
             ((f (x + dx)) - (f (x - dx))) / (2.0 * dx)
    
(derive cube) 5.0
    
let newton_transform g =
    fun x -> x - (g x)/((derive g) x)
    
let newton_method f = fixed_point2 (newton_transform f)
// let newton_method f x1 = fixed_point2 (newton_transform f) x1

let sqrt4 z = newton_method (fun x -> (square x - z)) 1.0
    
sqrt4 2.0

let cube_root5 z = newton_method (fun x -> (cube x - z)) 1.0

cube_root5 8.0

let fixed_point_of_transform f t = fixed_point2 (t f)

let sqrt5 z = fixed_point_of_transform (fun x -> z / x) average_damp 1.0

sqrt5 2.0

let sqrt6 z = fixed_point_of_transform (fun x -> square x - z) newton_transform 1.0

sqrt6 2.0

let fourth_root1 y = fixed_point_of_transform (fun x -> y / cube x) average_damp 1.0

fourth_root1 625.0
    
let inline double f = fun x -> (f (f x))
    
double sqrt6 625.0
    
let inline compose f g = fun x -> (f (g x))
    
let inc = ((+) 1.0) // operators are functions
    
compose cube_root3 inc 7.0
(cube_root3 << inc) 7.0
(inc >> cube_root3) 7.0

let fourth_root2 z = newton_method (fun x -> (double square) x - z) 1.0 

fourth_root2 625.0

let sixth_root1 z = newton_method (fun x -> (square >> cube) x - z) 1.0

sixth_root1 729.0

let power1 x n =
    Seq.replicate n x |> Seq.fold (*) 1.0
    
power1 3.0 6

let rec repeat f n =
    if n = 1
    then f
    else f >> (repeat f (n - 1))
    
let power2 x n = (repeat ((*) x) n) 1.0

power2 3.0 6

let root z n = newton_method (fun x -> power2 x n - z) 1.0

root 729.0 6