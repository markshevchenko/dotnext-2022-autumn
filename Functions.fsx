let inline square x = x * x

square 2
square 4.0

let inline cube x = x * x * x

cube 3
cube 5.0

let inline average a b = (a + b) / (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne) 

average 1 3
average 4.0 5.0

sqrt 2.0

let sqrt1 x =
    let rec iter guess =
        if abs (x - guess * guess) <= x * 0.001
        then guess
        else iter ((guess + x / guess) / 2.0)

    iter 1.0

sqrt1 2.0

let fixed_point f first_guess =
    let rec try_guess guess =
        let next = f guess
        if abs (guess - next) < abs (0.001 * guess) then next
        else try_guess next

    try_guess first_guess

fixed_point cos 1.0
fixed_point (fun x -> sin x + cos x) 1.0
fixed_point (fun x -> 1.0 + 1.0/x) 1.0
    
//let sqrt2 x = fixed_point (fun y -> x / y) 1.0
let sqrt2 x = fixed_point (fun y -> average y (x / y)) 1.0

sqrt2 2.0

let average_damp (f: float -> float) =
    fun x -> average x (f x)

(average_damp square) 10.0

let sqrt3 x = fixed_point (average_damp (fun y -> x / y)) 1.0

sqrt3 2.0

let cube_root1 x = fixed_point (average_damp (fun y -> x / (y * y))) 1.0

cube_root1 8.0

let derive g =
    fun x -> let dx = abs x * 0.001
             ((g (x + dx)) - (g x))/dx
    
(derive cube) 5.0
    
let newton_transform g =
    fun x -> x - (g x)/((derive g) x)
    
let newton_method f guess =
    fixed_point (newton_transform f) guess


let sqrt4 x = newton_method (fun y -> (square y - x)) 1.0
    
sqrt4 2.0

let cube_root2 x = newton_method (fun y -> (cube y - x)) 1.0

cube_root2 8.0
    
let fixed_point_of_transform f transform guess =
    fixed_point (transform f) guess
    
let sqrt5 x = fixed_point_of_transform (fun y -> x / y) average_damp 1.0

sqrt5 2.0

let sqrt6 x = fixed_point_of_transform (fun y -> square y - x) newton_transform 1.0

sqrt6 2.0
    
let inline double f =
    fun x -> (f (f x))
    
double ((+) 1) 5
    
let inline compose f g =
    fun x -> (f (g x))
    
compose square ((+)1) 6
(square << ((+)1)) 6
(((+)1) >> square) 6
    
let rec repeate f n =
    if n = 1
    then f
    else f >> repeate f (n - 1)
    
(repeate square 2) 5
    
// let fourth_root x = fixed_point_of_transform (fun y -> x / cube y) average_damp 1.0
let fourth_root x = fixed_point_of_transform (fun y -> x / cube y) (repeate average_damp 2) 1.0

fourth_root 625.0
