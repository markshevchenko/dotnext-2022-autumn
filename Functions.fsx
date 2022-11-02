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
        if abs (x - (square guess)) <= x * 0.001
        then guess
        else iter (average guess (x / guess))

    iter 1.0

sqrt1 2.0

let cube_root1 x =
    let rec iter guess =
        if abs (x - (cube guess)) <= x * 0.001
        then guess
        else iter (average guess (x / (square guess)))

    iter 1.0

cube_root1 8.0

//

let fixed_point f first_guess =
    let rec try_guess guess =
        let next = f guess
        if abs (guess - next) < abs (0.001 * guess) then next
        else try_guess next

    try_guess first_guess

fixed_point (fun x -> 1.0 + 1.0/x) 1.0
    
// let sqrt2 y = fixed_point (fun x -> y / x) 1.0
let sqrt2 y = fixed_point (fun x -> average x (y / x)) 1.0

sqrt2 2.0

let average_damp (f: float -> float) =
    fun x -> average x (f x)

(average_damp square) 10.0

let sqrt3 y = fixed_point (average_damp (fun x -> y / x)) 1.0

sqrt3 2.0

let cube_root2 y = fixed_point (average_damp (fun x -> y / square x)) 1.0

cube_root2 8.0

//

let derive g =
    fun x -> let dx = abs x * 0.001
             ((g (x + dx)) - (g (x - dx))) / (2.0 * dx)
    
(derive cube) 5.0
    
let newton_transform g =
    fun x -> x - (g x)/((derive g) x)
    
let newton_method f guess =
    fixed_point (newton_transform f) guess

let sqrt4 y = newton_method (fun x -> (square x - y)) 1.0
    
sqrt4 2.0

let cube_root3 y = newton_method (fun x -> (cube x - y)) 1.0

cube_root3 8.0

let fixed_point_of_transform f transform guess =
    fixed_point (transform f) guess
    
let sqrt5 y = fixed_point_of_transform (fun x -> y / x) average_damp 1.0

sqrt5 2.0

let sqrt6 y = fixed_point_of_transform (fun x -> square x - y) newton_transform 1.0

sqrt6 2.0

// let fourth_root y = fixed_point_of_transform (fun x -> y / cube x) average_damp 1.0
    
let inline double f =
    fun x -> (f (f x))
    
double sqrt6 625.0
    
let inline compose f g =
    fun x -> (f (g x))
    
let inc = ((+) 1.0)
    
compose cube_root3 inc 7.0
(cube_root3 << inc) 7.0
(inc >> cube_root3) 7.0
    
let rec repeat f n =
    if n = 1
    then f
    else f >> repeat f (n - 1)
    
(repeat square 2) 5
    
let fourth_root1 y = fixed_point_of_transform (fun x -> y / cube x) (repeat average_damp 2) 1.0

fourth_root1 625.0

let fourth_root2 y = newton_method (fun x -> (double square) x - y) 1.0 

fourth_root2 625.0
