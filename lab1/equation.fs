let eps = 0.0000001

let rec dichotomy f a b =
    let fa = f a
    let fb = f b
    let rec loop a b =
        let c = (a + b) / 2.
        let fc = f c
        if abs(fc) < eps then c
        elif fa * fc < 0. then loop a c
        else loop c b
    loop a b

let rec iterations f x0 =
    let x1 = f x0
    if abs(x1 - x0) < eps then
        x1
    else
        iterations f x1

let rec newton g x =
    let x1 = g x
    if abs(x1 - x) < eps then x1
    else newton g x1

// Определения функций и производных
let f1 x = 0.6 * 3. ** x - 2.3 * x - 3.
let f2 x = x * x - log(1. + x) - 3.
let f3 x = 2. * x * sin(x) - cos(x)

let f1' x = 1.8 * 3. ** x - 2.3
let f2' x = 2. * x - 1. / (1. + x)
let f3' x = 2. * sin(x) + 2. * x * cos(x)

let g1 x = x - f1 x / f1' x
let g2 x = x - f2 x / f2' x
let g3 x = x - f3 x / f3' x

let phi1 x = (0.6 * 3. ** x - 3.) / 2.3
let phi2 x = sqrt(log(1. + x) + 3.)
let phi3 x = cos(x) / (2. * sin(x))

let main =
    printfn "Dichotomy for f1: %10.5f" (dichotomy f1 2. 3.)
    // printfn "Iterations for f1: %10.5f" (iterations phi1 2.5) // это не выводится
    printfn "Newton for f1: %10.5f" (newton g1 3.)

    printfn "Dichotomy for f2: %10.5f" (dichotomy f2 2. 3.) 
    printfn "Iterations for f2: %10.5f" (iterations phi2 3.)
    printfn "Newton for f2: %10.5f" (newton g2 3.)

    printfn "Dichotomy for f3: %10.5f" (dichotomy f3 0.4 1.)
    printfn "Iterations for f3: %10.5f" (iterations phi3 0.5)
    printfn "Newton for f3: %10.5f" (newton g3 0.5)

main
