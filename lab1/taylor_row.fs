
let main_func x = 1. / (2. * x - 5.)

let a = 0.
let b = 2.
let n = 10

let eps = 0.0000001


let main (x, n) = (2. ** (n - 1.)) * (x ** (n - 1.)) / (5. ** n)

let rec taylor_naive x =
    let rec loop cnt sum =
        let current_term = main(x, cnt)
        if abs current_term > eps then
            loop (cnt + 1.) (sum - current_term)
        else
            sum
    loop 1. 0.


let taylor_smart x =
    let initialTerm = -0.2
    let rec loop n term sum =
        if abs term < eps then sum
        else
            let nextTerm = term * 2. * x / 5.
            loop (n + 1.) nextTerm (sum + nextTerm)
    loop 1 initialTerm (initialTerm)

// Пример использования:
let final =
    for i = 0 to n do
        let x = a + float i / float n * (b - a)
        printfn "%5.2f  %10.6f  %10.6f %10.6f " x (main_func x) (taylor_naive x) (taylor_smart x)
