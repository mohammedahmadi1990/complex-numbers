type Expression =
    | Complex of int * int
    | Neg of Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression


// task a -- toString method
let rec toString (expr: Expression) =
    match expr with
    | Complex (0, 0) -> "0"
    | Complex (0, im) -> sprintf "%di" im
    | Complex (rn, 0) -> sprintf "(%d)" rn    
    | Complex (rn, im) when im < 0 -> sprintf "%d %di" rn im
    | Complex (rn, im) when im = 1 -> sprintf "%d + i" rn
    | Complex (rn, im) -> sprintf "%d + %di" rn im    
    | Neg e -> sprintf "-(%s)" (toString e)
    | Add (e1, e2) -> sprintf "((%s)+(%s))" (toString e1) (toString e2)
    | Sub (e1, e2) -> sprintf "((%s)-(%s))" (toString e1) (toString e2)
    | Mul (e1, e2) -> sprintf "%s*%s" (toString e1) (toString e2)


// test-cases 1
let expr1 = Neg(Complex (3, 2))
printfn "%s" (toString expr1) // -(3 + 2i)

let expr2 = Add(Complex (3, 1), Complex (2, 4))
printfn "%s" (toString expr2) // ((3 + i)+(2 + 4i))

let expr3 = Neg(Mul(Complex (3, 0), Neg(Complex (2, 4))))
printfn "%s" (toString expr3) // -((3)*-(2 + 4i))




// task b -- evaluate method
let rec evaluate expr =
    match expr with
    | Complex _ -> expr
    | Neg expr1 ->
        let res = evaluate expr1
        match res with
        | Complex (r, i) -> Complex (-r, -i)
        | _ -> failwith "Error! Invalid Expression!"
    | Add (expr1, expr2) ->
        let res1 = evaluate expr1
        let res2 = evaluate expr2
        match (res1, res2) with
        | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1+r2, i1+i2)
        | _ -> failwith "Error! Invalid Expression!"
    | Sub (expr1, expr2) ->
        let res1 = evaluate expr1
        let res2 = evaluate expr2
        match (res1, res2) with
        | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1-r2, i1-i2)
        | _ -> failwith "Error! Invalid Expression!"
    | Mul (expr1, expr2) ->
        let res1 = evaluate expr1
        let res2 = evaluate expr2
        match (res1, res2) with
        | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1*r2-i1*i2, r1*i2+i1*r2)
        | _ -> failwith "Error! Invalid Expression!"


// test-cases 2
let expr4 = Add(Complex (3, 1), Complex (2, 4))
let res4 = evaluate (expr4)
printfn "%s" (toString res4) // Complex (5, 5)

let expr5 = Sub(Complex (3, 1), Complex (2, 4))
let res5 = evaluate (expr5)
printfn "%s" (toString res5) // Complex (1, -3)

let expr6 = Mul(Complex (3, 1), Complex (2, 4))
let res6 = evaluate (expr6)
printfn "%s" (toString res6) // Complex (2, 14)

let expr7 = Neg(Complex (3, 1))
let res7 = evaluate (expr7)
printfn "%s" (toString res7) // Complex (-3, -1)

let expr8 = Neg(Complex (-3, -1))
let res8 = evaluate (expr8)
printfn "%s" (toString res8) // Complex (3, 1)

let expr9 = Add(Complex (3, 1), Complex (0, 0))
let res9 = evaluate (expr9)
printfn "%s" (toString res9) // Complex (3, 1)

let expr10 = Mul(Complex (3, 1), Complex (0, 0))
let res10 = evaluate (expr10)
printfn "%s" (toString res10) // Complex (0, 0)

let expr11 = Mul(Complex (3, 1), Complex (0, 1))
let res11 = evaluate (expr11)
printfn "%s" (toString res11) // Complex (-1, 3)

let expr12 = Neg(Neg(Complex (3, 1)))
let res12 = evaluate (expr12)
printfn "%s" (toString res12) // Complex (3, 1)

let expr13 = Neg(Mul(Complex (3, 0), Neg(Complex (2, 4))))
let res13 = evaluate (expr13)
printfn "%s" (toString res13) // Complex (6, 12)

let expr14 = (Sub(Mul(Complex (3,1), Complex (-2,5)),Add(Complex (-6,6), Complex(-5,7))))
let res14 = evaluate (expr14)
printfn "%s" (toString res14) // 0




// task c -- isConjugate method
let isConjugate expr1 expr2 =
    match (expr1, expr2) with
    | (Complex (r1, i1), Complex (r2, i2)) when r1 = r2 && i1 = -i2 -> Some (Complex (r1*r1+i1*i1, 0))
    | _ -> None


// test-cases 3
let res15 = isConjugate (Complex (3, 1)) (Complex (3, -1));;
printfn "val it: Expression option = %A" res15

let res16 = isConjugate (Complex (3, 1)) (Complex (3, -3));;
printfn "val it: Expression option = %A" res16
