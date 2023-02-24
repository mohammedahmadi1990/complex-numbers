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



// tests 1
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
    | Neg expr1 -> Neg (evaluate expr1)
    | Add (expr1, expr2) ->
        let res1 = evaluate expr1
        let res2 = evaluate expr2
        match (res1, res2) with
        | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1+r2, i1+i2)
        | _ -> failwith "Invalid expression"
    | Sub (expr1, expr2) ->
        let res1 = evaluate expr1
        let res2 = evaluate expr2
        match (res1, res2) with
        | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1-r2, i1-i2)
        | _ -> failwith "Invalid expression"
    | Mul (expr1, expr2) ->
        let res1 = evaluate expr1
        let res2 = evaluate expr2
        match (res1, res2) with
        | (Complex (r1, i1), Complex (r2, i2)) -> Complex (r1*r2-i1*i2, r1*i2+i1*r2)
        | _ -> failwith "Invalid expression"





// tests 2
let expr4 = (Sub(Mul(Complex (3,1), Complex (-2,5)),Add(Complex (-6,6), Complex(-5,7))))
let res4 = evaluate (expr4)
printfn "%s" (toString res4) // (5 -8i)



// task c -- isConjugate method
let isConjugate expr1 expr2 =
    match (expr1, expr2) with
    | (Complex (r1, i1), Complex (r2, i2)) when r1 = r2 && i1 = -i2 -> Some (Complex (r1*r1+i1*i1, 0))
    | _ -> None

