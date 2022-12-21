open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
  |Value(Int(i)) -> Int(i)
  |Value(Bool(b)) -> Bool(b)
  |Value(String(s)) -> String(s)
  |Value(Closure(x,y,z)) -> Closure(x,y,z)
  |ID(x) -> lookup env x
  |Not(ex) -> (let b = (eval_expr env ex) in 
              match b with 
                |Bool(x) -> Bool(not x)
                |_ -> raise(TypeError("not a bool")))
  |Binop(Add, e1, e2) -> (let n = (eval_expr env e1) in 
                         let n2 = (eval_expr env e2) in 
                         match n with 
                          |Int(i) -> (match n2 with 
                                        |Int(i2) -> Int(i+i2)
                                        |_ -> raise(TypeError("not an integer")))
                          |_ -> raise(TypeError("not an integer")))
  |Binop(Sub, e1, e2) -> (let n = (eval_expr env e1) in 
                         let n2 = (eval_expr env e2) in 
                         match n with 
                          |Int(i) -> (match n2 with 
                                        |Int(i2) -> Int(i-i2)
                                        |_ -> raise(TypeError("not an integer")))
                          |_ -> raise(TypeError("not an integer")))
  |Binop(Mult, e1, e2) -> (let n = (eval_expr env e1) in 
                          let n2 = (eval_expr env e2) in 
                          match n with 
                            |Int(i) -> (match n2 with 
                                          |Int(i2) -> Int(i*i2)
                                          |_ -> raise(TypeError("not an integer")))
                            |_ -> raise(TypeError("not an integer")))
  |Binop(Div, e1, e2) -> (let n = (eval_expr env e1) in 
                          let n2 = (eval_expr env e2) in 
                          match n with 
                            |Int(i) -> (match n2 with 
                                          |Int(0) -> raise(DivByZeroError)
                                          |Int(i2) -> Int(i/i2)
                                          |_ -> raise(TypeError("not an integer")))
                            |_ -> raise(TypeError("not an integer")))
  |Binop(Greater, e1, e2) -> (let n = (eval_expr env e1) in 
                              let n2 = (eval_expr env e2) in 
                              match n with 
                                |Int(i) -> (match n2 with 
                                              |Int(i2) -> Bool(i > i2)
                                              |_ -> raise(TypeError("not an integer")))
                                |_ -> raise(TypeError("not an integer")))
  |Binop(Less, e1, e2) -> (let n = (eval_expr env e1) in 
                           let n2 = (eval_expr env e2) in 
                           match n with 
                            |Int(i) -> (match n2 with 
                                          |Int(i2) -> Bool(i < i2)
                                          |_ -> raise(TypeError("not an integer")))
                            |_ -> raise(TypeError("not an integer")))
  |Binop(GreaterEqual, e1, e2) -> (let n = (eval_expr env e1) in 
                                   let n2 = (eval_expr env e2) in 
                                   match n with 
                                    |Int(i) -> (match n2 with 
                                                  |Int(i2) -> Bool(i >= i2)
                                                  |_ -> raise(TypeError("not an integer")))
                                    |_ -> raise(TypeError("not an integer")))
  |Binop(LessEqual, e1, e2) -> (let n = (eval_expr env e1) in 
                                let n2 = (eval_expr env e2) in 
                                match n with 
                                  |Int(i) -> (match n2 with 
                                                |Int(i2) -> Bool(i <= i2)
                                                |_ -> raise(TypeError("not an integer")))
                                  |_ -> raise(TypeError("not an integer")))
  |Binop(Concat, e1, e2) -> (let s = (eval_expr env e1) in 
                             let s2 = (eval_expr env e2) in 
                             match s with 
                               |String(i) -> (match s2 with 
                                             |String(i2) -> String(i^i2)
                                             |_ -> raise(TypeError("not a string")))
                               |_ -> raise(TypeError("not a string")))
  |Binop(Equal, e1, e2) -> (let v = (eval_expr env e1) in 
                           let v2 = (eval_expr env e2) in 
                           match v with 
                            |Int(i) -> (match v2 with 
                                        |Int(i2) -> Bool(i = i2)
                                        |_ -> raise(TypeError("mismatched types")))
                            |String(s) -> (match v2 with 
                                            |String(s2) -> Bool(s = s2)
                                            |_ -> raise(TypeError("mismatched types")))
                            |Bool(b) -> (match v2 with 
                                          |Bool(b2) -> Bool(b = b2)
                                          |_ -> raise(TypeError("mismatched types")))
                            |_ -> raise(TypeError("mismatched types")))
  |Binop(NotEqual, e1, e2) -> (let v = (eval_expr env e1) in 
                              let v2 = (eval_expr env e2) in 
                              match v with 
                                |Int(i) -> (match v2 with 
                                            |Int(i2) -> Bool(i <> i2)
                                            |_ -> raise(TypeError("mismatched types")))
                                |String(s) -> (match v2 with 
                                                |String(s2) -> Bool(s <> s2)
                                                |_ -> raise(TypeError("mismatched types")))
                                |Bool(b) -> (match v2 with 
                                              |Bool(b2) -> Bool(b <> b2)
                                              |_ -> raise(TypeError("mismatched types")))
                                |_ -> raise(TypeError("mismatched types")))
  |Binop(Or, e1, e2) -> (let o = (eval_expr env e1) in 
                        let o2 = (eval_expr env e2) in 
                        match o with 
                          |Bool(b) -> (match o2 with 
                                        |Bool(b2) -> Bool(b || b2)
                                        |_ -> raise(TypeError("not a bool")))
                          |_ -> raise(TypeError("not a bool")))
  |Binop(And, e1, e2) -> (let o = (eval_expr env e1) in 
                        let o2 = (eval_expr env e2) in 
                        match o with 
                          |Bool(b) -> (match o2 with 
                                        |Bool(b2) -> Bool(b && b2)
                                        |_ -> raise(TypeError("not a bool")))
                          |_ -> raise(TypeError("not a bool")))
  |If(e1, e2, e3) -> (let b = (eval_expr env e1) in 
                     match b with 
                      |Bool(r) -> if r then (eval_expr env e2) else (eval_expr env e3)
                      |_ -> raise(TypeError("not a bool condition")))
  |Let(x, b, e1, e2) -> (if not b then 
                          let v = (eval_expr env e1) in 
                          let new_env = extend env x v in 
                          (eval_expr new_env e2)
                        else 
                          let n_env = extend_tmp env x in 
                          let v = (eval_expr n_env e1) in 
                          update n_env x v ; 
                          (eval_expr n_env e2))
  |Fun(v, ex) -> Closure(env, v, ex)
  |FunctionCall(e1, e2) -> let cl = (eval_expr env e1) in 
                           match cl with 
                            |Closure(e, x, b) -> let v = eval_expr env e2 in 
                                                 let n_e = extend e x v in 
                                                 (eval_expr n_e b)       
                            |_ -> raise(TypeError("not a function"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
    |Def(x, ex) -> let n_env = extend_tmp env x in 
                  let v = (eval_expr n_env ex) in 
                  update n_env x v ; 
                  (n_env, Some v)
    |Expr(ex) -> (env, Some(eval_expr env ex))
    |NoOp -> (env, None)