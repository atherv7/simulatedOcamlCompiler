open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  let pVals tks = match tks with 
                  |(Tok_Int(i))::t -> let lst = (match_token tks (Tok_Int(i))) in (lst, Value(Int(i)))
                  |(Tok_Bool(b))::t -> let lst = (match_token tks (Tok_Bool(b))) in (lst, Value(Bool(b)))
                  |(Tok_String(s))::t -> let lst = (match_token tks (Tok_String(s))) in (lst, Value(String(s)))
                  |(Tok_ID(d))::t -> let lst = (match_token tks (Tok_ID(d))) in (lst, ID(d))
                  |Tok_LParen::t -> let lst = (match_token tks Tok_LParen) in let (list, ex) = (parse_expr lst) in 
                                                                              (match list with 
                                                                                |Tok_RParen::t -> (t, ex)
                                                                                |_ -> raise(InvalidInputException "Error"))
                  |_ -> raise(InvalidInputException "Error")
  in
  let pFunCall tks = let (ls, ex) = (pVals tks) in 
                      match lookahead ls with 
                        |Some Tok_Int(i) -> let (sls, exp) = (pVals ls) in (sls, FunctionCall(ex, exp))
                        |Some Tok_Bool(b) -> let (sls, exp) = (pVals ls) in (sls, FunctionCall(ex, exp))
                        |Some Tok_String(s) -> let (sls, exp) = (pVals ls) in (sls, FunctionCall(ex, exp))
                        |Some Tok_ID(d) -> let (sls, exp) = (pVals ls) in (sls, FunctionCall(ex, exp))
                        |Some Tok_LParen -> let (sls, exp) = (pVals ls) in (sls, FunctionCall(ex, exp))
                        |_ -> (ls, ex)                   
  in
  let rec pUni tks = match tks with 
                      |Tok_Not::t -> let ls = (match_token tks Tok_Not) in let (list, e) = (pUni ls) in (list, Not(e))
                      |_ -> (pFunCall tks)
  in 
  let rec pCon tks = let (ls, ex) = (pUni tks) in 
                      match ls with 
                        |Tok_Concat::t -> let lst = (match_token ls Tok_Concat) in 
                                          let (list, e) = (pCon lst) in (list, Binop(Concat, ex, e))
                        |_ -> (ls, ex)
  in
  let rec pRelation tks = let (ls, ex) = (pCon tks) in 
                          match ls with 
                            |Tok_Less::t -> let lst = (match_token ls Tok_Less) in 
                                            let (list, e) = (pRelation lst) in (list, Binop(Less, ex, e))
                            |Tok_Greater::t -> let lst = (match_token ls Tok_Greater) in 
                                               let (list, e) = (pRelation lst) in (list, Binop(Greater, ex, e))
                            |Tok_GreaterEqual::t -> let lst = (match_token ls Tok_GreaterEqual) in 
                                                    let (list, e) = (pRelation lst) in (list, Binop(GreaterEqual, ex, e))
                            |Tok_LessEqual::t -> let lst = (match_token ls Tok_LessEqual) in 
                                                 let (list, e) = (pRelation lst) in (list, Binop(LessEqual, ex, e))
                            |_ -> (ls, ex)
  in 
  let rec pEq tks = let (ls, ex) = (pRelation tks) in 
                    match ls with 
                      |Tok_Equal::t -> let lst = (match_token ls Tok_Equal) in 
                                       let (list, e) = (pEq lst) in (list, Binop(Equal, ex, e))
                      |Tok_NotEqual::t -> let lst = (match_token ls Tok_NotEqual) in 
                                          let (list, e) = (pEq lst) in (list, Binop(NotEqual, ex, e))
                      |_ -> (ls, ex)
  in
  let rec pMultDiv tks = let (ls, ex) = (pEq tks) in 
                        match ls with 
                          |Tok_Mult::t -> let lst = (match_token ls Tok_Mult) in 
                                          let (list, e) = (pMultDiv lst) in (list, Binop(Mult, ex, e))
                          |Tok_Div::t -> let lst = (match_token ls Tok_Div) in 
                                         let (list, e) = (pMultDiv lst) in (list, Binop(Div, ex, e))
                          |_ -> (ls, ex)
  in
  let rec pAddSub tks = let (ls, ex) = (pMultDiv tks) in 
                    match ls with 
                      |Tok_Add::t -> let lst = (match_token ls Tok_Add) in 
                                     let (list, e) = (pAddSub lst) in (list, Binop(Add, ex, e))
                      |Tok_Sub::t -> let lst = (match_token ls Tok_Sub) in 
                                     let (list, e) = (pAddSub lst) in (list, Binop(Sub, ex, e))
                      |_ -> (ls, ex)
  in  
  let rec pAnd tks = let (ls, ex) = (pAddSub tks) in 
                      match ls with 
                        |Tok_And::t -> let lst = (match_token ls Tok_And) in 
                                       let (list, e) = (pAnd lst) in (list, Binop(And, ex, e))
                        |_ -> (ls, ex)
  in 
  let rec pOr tks = let (ls, ex) = (pAnd tks) in 
                    match ls with 
                      |Tok_Or::t -> let lst = (match_token ls Tok_Or) in 
                                    let (list, e) = (pOr lst) in (list, Binop(Or, ex, e))
                      |_ -> (ls, ex) 
  in
  let pFun tks = match tks with 
                  |Tok_Fun::t -> (match lookahead t with 
                                  |Some Tok_ID(x) -> let lst = match_token (match_token t (Tok_ID(x))) Tok_Arrow in 
                                                     let (slst, ex) = parse_expr lst in 
                                                     (slst, Fun(x, ex))
                                  |_ -> raise(InvalidInputException "Error"))
                  |_ -> pOr tks
  in
  let pIf tks = match tks with 
                |Tok_If::t -> let (lst, cond) = parse_expr t in 
                              let slst = match_token lst Tok_Then in 
                              let (tlst, ifSt) = parse_expr slst in 
                              let stlst = match_token tlst Tok_Else in 
                              let (flst, elseSt) = parse_expr stlst in 
                              (flst, If(cond, ifSt, elseSt))
                |_ -> pFun tks 
  in
  let pDeclare tks = match tks with 
                      |Tok_Let::t -> (match (lookahead t) with 
                                      |Some Tok_Rec -> let lst = match_token t Tok_Rec 
                                                       in
                                                       (match lookahead lst with 
                                                        |Some Tok_ID(x) -> let lst = match_token (match_token lst (Tok_ID(x))) Tok_Equal in 
                                                                           let (sLst, fExp) = parse_expr lst in 
                                                                           let ssLst = match_token sLst Tok_In in 
                                                                           let (tLst, sExp) = parse_expr ssLst in 
                                                                           (tLst, Let(x, true, fExp, sExp))
                                                        |_ -> raise(InvalidInputException "Error"))
                                      |Some Tok_ID(y) -> let lst = match_token (match_token t (Tok_ID(y))) Tok_Equal in 
                                                         let (sLst, fExp) = parse_expr lst in 
                                                         let ssLst = match_token sLst Tok_In in
                                                         let (tLst, sExp) = parse_expr ssLst in 
                                                         (tLst, Let(y, false, fExp, sExp))                
                                      |_ -> raise(InvalidInputException "Error"))
                      |_ -> (pIf tks)
  in
  match toks with 
    |[Tok_DoubleSemi] -> raise(InvalidInputException "Error")
    |_ -> pDeclare toks
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let pEmu tks = let (lst, ex) = parse_expr tks in (lst, Expr(ex))
  in 
  let pDmu tks = match lookahead tks with 
                 |Some Tok_ID(d) -> let lst = match_token (match_token tks (Tok_ID(d))) Tok_Equal in 
                                    let (slst, ex) = parse_expr lst in 
                                    (slst, Def(d, ex))
                 |Some Tok_Rec -> raise(InvalidInputException "Error") 
                 |_ -> raise(InvalidInputException "Error")
  in
  let pMu tks = match tks with 
                |[Tok_DoubleSemi] -> ([], NoOp)
                |Tok_Def::t -> pDmu t 
                |_ -> pEmu tks 
  in 
  let (lst, exp) = pMu toks in 
  match lst with 
    |[Tok_DoubleSemi] -> ([], exp)
    |[] -> (match exp with 
            |NoOp -> (lst, exp)
            |_ -> raise(InvalidInputException "Error"))
    |_ -> raise(InvalidInputException "Error")