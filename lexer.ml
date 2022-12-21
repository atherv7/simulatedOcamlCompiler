open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
    let rec tok str pos = 
        if pos >= String.length str then 
            []
        else if Str.string_match (Str.regexp("[ \t\n]+")) str pos then 
            tok str (pos+1) 
        else if Str.string_match (Str.regexp("true\\|false")) str pos then 
            let s = Str.matched_string str in 
            (Tok_Bool(bool_of_string s))::(tok str (pos + String.length s))
        else if Str.string_match (Str.regexp {|\"\([^\"]*\)\"|}) str pos then 
            let s = Str.matched_string str in 
            let acc = String.sub s 1 ((String.length s)-2) in 
            (Tok_String(acc))::(tok str (pos + String.length s))
        else if Str.string_match (Str.regexp("[0-9]+\\|(\\(-[0-9]+\\))")) str pos then 
            let s = try Str.matched_group 1 str with Not_found -> Str.matched_string str in 
            let add = String.length(Str.matched_string str) in 
            (Tok_Int(int_of_string(s)))::(tok str (pos + add))
        else if Str.string_match (Str.regexp("(")) str pos then 
            (Tok_LParen)::(tok str (pos+1))
        else if Str.string_match (Str.regexp(")")) str pos then 
            (Tok_RParen)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("=")) str pos then 
            (Tok_Equal)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("<>")) str pos then 
            (Tok_NotEqual)::(tok str (pos+2))
        else if Str.string_match (Str.regexp(">")) str pos then 
            (Tok_Greater)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("<")) str pos then 
            (Tok_Less)::(tok str (pos+1))
        else if Str.string_match (Str.regexp(">=")) str pos then 
            (Tok_GreaterEqual)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("<=")) str pos then 
            (Tok_LessEqual)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("||")) str pos then 
            (Tok_Or)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("&&")) str pos then 
            (Tok_And)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("not")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+3) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Not)::(tok str (pos+3))
        else if Str.string_match (Str.regexp("if")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+2) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_If)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("then")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+4) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Then)::(tok str (pos+4))
        else if Str.string_match (Str.regexp("->")) str pos then 
            (Tok_Arrow)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("else")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+4) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Else)::(tok str (pos+4))
        else if Str.string_match (Str.regexp("+")) str pos then 
            (Tok_Add)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("-")) str pos then 
            (Tok_Sub)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("*")) str pos then 
            (Tok_Mult)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("/")) str pos then 
            (Tok_Div)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("\\^")) str pos then 
            (Tok_Concat)::(tok str (pos+1))
        else if Str.string_match (Str.regexp("let")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+3) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Let)::(tok str (pos+3))
        else if Str.string_match (Str.regexp("def")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+3) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Def)::(tok str (pos+3))
        else if Str.string_match (Str.regexp("in")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+2) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_In)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("rec")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+3) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Rec)::(tok str (pos+3))
        else if Str.string_match (Str.regexp("fun")) str pos then 
            let matched = Str.matched_string str in 
            if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str (pos+3) then 
                (Tok_ID(matched^(Str.matched_string str)))::(tok str (Str.match_end()))
            else 
                (Tok_Fun)::(tok str (pos+3))
        else if Str.string_match (Str.regexp(";;")) str pos then 
            (Tok_DoubleSemi)::(tok str (pos+2))
        else if Str.string_match (Str.regexp("[a-zA-Z][a-zA-Z0-9]*")) str pos then 
            let s = Str.matched_string str in 
            (Tok_ID(s))::(tok str (pos + String.length s)) 
        else failwith("no Character")
    in 
    tok input 0 

