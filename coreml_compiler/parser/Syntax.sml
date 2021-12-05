structure Syntax =
struct
    datatype prim = EQ | ADD | SUB | MUL | DIV
    datatype exp =
             EXPID of string
             | INT of int
             | STRING of string
             | TRUE
             | FALSE
             | EXPFN of string * exp
             | EXPAPP of exp * exp
             | EXPPAIR of exp * exp
             | EXPPROJ1 of exp
             | EXPPROJ2 of exp
             | EXPPRIM of prim * exp * exp
             | EXPIF of exp * exp * exp
    and dec =
        VAL of string * exp
        | FUN of string * string * exp
    fun expToString exp =
        case exp
         of EXPID string => string
          | INT int => Int.toString int
          | STRING string => "\"" ^ string ^ "\""
          | TRUE => "true"
          | FALSE => "false"
          | EXPFN (string, exp) => "(fn " ^ string ^ " => "^ expToString exp ^")"
          | EXPAPP (exp1, exp2) => "("^ expToString exp1 ^" " ^ expToString exp2 ^")"
          | EXPPAIR (exp1, exp2) => "("^ expToString exp1 ^", " ^ expToString exp2 ^")"
          | EXPPROJ1 exp => "#1 " ^ expToString exp
          | EXPPROJ2 exp => "#2 " ^ expToString exp
          | EXPPRIM (p, exp1, exp2) =>
            let
                val prim = case p of ADD => "add"
                                   | SUB => "sub"
                                   | MUL => "mul"
                                   | DIV => "div"
                                   | EQ => "eq"
            in
                "prim(" ^ prim ^ ", " ^ expToString exp1 ^ ", " ^ expToString exp2 ^")"
            end

          | EXPIF (exp1, exp2, exp3) =>
            "if " ^ expToString exp1  ^ " then " ^ expToString exp2 ^ " else " ^  expToString exp3

    fun decToString dec =
        case dec of
            VAL (x, exp) => "val " ^ x ^ " = " ^ expToString exp
         | FUN (str1, str2, exp) => "fun " ^ str1 ^ "(" ^ str2 ^ ") = " ^ expToString exp
end