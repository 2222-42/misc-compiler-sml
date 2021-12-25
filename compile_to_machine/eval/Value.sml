structure Value =
struct
    datatype value
        = INT of int | BOOL of bool | STRING of string
        | PAIR of value * value
        | CLS of env * string * Syntax.exp
        | REC of env * string * string * Syntax.exp
    withtype env = value SEnv.map
    val emptyEnv = SEnv.empty
    fun toString value =
        case value
         of INT int => Int.toString int
          | BOOL bool => Bool.toString bool
          | STRING str => "\"" ^ str ^ "\""
          | PAIR (v1, v2) => "(" ^ toString v1 ^ "," ^ toString v2 ^ ")"
          | CLS (env, str, exp) => "fn" (*^ str ^ " => " ^ toString exp*)
          | REC (env, str1, str2, exp) => "fix" (* ^   *)
end
