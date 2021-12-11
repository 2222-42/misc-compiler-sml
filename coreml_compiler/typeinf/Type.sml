structure Type =
struct
    datatype ty = TYVARty of string | INTty | STRINGty | BOOLty
                  | FUNty of ty * ty | PAIRty of ty * ty
                  | POLYty of string list * ty
    val seed = ref 0
    fun gensym () =
        ("$" ^ Int.toString(!seed) before seed := !seed + 1)
    fun newTy () =
        TYVARty (gensym ())
    fun tyToString ty =
        case ty
         of TYVARty string => "'" ^ string
          | INTty => "int"
          | STRINGty => "string"
          | BOOLty => "bool"
          | FUNty (t1, t2) => "(" ^ tyToString t1 ^ " -> " ^ tyToString t2 ^")"
          | PAIRty (t1, t2) => "(" ^ tyToString t1 ^ ", " ^ tyToString t2 ^ ")"
          | POLYty (tids, ty) => "[" ^
                                String.concatWith "," tids ^ "." ^
                                tyToString ty ^
                                "]"
end
