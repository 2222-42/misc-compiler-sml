structure Token =
struct
    datatype token
        = EOF | UNDERBAR | ID of string | STRING of string
          | REAL of string | SPECIAL of string
          | INT of int

    fun toString token =
        case token
         of EOF => "EOF"
          | UNDERBAR => "UNDERBAR "
          | ID s => "ID " ^ s
          | STRING s => "STRING " ^ "\"" ^s ^ "\""
          | REAL s => "REAL " ^ s
          | SPECIAL s =>  "SPECIAL " ^ s
          | INT i => "INT " ^ Int.toString(i)
end
