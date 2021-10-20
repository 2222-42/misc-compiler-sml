structure Token =
struct
    datatype token
        = EOF | UNDERBAR | ANDALSO | AND | AS | CASE
          | DO | END | EXCEPTION | FN | FUN | HANDLE | IF
          | IN | INFIX | INFIXR | NONFIX | LET | LOCAL
          | OF | OP | OPEN | ORELSE | RAISE | REC
          | THEN | USE | VAL | WHILE
          | COMMA | PERIOD | THREEPOINT | COLON | SEMICOLON
          | EQUALSYM | ARROW | LBRACKET | RBRACKET | BAR
          | ID of string | STRING of string
          | REAL of string | SPECIAL of string
          | INT of string

    fun toString token =
        case token
         of EOF => "EOF"
          | UNDERBAR => "UNDERBAR "
          | ANDALSO => "ANDALSO "
          | AND => "AND "
          | AS => "AS "
          | CASE => "CASE "
          | DO => "DO "
          | END => "END "
          | EXCEPTION => "EXCEPTION "
          | FN => "FN "
          | FUN => "FUN "
          | HANDLE => "HANDLE "
          | IF => "IF "
          | IN => "IN "
          | INFIX => "INFIX "
          | INFIXR => "INFIXR "
          | NONFIX => "NONFIX "
          | LET => "LET "
          | LOCAL => "LOCAL "
          | OF => "OF "
          | OP => "OP "
          | OPEN => "OPEN "
          | ORELSE => "ORELSE "
          | RAISE => "RAISE "
          | REC => "REC "
          | THEN => "THEN "
          | USE => "USE "
          | VAL => "VAL "
          | WHILE => "WHILE "
          | COMMA => "COMMA "
          | PERIOD => "PERIOD "
          | THREEPOINT => "THREEPOINT "
          | COLON => "COLON "
          | SEMICOLON => "SEMICOLON "
          | EQUALSYM => "EQUALSYM "
          | ARROW => "ARROW "
          | LBRACKET => "LBRACKET "
          | RBRACKET => "RBRACKET "
          | BAR => "BAR "
          | ID s => "ID " ^ s
          | STRING s => "STRING " ^ "\"" ^s ^ "\""
          | REAL s => "REAL " ^ s
          | SPECIAL s =>  "SPECIAL " ^ s
          | INT s => "INT " ^ s
end
