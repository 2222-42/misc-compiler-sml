type lexresult = Token.token
val eof = fn () => Token.EOF
fun atoi s = valOf (Int.fromString s)
%%
%structure CoreMLLex
alpha = [A-Za-z];
digit = [0-9];
id = {alpha}({alpha}|{digit})*;
num = {digit}+;
frac = "."{num};
exp = [eE](~?){num};
real = (~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
ws = "\ " | "\t" | "\r\n" | "\n" | "\r";
%%
\"[^"]*\" => (Token.STRING
              (String.substring
                  (yytext,1,String.size yytext - 2)));
"_" => (Token.UNDERBAR);
"andalso" => (Token.ANDALSO);
"and" => (Token.AND);
"as" => (Token.AS);
"case" => (Token.CASE);
"do" => (Token.DO);
"end" => (Token.END);
"exception" => (Token.EXCEPTION);
"fn" => (Token.FN);
"fun" => (Token.FUN);
"handle" => (Token.HANDLE);
"if" => (Token.IF);
"in" => (Token.IN);
"infix" => (Token.INFIX);
"infixr" => (Token.INFIXR);
"nonfix" => (Token.NONFIX);
"let" => (Token.LET);
"local" => (Token.LOCAL);
"of" => (Token.OF);
"op" => (Token.OP);
"opne" => (Token.OPEN);
"oresle" => (Token.ORELSE);
"raise" => (Token.RAISE);
"rec" => (Token.REC);
"then" => (Token.THEN);
"use" => (Token.USE);
"val" => (Token.VAL);
"while" => (Token.WHILE);
"," => (Token.COMMA);
"..." => (Token.THREEPOINT);
"." => (Token.PERIOD);
":" => (Token.COLON);
";" => (Token.SEMICOLON);
"=>" => (Token.ARROW);
"=" => (Token.EQUALSYM);
"[" => (Token.LBRACKET);
"]" => (Token.RBRACKET);
"|" => (Token.BAR);
{id} => (Token.ID yytext);
{num} => (Token.INT yytext);
{real} => (Token.REAL yytext);
{ws} => (lex());
. => (Token.SPECIAL yytext);
