structure Tokens = CoreMLLrVals.Tokens
type token = Tokens.token
type pos = Tokens.pos
type lexresult = Tokens.token
exception Error
val eof = fn () => Tokens.EOF (0,0)
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

"val" => (Tokens.VAL (yypos, yypos + 3));
"true" => (Tokens.TRUE (yypos, yypos + 4));
"then" => (Tokens.THEN (yypos, yypos + 4));
"sub" => (Tokens.SUB (yypos, yypos + 3));
\"[^"]*\" => (Tokens.STRING
                (String.substring(yytext,1,String.size yytext - 2),
                yypos - String.size yytext + 1,
                yypos + 1));
";" => (Tokens.SEMICOLON (yypos, yypos + 1));
")" => (Tokens.RPAREN (yypos, yypos + 1));
"mul" => (Tokens.MUL (yypos, yypos + 3));
"(" => (Tokens.LPAREN (yypos, yypos + 1));
{num} => (Tokens.INT (atoi yytext, yypos, yypos + String.size yytext));
~{num} => (Tokens.INT (atoi yytext, yypos, yypos + String.size yytext));
"if" => (Tokens.IF (yypos, yypos + 2));
{id} => (Tokens.ID (yytext, yypos, yypos + String.size yytext));
"#2" => (Tokens.HASH2 (yypos,yypos+2));
"#1" => (Tokens.HASH1 (yypos,yypos+2));
"fun" => (Tokens.FUN (yypos, yypos + 3));
"fn" => (Tokens.FN (yypos, yypos + 2));
"false" => (Tokens.TRUE (yypos, yypos + 5));
"=" => (Tokens.EQUAL (yypos, yypos + 1));
"eq" => (Tokens.EQ (yypos, yypos + 2));

"else" => (Tokens.ELSE (yypos, yypos + 4));
"div" => (Tokens.DIV (yypos, yypos + 3));
"=>" => (Tokens.DARROW (yypos, yypos + 2));
"," => (Tokens.COMMA (yypos, yypos + 1));
"add" => (Tokens.ADD (yypos, yypos + 3));
{ws} => (lex());
. => (Tokens.ID (yytext, yypos, yypos + 1));
