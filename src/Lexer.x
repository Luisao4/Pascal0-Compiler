{
-- Gerador de analise lexical para linguagem pascal0
module Lexer where
}

%wrapper "basic"

$white = [\ \t\n\r\b\v\f\a\\\'\"]                                             -- caracteres brancos
$alpha = [_A-Za-z]                                                            -- letras da string/Identificador
$digit = [0-9]                                                                -- numeros

tokens :-

-- INFIX OPERATORS
"+"                                          { \_ -> PLUS }
"-"                                          { \_ -> MINUS }
"*"	                                         { \_ -> MULT }
":="                                         { \_ -> ASSIGN }
">"                                          { \_ -> GREATER }
"<"                                          { \_ -> LESS }
">="                                         { \_ -> GREATE }
"<="                                         { \_ -> LESSE }
"<>"                                         { \_ -> DIFF }
"="                                          { \_ -> EQUAL }
not                                          { \_ -> NOT }
and                                          { \_ -> AND}
or                                           { \_ -> OR } 
div                                          { \_ -> DIV}
mod                                          { \_ -> MOD}

--PUNCTUATION SIGNS

"("                                          { \_ -> LPAREN }
")"                                          { \_ -> RPAREN }
"["                                          { \_ -> LSBRACE }
"]"                                          { \_ -> RSBRACE }
";"                                          { \_ -> SEMICOLON }
","                                          { \_ -> COMMA }
"."                                          { \_ -> POINT}
":"                                          { \_ -> DPOINT}




--keywords
program                                      {\_ -> PROGRAM}
function                                     {\_ -> FUNCTION}
procedure                                    {\_ -> PROCEDURE}
var                                          {\_ -> VAR}
begin                                        {\_ -> BEGIN}
end                                          {\_ -> END}
if		                                     {\_ -> IF }
then                                         {\_ -> THEN}
else                                         { \_ -> ELSE }
while                                        { \_ -> WHILE }
do                                           { \_ -> DO }
break                                        { \_ -> BREAK }
for                                          { \_ -> FOR }
to                                           {\_ -> TO}
true                                         { \_ -> TRUE }
false                                        { \_ -> FALSE }
integer                                      { \_ -> INTEGER}
boolean                                      { \_ -> BOOLEAN}
string                                       { \_ -> STRING}
array                                        { \_ -> ARRAY}
of                                           { \_ -> OF}
const                                        {\_ -> CONST}

--          (might be wrong, either way) 
--  $digit+"."$digit+                            { \s -> INTEGER (read s)} --nºs float
--  "-"+$digit+                                  { \s -> INTEGER (read s)} --nºs int negativos
--  "-"+$digit+"."$digit+                        { \s -> INTEGER (read s)} -- nºs float negativos


\'[^\']*\'                                   {\(xs:x) -> TEXT_IN_QUOTES (init x)}     --  Simple String (in between ' ')
$alpha(($alpha|$digit))*	                 { \x -> ID x }                    -- letrascomnumeros 1 ou mais vezes, _ -> ja esta incluido no alpha
$digit+                                      { \x -> NUM (read x) }            -- ints
$white+                                                 ;                      -- ignorar caracteres brancos

\(\*(.\n?)*\*\)|\(\*(\n?.)*\*\)                         ;                      -- ignorar comentarios


{
data Token = ID String  
          | NUM Int 
          | TEXT_IN_QUOTES String
          | POINT 
          | DPOINT
          | OF 
          | ARRAY 
          | PROGRAM 
          | FUNCTION 
          | PROCEDURE 
          | VAR 
          | BEGIN 
          | END 
          | DO 
          | TO 
          | INTEGER 
          | BOOLEAN 
          | STRING 
          | GREATER -- >
          | LESS   -- <
          | GREATE   -- >=
          | LESSE  -- <=
          | DIFF 
          | EQUAL 
          | ASSIGN 
          | IF 
          | THEN 
          | ELSE 
          | WHILE 
          | PLUS 
          | DIV 
          | MINUS 
          | MOD 
          | MULT 
          | SEMICOLON 
          | COMMA 
          | RPAREN 
          | LPAREN 
          | RBRACK 
          | LBRACK 
          | RSBRACE 
          | LSBRACE 
          | NOT 
          | AND 
          | OR 
          | BREAK 
          | FOR 
          | TRUE 
          | FALSE
          | CONST
          deriving (Eq, Show)
}