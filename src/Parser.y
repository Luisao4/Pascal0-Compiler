{

-- Gerador de analisador sintatico para Linguagem Pascal0
module Parser where
import Lexer  

}


%name parse
%tokentype { Token }
%error { parseError }

%token
-- var types
identity                                     { ID $$}
text                                         { TEXT_IN_QUOTES $$}
nume                                         { NUM $$}

--infix operators
"+"                                          { PLUS }
"-"                                          { MINUS }
"*"	                                         { MULT }
":="                                         { ASSIGN }
">"                                          { GREATER }
"<"                                          { LESS }
">="                                         { GREATE }
"<="                                         { LESSE }
"<>"                                         { DIFF }
"="                                          { EQUAL }
not                                          { NOT }
and                                          { AND }
or                                           { OR } 
div                                          { DIV }
mod                                          { MOD }

--PUNCT SIGNS
"("                                          { LPAREN }
")"                                          { RPAREN }
"["                                          { LSBRACE }
"]"                                          { RSBRACE }
";"                                          { SEMICOLON }
","                                          { COMMA }
"."                                          { POINT }
":"                                          { DPOINT }

--keywords
program                                      { PROGRAM}
function                                     { FUNCTION}
procedure                                    { PROCEDURE}
var                                          { VAR}
begin                                        { BEGIN}
end                                          { END}
if		                                     { IF }
then                                         { THEN}
else                                         { ELSE }
while                                        { WHILE }
do                                           { DO }
break                                        { BREAK }
for                                          { FOR }
to                                           { TO}
true                                         { TRUE }
false                                        { FALSE }
integer                                      { INTEGER}
boolean                                      { BOOLEAN}
string                                       { STRING}
of                                           { OF}
const                                        { CONST}
array                                        { ARRAY }


-- prioridades
%nonassoc "<>" "=" "<" ">" "<=" ">="     -- precedencia ainda mais baixa
%left "+" "-"     -- precedencia mais baixa 
%left "*" div
%left mod           
%left and or        
%left not           -- precedencia mais elevada

%%

-- Expessoes/ tipo de Expessoes

Prog : ProgHeader ProgBody "."          { Program $1 $2 }

ProgHeader   : program identity ";"   { Prog_Header $2 }

ProgBody : ConstDecls ProcDecls VarDecls CompoundStm    { Prog_Body $1 $2 $3 $4 }

ProcDecls   : ProcDefSeq        { $1 }
            | {-empty-}         { [] }

ProcDefSeq  :: { [Proc] }
            : Proc ProcDefSeq   { $1 : $2 }
            | Proc              { [$1] }

AcesstoVariable    : identity                { VariableAcess $1 }
                   | identity "[" Exp "]"   { ArrayPos $1 $3 }


Exp    : nume                          { Num $1 }
        -- Ops
        | Exp "+" Exp                 { Add $1 $3 }
        | Exp "-" Exp                 { Sub $1 $3 }
        | Exp "*" Exp                 { Mult $1 $3 }
        | Exp div Exp                 { Div $1 $3 }
        | Exp mod Exp                 { Mod $1 $3 }
        | text                        { TEXTinQUOTES $1 }         --TEXTinQUOTES
        | AcesstoVariable             { Var $1 }
        | "-" Exp                     { Negation $2 } -- Negative numbers
        -- Comparisons and assignments
        | Exp "=" Exp                 { Eq $1 $3 }
        | Exp "<>" Exp                { DIff $1 $3 }
        | Exp "<" Exp                 { Lthan $1 $3 }
        | Exp ">" Exp                 { Gthan $1 $3 }
        | Exp "<=" Exp                { Lequal $1 $3 }
        | Exp ">=" Exp                { Gequal $1 $3 }
        --boolean things
        | true                        { BoolVal True }
        | false                       { BoolVal False }
        | Exp and Exp                 { And $1 $3 }
        | Exp or Exp                  { Or $1 $3 }
        | not Exp                      { Not $2 }
        | "(" Exp ")"                  { $2 }   
        | identity   "(" ExpList ")"   { Func $1 $3 } 


ExpList:: {[Exp]} 
        : Exp ExpList1    { $1 : $2 }
        |                   { [] }  --nada -> vazio

ExpList1::{[Exp]} 
        : "," Exp ExpList1    { $2 : $3 }
        |                       { [] }  --nada -> vazio

--Statements

Stm : AcesstoVariable ":=" Exp                         { Assign $1 $3 } --assignments do tipo x=4
    | if Exp then Stm                                  { If $2 $4 } -- ifs sem elses    
    | if Exp then Stm else Stm                         { IfElse $2 $4 $6 } --ifs com elses 
    | while Exp do Stm                                 { While $2 $4 }  --ciclos while
    | for identity ":=" Exp to Exp do Stm              { For $2 $4 $6 $8 } -- ciclos for
    | identity "(" ExpList ")"                         { Procedure $1 $3 } 
    | break                                            { Break }   -- break
    | CompoundStm                                      { Compound $1 } --Multiplos statements

CompoundStm : begin StmList end                         { CPstm $2 }

StmList : Stm ";" StmList       { StatementList $1 $3 }
        | Stm                   { Stm $1 }


--Procedures and Functions

{- PDF
Proc : ProcHeader ProcBody ;
ProcHeader : procedure identity ( ParamList ) ;
           | function identity ( ParamList ) : BasicType ;
ProcBody : VarDecls CompoundStm

ParamList : ParamList1
          | ε
ParamList1 : Param ; ParamList1
           | Param
Param : identity : Type
-}

Proc: ProcHeader ProcBody ";"   { ProcedureDef $1 $2 }

ProcHeader  : procedure identity   "(" ParamList ")" ";"            { ProcedureHeader $2 $4 }
            | function identity   "(" ParamList ")" ":" BasicType ";"    { ProcedureHeaderRet $2 $4 $7 }

ProcBody: VarDecls CompoundStm  { ProcedureBody $1 $2 }


ConstDecls  ::                    { [ConstantDef] }
            : const ConstDefSeq   { $2 }
            |                     { [] }  --nada -> vazio

ConstantDef: identity   "=" nume ";" { ConstDef $1 $3 }

{-          PDF
ConstDecls : const ConstDefSeq
           | ε

VarDecls : var VarDefSeq
         | ε

    -> Copia 3.1Declarations         
-}

ConstDefSeq::                           { [ConstantDef] }
        : ConstantDef ConstDefSeq       { $1 : $2 }
        | ConstantDef                   { [$1] }

VarDecls  ::                  { [VariableDef] }
        : var VarDefSeq       { $2 }
        |                     { [] }  --nada -> vazio

VariableDef: identity   ":" Type ";"    { VarDef $1 $3 }

VarDefSeq::                         { [VariableDef] }
        : VariableDef VarDefSeq     { $1 : $2 }
        | VariableDef               { [$1] }

Param   : identity   ":" Type   { Parameter $1 $3 }

ParamList   :: { [Param] }
            : Param ParamList1      { $1 : $2 }
            |                       { [] }  --nada -> vazio

ParamList1  :: { [Param] }
            : ";" Param ParamList1  { $2 : $3 }
            |                       { [] }  --nada -> vazio

Type    : BasicType { BType $1 }
        | ArrayType { AType $1 }

ArrayType: array "[" Constant "." "." Constant "]" of BasicType { Array $9 $3 $6 }

BasicType: integer  { Integer }
        | boolean   { Boolean }
        | string    { String }


Constant: nume   { IntConst $1 }
        | identity  { IdConst $1 }

{

data Exp = Num Int    --Data table for Exp.
    | Text String
    | TEXTinQUOTES String
    | Var AcesstoVariable
    | Func String [Exp]
    | Not Exp
    | Add Exp Exp
    | Sub Exp Exp
    | Mult Exp Exp
    | Div Exp Exp
    | Mod Exp Exp
    | Negation Exp
    | Eq Exp Exp        -- =
    | DIff Exp Exp      -- <>
    | Lthan Exp Exp     -- <
    | Lequal Exp Exp    -- <=
    | Gthan Exp Exp     -- >
    | Gequal Exp Exp    -- >=
    | BoolVal Bool
    | And Exp Exp
    | Or Exp Exp
    deriving Show

data AcesstoVariable = VariableAcess String
    | ArrayPos String Exp
    deriving Show

data Stm = Assign AcesstoVariable Exp -- atribui um valor
         | If Exp Stm   -- if sem else
         | IfElse Exp Stm Stm   --if com else
         | While Exp Stm    --while
         | For String Exp Exp Stm   --ciclo for 
         | Procedure String [Exp]
         | Break
         | Compound CompoundStm -- multiplos stm
         deriving Show


data CompoundStm = CPstm StmList
    deriving Show   

data ProcStm = Proc String [Exp]
    deriving Show


data StmList = StatementList Stm StmList
            | Stm Stm
    deriving Show

--DATA TYPE FOR Programs

data Prog = Program ProgHeader ProgBody
    deriving Show

data ProgHeader = Prog_Header String
    deriving Show

data ProgBody = Prog_Body [ConstantDef] [Proc] [VariableDef] CompoundStm
    deriving Show


-- DATA TYPES FOR Procedures
data ArrayType = Array BasicType Constant Constant
    deriving Show

data BasicType = Integer
            | Boolean
            | String
    deriving Show

data Constant = IntConst Int
            | IdConst String
    deriving Show

data VariableDef = VarDef String Type
    deriving Show

data ConstantDef = ConstDef String Int
    deriving Show

data Param = Parameter String Type
    deriving Show

data Type   = BType BasicType
            | AType ArrayType
    deriving Show

data Proc   = ProcedureDef ProcHeader ProcBody
    deriving Show

data ProcHeader = ProcedureHeader String [Param]
                | ProcedureHeaderRet String [Param] BasicType
    deriving Show

data ProcBody   = ProcedureBody [VariableDef] CompoundStm
    deriving Show

parseError :: [Token] -> a
parseError toks = error "parse error"  

}