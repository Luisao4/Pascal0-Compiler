module AST where

import Parser
import Data.String
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

intermediate :: Prog -> [Instr]
intermediate prog = evalState (transProg prog) (0,0)


-- Tipos de dados usados

data Instr = MOVE Temp Temp -- temp1 := temp2
    | MOVEI Temp Int -- temp1 := num
    | MOVES Temp String
    | LABEL Label
    | OP BinOp Temp Temp Temp -- temp1 := temp2 op temp3
    | OPI BinOp Temp Temp Int  -- temp1 := temp2 op num
    | JUMP Label
    | COND Temp RelOp Temp Label Label
    | RSARRAY Temp Temp Temp --RS = Right Side (temp := M[Atom]) 
    | LSARRAY Temp Temp Temp --LS = Left Side (M[Atom] := temp)
    | CALL String [Temp]        -- Call sem return
    | CALLR String [Temp] Temp  -- Call com return
    | BREAK
    deriving Show

data BinOp = IPlus | ISub | IMult | IDiv | IMod --dados para + - * div mod
    deriving Show
    
data RelOp = Lesst | Greatert | Greaterteq | Lesseq | IEq | INeq -- dados para < > >= <= = <>
    deriving Show

type Label = String
type Temp = String
type Count = (Int, Int)
type Table = [(String, Temp)]

-- Monads usados 

-- create a new temporary variable
newTemp :: State Count Temp
newTemp = do (temps,labels) <- get
             put (temps+1,labels)
             return ("t"++show temps)

-- create a new label
newLabel :: State Count Label
newLabel = do (temps,labels) <- get
              put (temps,labels+1)
              return ("L"++show labels)

-- reuse temporary variables
popTemp :: Int -> State Count ()
popTemp k = do (temps,labels) <- get
               put (temps-k,labels)

getTemp :: AcesstoVariable -> Table -> String -> Temp
getTemp s table errMsg = case (lookup (getStringFromVar s) table) of
      Just temp -> temp
      Nothing -> error errMsg

-- Trans do Exp
transExp :: Exp -> Table -> Temp -> State Count [Instr]
transExp (Num n) table dest = return [MOVEI dest n] --numero
transExp (Text txt) table dest = return [MOVES dest txt]    -- texto    
transExp (Var s) table dest = do    --variavel
    code <- transExprVar s table dest
    return code

transExp (Sub e1 e2) table dest = do -- aritmetica subtraçao
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp e1 table t1
    code2 <- transExp e2 table t2
    popTemp 2
    return (code1 ++ code2 ++ [OP ISub dest t1 t2])

transExp (Add e1 e2) table dest = do -- aritmetica adiçao
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp e1 table t1
    code2 <- transExp e2 table t2
    popTemp 2
    return (code1 ++ code2 ++ [OP IPlus dest t1 t2])

transExp (Mult e1 e2) table dest = do -- aritmetica multiplicaçao
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp e1 table t1
    code2 <- transExp e2 table t2
    popTemp 2
    return (code1 ++ code2 ++ [OP IMult dest t1 t2])

transExp (Mod e1 e2) table dest = do -- aritmetica
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp e1 table t1
    code2 <- transExp e2 table t2
    popTemp 2
    return (code1 ++ code2 ++ [OP IMod dest t1 t2]) --obter resto

transExp (Div e1 e2) table dest = do -- aritmetica divisao 
    t1 <- newTemp   
    t2 <- newTemp
    code1 <- transExp e1 table t1
    code2 <- transExp e2 table t2
    popTemp 2
    return (code1 ++ code2 ++ [OP IDiv dest t1 t2]) --obter quociente

transExp (Func name args) table dest = do
    (code, temps) <- transArgs args table
    popTemp (length args)
    return (code ++ [CALLR name temps dest]) -- CALL com Return

transExp (BoolVal False) table dest = do -- valor falso
    return [MOVEI dest 0]

transExp (BoolVal True) table dest = do -- valor true
    return [MOVEI dest 1]

transExp (Lthan e1 e2) table dest = do -- comparaçao <
    label1 <- newLabel -- if true, comparaçao for verdade continua label1
    label2 <- newLabel  -- if false, comparaçao vai para label2
    label3 <- newLabel  -- label3 é usada para continuaçao depois da comparaçao
    code1 <- transCond table (Lthan e1 e2) label1 label2 -- apenas faz um codigo
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (Gthan e1 e2) table dest = do --Comparaçao >
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (Gthan e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (Lequal e1 e2) table dest = do -- comparaçao <=
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (Lequal e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (Gequal e1 e2) table dest = do --Comparaçao >=
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (Gequal e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (DIff e1 e2) table dest = do -- comparaçao <>
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (DIff e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (Eq e1 e2) table dest = do --Comparaçao de igualdade =
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (Eq e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

{- Aqui se o da esquerda/ o que for avaliado primeiro for falso ele nem chegue a correr / avaliar a segunda parte
por causa da "lazy evaluation"-}

transExp (And e1 e2) table dest = do -- Logica &&
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (And e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

{- Aqui o principio é o mesmo so que se for Verdadeiro nao chega a avaliar a segunda parte avança logo-}

transExp (Or e1 e2) table dest = do -- Operador Logico ||
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (Or e1 e2) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (Not exp) table dest = do -- operador logico de negaçao !
    label1 <- newLabel
    label2 <- newLabel
    label3 <- newLabel 
    code1 <- transCond table (Not exp) label1 label2
    return (code1 ++ [LABEL label1] ++ [MOVEI dest 1] ++ [JUMP label3] ++ [LABEL label2] ++ [MOVEI dest 0] ++ [LABEL label3])

transExp (Negation exp) table dest = do -- Aqui negaçao pode ser por exemplo a negaçao de um numero
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp (Num 0) table t1 
    code2 <- transExp exp table t2
    popTemp 2
    return (code1 ++ code2 ++ [OP ISub dest t1 t2])

transArgs args table = worker args
  where
    worker []  = return ([], [])
    worker (x:y) = do 
            temp <- newTemp
            code <- transExp x table temp 
            (code', temps') <- worker y 
            return (code ++ code', temp:temps')

-- Arrays !!

getStringFromVar :: AcesstoVariable -> String
getStringFromVar (VariableAcess x) = x
getStringFromVar (ArrayPos a b) = a

transIndex (ArrayPos s index) table = auxIndex (ArrayPos s index)
    where
        auxIndex (ArrayPos s index) = do
            addr <- newTemp
            number4 <- newTemp
            code1 <- transExp index table addr
            code2 <- transExp (Num 4) table number4
            return (code1 ++ code2 ++ [OP IMult addr addr number4] ++ [OP IPlus addr addr base], addr)
                where base = getTemp (ArrayPos s index) table ("undefined array")

transExprVar :: AcesstoVariable -> Table -> Temp -> State Count [Instr]
transExprVar (VariableAcess s) table dest = do
            return [MOVE dest (getTemp (VariableAcess s) table "invalid variable")]
transExprVar (ArrayPos s index) table dest = do
            (code1, addr) <- transIndex (ArrayPos s index) table
            return (code1 ++ [RSARRAY dest s addr])

transStmVar :: Table -> AcesstoVariable -> Temp -> State Count [Instr]
transStmVar table (VariableAcess s) value = do
            return [MOVE (getTemp (VariableAcess s) table "undefined variable") value]
transStmVar table (ArrayPos s index) value = do
            (code1, addr) <- transIndex (ArrayPos s index) table
            return (code1 ++ [LSARRAY s addr value])

-- Trans de Statements

transStm :: Table -> Stm -> State Count [Instr]
transStm table (Assign var expr) = do -- x = 5 por exemplo
            value <- newTemp
            code1 <- transExp expr table value
            code2 <- transStmVar table var value
            return (code1 ++ code2)

transStm table (If cond stm) = do -- IF SEM ELSE!
            ltrue <- newLabel
            lfalse <- newLabel
            code0 <- transCond table cond ltrue lfalse
            code1 <- transStm table stm
            return (code0 ++ [LABEL ltrue] ++ code1 ++ [LABEL lfalse])

transStm table (IfElse cond stm1 stm2) = do -- IF COM ELSE
            ltrue <- newLabel
            lfalse <- newLabel
            lend <- newLabel
            code0 <- transCond table cond ltrue lfalse
            code1 <- transStm table stm1
            code2 <- transStm table stm2
            return (code0 ++ [LABEL ltrue] ++ code1 ++ [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transStm table (While cond stm) = do -- Ciclo while -> bem explicado no video
            lcond <- newLabel
            lstm <- newLabel
            lend <- newLabel
            code0 <- transCond table cond lstm lend
            code1 <- transStm table stm
            return ([LABEL lcond] ++ code0 ++ [LABEL lstm] ++ code1 ++ [JUMP lcond, LABEL lend])

transStm table (For i beg end stm) = do -- Ciclo for
            t1 <- newTemp
            table1 <- insertTempIntoTable table t1 i
            lcond <- newLabel
            lstm <- newLabel
            lend <- newLabel
            code0 <- transCond table1 (Lthan beg end) lstm lend
            code1 <- transStm table1 stm
            code2 <- transExp (Add (Var (VariableAcess i)) (Num 1)) table1 t1
            popTemp 1
            return ([LABEL lcond] ++ code0 ++ [LABEL lstm] ++ code1 ++ code2 ++ [JUMP lcond, LABEL lend])

transStm table (Procedure procName args) = do
            (code, temps) <- transArgs args table
            popTemp (length args)
            return (code ++ [CALL procName temps])

transStm table (Compound (CPstm stmList)) = transStmList table stmList -- Composta -> bem explicado no video

transStm table Break = return [BREAK] -- COMO E QUE ME ESQUECI ???

transStmList :: Table -> StmList -> State Count [Instr]
transStmList table (Stm stm) = transStm table stm
transStmList table (StatementList stm stmList) = do
            code0 <- (transStm table stm)
            code1 <- (transStmList table stmList)
            return (code0 ++ code1)

-- Trans para Conditions

transCond :: Table -> Exp -> Label -> Label -> State Count [Instr]
transCond table (Lthan e1 e2) ltrue lfalse = do -- LESS THAN
        temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp e1 table temp1
        code2 <- transExp e2 table temp2
        popTemp 2
        return (code1 ++ code2 ++ [COND temp1 Lesst temp2 ltrue lfalse])

transCond table (Gthan e1 e2) ltrue lfalse = do -- GREATER THAN
        temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp e1 table temp1
        code2 <- transExp e2 table temp2
        popTemp 2
        return (code1 ++ code2 ++ [COND temp1 Greatert temp2 ltrue lfalse])

transCond table (Lequal e1 e2) ltrue lfalse = do -- LESS OR EQUAL THAN
        temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp e1 table temp1
        code2 <- transExp e2 table temp2
        popTemp 2
        return (code1 ++ code2 ++ [COND temp1 Lesseq temp2 ltrue lfalse])

transCond table (Gequal e1 e2) ltrue lfalse = do -- GREATER OR EQUAL
        temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp e1 table temp1
        code2 <- transExp e2 table temp2
        popTemp 2
        return (code1 ++ code2 ++ [COND temp1 Greaterteq temp2 ltrue lfalse])
        
transCond table (Eq e1 e2) ltrue lfalse = do -- EQUAL
        temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp e1 table temp1
        code2 <- transExp e2 table temp2
        popTemp 2
        return (code1 ++ code2 ++ [COND temp1 IEq temp2 ltrue lfalse])

transCond table (DIff e1 e2) ltrue lfalse = do -- DIFFERENT
        temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp e1 table temp1
        code2 <- transExp e2 table temp2
        popTemp 2
        return (code1 ++ code2 ++ [COND temp1 INeq temp2 ltrue lfalse])

transCond table (BoolVal a) ltrue lfalse -- valores booleanos
        |a == True = do return [JUMP ltrue]
        |a == False = do return [JUMP lfalse]

transCond table (Not exp) ltrue lfalse = do   -- !
        code <- transCond table exp lfalse ltrue 
        return code

transCond table (And e1 e2) ltrue lfalse = do -- &&
        label2 <- newLabel
        code1 <- transCond table e1 label2 lfalse
        code2 <- transCond table e2 ltrue  lfalse
        return (code1 ++ [LABEL label2] ++ code2)

transCond table (Or e1 e2) ltrue lfalse = do -- ||
        label2 <- newLabel
        code1 <- transCond table e1 ltrue label2
        code2 <- transCond table e2 ltrue  lfalse
        return (code1 ++ [LABEL label2] ++ code2)

transCond table sobras ltrue lfalse = do --aqui é condiçoes para as "Sobras"
        t <- newTemp
        code1 <- transCond table (DIff sobras (Num 0)) ltrue lfalse 
        return code1

-- Trans do PROG!

transProg :: Prog -> State Count [Instr]
transProg (Program (Prog_Header name) (Prog_Body constDef procs varDefs (CPstm stmList))) = do
            table1 <- transConstDef [] constDef
            code1 <- transProcs table1 procs
            table2 <- transVarDef table1 varDefs
            code2 <- transStmList table2 stmList
            return ([LABEL name] ++ code1 ++ code2)

-- tem de chamar os de baixo!

-- Trans do PROCEDURE

transProc :: Table -> Proc -> State Count [Instr]
transProc table (ProcedureDef (ProcedureHeader id params) body) = do
            table1 <- transParam table params
            code <- transProcBody table1 body
            return ([LABEL id] ++ code)
transProc table (ProcedureDef (ProcedureHeaderRet id params ty) body) = do
            table1 <- transParam table params
            table2 <- insertVarIntoTable table1 id
            code <- transProcBody table2 body
            return ([LABEL id] ++ code)

transProcBody :: Table -> ProcBody -> State Count [Instr]
transProcBody table (ProcedureBody varDecls (CPstm stmList)) = do
    table1 <- transVarDef table varDecls 
    code <- (transStmList table1 stmList)
    return code

transVarDef :: Table -> [VariableDef] -> State Count Table
transVarDef table [] = return table
transVarDef table ((VarDef var ty):xs) = do
    t1 <- newTemp
    table1 <- transVarDef ((var, t1) : table) xs
    return table1

insertVarIntoTable :: Table -> String -> State Count Table
insertVarIntoTable table varName = do
    t1 <- newTemp
    return ((varName, t1) : table)

insertTempIntoTable :: Table -> Temp -> String -> State Count Table
insertTempIntoTable table varName t1 = do
    return ((varName, t1) : table)

transConstDef :: Table -> [ConstantDef] -> State Count Table
transConstDef table [] = return table
transConstDef table ((ConstDef var ty):xs) = do
    t1 <- newTemp
    table1 <- transConstDef ((var, t1) : table) xs
    return table1

transParam :: Table -> [Param] -> State Count Table
transParam table [] = return table
transParam table ((Parameter var ty):xs) =  do
    t1 <- newTemp
    table1 <- transParam ((var, t1) : table) xs
    return table1 

transProcs :: Table -> [Proc] -> State Count [Instr]
transProcs table [] = return []
transProcs table (x:xs) = do
    code1 <- transProc table x
    code2 <- transProcs table xs
    return (code1 ++ code2)