module Typecheck where

import           Parser

import           Data.Map(Map)
import qualified Data.Map as Map

-- type environment (i.e. symbol table)
type TypeEnv = Map Ident Type1

----------------------------------------------------------------------------------
-- Expressions


checkExpr :: TypeEnv -> Expr1 -> Type1
checkExpr env (Num n) = TyInt
checkExpr env (Bool n) = TyBool
checkExpr env (Var x) = case Map.lookup x env of
    Nothing -> error "undeclared variable"
    Just t -> t
checkExpr env (Add e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyInt
         else error "type error in +"

checkExpr env (Sub e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1==TyInt && t2==TyInt then TyInt
       else error "type error in -"

checkExpr env (Div e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1==TyInt && t2==TyInt then TyInt
        else error "type error in IDiv"

checkExpr env (Mod e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1==TyInt && t2==TyInt then TyInt
        else error "type error in IMod"

checkExpr env (Mult e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
    in if t1==TyInt && t2==TyInt then TyInt
        else error "type error in *"

checkExpr env (LessThan e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in <"

checkExpr env (LessEq e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in <="    

checkExpr env (Eq e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in ="  

checkExpr env (Greatert e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in >" 

checkExpr env (Greaterteq e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in >="    

checkExpr env (Neq e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in <>" 

checkExpr env (And e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in and" 

checkExpr env (Or e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in or"

checkExpr env (Not e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in not"

checkExpr env (Negate e1)
   = let t1 = checkExpr env e1
      in if t1==TyInt then TyBool
         else error "type error in -Int"  

checkExpr env (FunCall f args)
  = let ts = map (checkExpr env) args
    in case Map.lookup f env of
         Just (TyFun ts' t) ->
           if ts == ts' then t
           else error "type error in application"
         _ -> error "invalid function name"

-------------------------------------------------------------------------------------
-- Statements

checkStm :: TypeEnv -> Maybe Type1 -> Stm1 -> Bool

checkStm env tyret (Assign var expr)
  = case Map.lookup var env of
      Nothing -> error "undeclared variable"
      Just t1 -> let t2 = checkExpr env expr
                 in if t1 == t2  then True
                    else error "type error in assignment"

checkStm env tyret (IfElse cond stm1 stm2)
  = let t0 = checkExpr env cond
    in if t0 == TyBool then
         checkStm env tyret stm1 &&
         checkStm env tyret stm2
       else error "type error: condition should be bool"

checkStm env tyret (If cond stm)
  = let t0 = checkExpr env cond
    in if t0 == TyBool then
         checkStm env tyret stm
       else error "type error: condition should be bool"

checkStm env tyret (While cond stm1)
  = let t0 = checkExpr env cond
    in if t0 == TyBool then checkStm env tyret stm1
       else error "type error: condition should be bool"

{-Não tenho a certeza se isto está certo-}
{-checkStm env tyret (Coumpound stm1 stm2)
  = let t0 = checkStm stm1
    let t1 = checkStm stm
    in if t0 == TyBool then
         checkStm env tyret stm
       else error "type error: condition should be bool"-}

extendEnv :: TypeEnv -> [Decl] -> TypeEnv
extendEnv env [] = env
extendEnv env ((v,t):rest) = extendEnv (Map.insert v t env) rest

checkFunDef :: TypeEnv -> FunDef1 -> TypeEnv
checkFunDef env (FunDef fun decls tyret stm)
  = let tyargs = map snd decls
        env' = extendEnv env ((fun, TyFun tyargs tyret):decls)
    in if checkStm env' (Just tyret) stm then
         extendEnv env [(fun,TyFun tyargs tyret)]
       else error "type error in function definition"

checkProg :: Prog -> Bool
checkProg (Prog defs stm)
  = let env = foldl checkFunDef Map.empty defs
    in checkStm env Nothing stm
