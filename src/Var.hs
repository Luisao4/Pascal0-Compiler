module Var where

import Parser
import Data.String
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Ident = String

--- types
data Type1 = TyInt              -- integers
          | TyBool             -- booleans
          | TyFun [Type1] Type1  -- functions
          deriving (Show, Eq)

--- expressions
data Expr1 = Var Ident             -- x, y, z
          | Num Int               -- 1, 2, 3
          | Bool Expr1               -- 1, 2, 3
          | Add Expr1 Expr1         -- e1 + e2
          | Sub Expr1 Expr1         -- e1 - e2
          | Mod Expr1 Expr1         -- e1 - e2    
          | Mult Expr1 Expr1         -- e1 - e2
          | Div Expr1 Expr1         -- e1 - e2                  
          | LessThan Expr1 Expr1         -- e1 - e2         
          | LessEq Expr1 Expr1         -- e1 - e2         
          | Eq Expr1 Expr1         -- e1 - e2         
          | Greatert Expr1 Expr1         -- e1 - e2         
          | Greaterteq Expr1 Expr1         -- e1 - e2         
          | Neq Expr1 Expr1         -- e1 - e2         
          | And Expr1 Expr1         -- e1 - e2         
          | Or Expr1 Expr1         -- e1 - e2     
          | Not Expr1 Expr1         -- e1 - e2             
          | Negate Expr1          -- e1 - e2         
          | FunCall Ident [Expr1]  -- f(e1,e2,...)
          deriving Show

--- statements
data Stm1 = Assign Ident Expr1        -- var = expr
         | IfElse Expr1 Stm1 Stm1  -- if (cond) stm1 else stm2
         | If Expr1 Stm1          -- if (cond) stm
         | While Expr1 Stm1           -- while (cond) stm
         | Block [Decl] [Stm1]       -- { decls; stms }
         | Return Expr1              -- return expr
         | Coumpound Expr1 Expr1
         deriving Show

--- variable declarations
type Decl = (Ident, Type1)           -- variable, type

--- function declarations
data FunDef1
  = FunDef Ident [(Ident,Type1)] Type1 Stm1
  deriving Show

--- complete programs
data Prog1 = Prog [FunDef1] Stm1
  deriving Show
