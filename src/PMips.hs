module CMips where

import AST
import Parser
import Data.Map (Map)
import qualified Data.Map as Map


-- NOT EVEN HALF WAY THERE
--mips :: [Instr] -> IO ()

transText :: [Instr] -> IO ()

transText [] = return ()

transText [instr] -- base case
  = printText instr instr

transText (instr:next:instrs)
  = do printText instr next 
       transText (next:instrs)

-- function that will print EACH instruction according to its content
printText :: Instr -> Instr -> IO ()

printText BREAK _ = return ()

printText (JUMP label) _ = putStrLn ("\tj " ++ label)

printText START _ = putStrLn ("\t.text\n") 

printText (MOVE t1 t2) _ = putStrLn ("\tmove $" ++ t1 ++ ", $" ++ t2)

printText (MOVEI t i) _ = putStrLn ("\tli $" ++ t ++ ", " ++ (show i))

printText (OP op t2 t0 t1) _
  = case op of
      IPlus -> putStrLn ("\tadd $" ++ t2 ++ ", $" ++ t0 ++ ", $" ++ t1)
      ISub -> putStrLn ("\tsub $" ++ t2 ++ ", $" ++ t0 ++ ", $" ++ t1)
      IDiv -> putStrLn ("\tdiv $" ++ t0 ++ ", $" ++ t1 ++ "\n\t mflo $" ++ t2)
      IMult -> putStrLn ("\tmul $" ++ t2 ++ ", $" ++ t0 ++ ", $" ++ t1)
      IMod -> putStrLn ("\tdiv $"++t0++", $"++t1 ++"\n\tmfhi $"++t2)

printText (OPI op t1 t0 i) _
  = case op of
      IPlus -> putStrLn ("\taddi $" ++ t1 ++ ", $" ++ t0 ++ ", " ++ (show i))
      _ -> return ()

printText (LABEL l1) _ = putStrLn (l1 ++ ": ")


printText (COND t1 op t2 l1 l2) (LABEL l)
  | l == l2 = putStrLn ("\t" ++ (opMips "second" op) ++ " $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1)
  | l == l1 = putStrLn ("\t" ++ (opMips "first" op) ++ " $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l2)

-- if we dont have a label after the condition instruction
printText (COND t1 op t2 l1 l2) _
  = putStrLn ("\t" ++ (opMips "second" op) ++ " $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\nj " ++ l2)
                         
printText (RETURN t) _ 
  = putStrLn ("\tmove $v0, $" ++ t ++ "\n\tla $sp, 0($fp)" ++
              "\n\tlw $ra, -8($sp)" ++ "\n\tlw $fp, -4($sp)" ++ "\n\tjr $ra")
             
printText (CALL dest f argList) _ = do
  if (f == "scan_int")
    then do putStrLn ("\tjal scan_int")
            putStrLn ("\tmove $" ++ dest ++ ", $v0") --adicionei esta linha para armazenar o nosso "input" qnd chega do scan
              
    else do printStores (reverse argList) 1
            putStrLn ("\tla $sp, " ++ (show ((-4)*(length argList))) ++ "($sp)")   -- grow stack
            putStrLn ("\tjal " ++ f)                                               -- jump and link
            putStrLn ("\tla $sp, " ++ (show (4*(length argList))) ++ "($sp)")      -- shrink stack
            putStrLn ("\tmove $" ++ dest ++ ", $v0")                                -- save result

printText (Func fun(argList) funCode) _ =
  do putStrLn (fun ++ ":")                                                 -- entry label for fun
     putStrLn ("\tsw $fp, -4($sp)")                                        -- save old $fp
     putStrLn ("\tsw $ra, -8($sp)")                                        -- save return address
     putStrLn ("\tla $fp, 0($sp)")                                         -- setup frame pointer
     putStrLn ("\tla $sp, " ++ (show (length argList * (-4)))  ++ "($sp)") -- allocate frame
     loadArgs argList 0
     transText funCode














-- translation of the conditions depending on the
-- the instruction that comes next: ltrue, lfalse or neither
opMips :: String -> RelOp -> String
opMips "first" op
  | op == Lesst = "bge"
  | op == Lesseq = "bgt"
  | op == Greatert = "ble"
  | op == Greaterteq = "blt"
  | op == INeq = "beq"
  | otherwise = "bne"

opMips "second" op
  | op == Lesst = "blt"
  | op == Lesseq = "ble"
  | op == Greatert = "bgt"
  | op == Greaterteq = "bge"
  | op == INeq = "bne"
  | otherwise = "beq"