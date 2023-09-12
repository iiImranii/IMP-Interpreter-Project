  
--------------------------------------------------------------------
-- HASKELL IMP PRETTY PRINTER                
-- Time-stamp: <2023-01-25 11:44:22 rlc3>                             
-- Author: Roy L. Crole                                             
--------------------------------------------------------------------

module Pretty where

import Basic
import AST

----------------------------------
-- Bookkeeping Functions        --
----------------------------------

z_to_str :: Z -> String
z_to_str = show

v_to_str :: V -> String 
v_to_str s = s

b_to_str :: B -> String
b_to_str b = if b == True then "true" else "false";

----------------------------
-- Code a Pretty Printer  --
----------------------------

-- set the precedence bindings for the constructors 
precedencec Skip  = 4
precedencec (Ass (v,ie)) = 4
precedencec (If (be,co,co')) = 3
precedencec (While (co,co')) = 2
precedencec (Seq (co,co')) = 1

precedencee (Int z) = 4
precedencee (Var v) = 4
precedencee (Bool b) = 4

precedencee (BopExp (Times,ie,ie')) = 3
precedencee (BopExp (Plus,ie,ie')) = 1
precedencee (BopExp (Minus,ie,ie')) = 1
precedencee (BopExp (Le,ie,ie')) = 1
precedencee (BopExp (Gr,ie,ie')) = 1
precedencee (BopExp (LeEq,ie,ie')) = 1
precedencee (BopExp (GrEq,ie,ie')) = 1

-- test precedence and on lower precedence wrap in brackets 
brackete n e 
  | (precedencee e) >= n = ppe e
  | otherwise           = "("++(ppe e)++")"

bracketc n c
  | (precedencec c) >= n = ppc c
  | otherwise           = "("++(ppc c)++")"

-- a pretty printer for programs
-- ppprog (c,s) = (ppc c, showstate s)
  
-- a pretty printer for commands 
ppc Skip = "skip"
ppc (Ass (Var v,e)) = (v_to_str v) ++ ":=" ++ (brackete 4 e)
ppc (Seq (c1,c2)) = (bracketc 1 c1) ++ " ; " ++ (bracketc 1 c2)
ppc (If(e,c1,c2)) = 
    "if " ++ (brackete 3 e) ++ " then " ++ (bracketc 3 c1) ++ " else " ++ (bracketc 3 c2)
ppc (While(e,c)) = "while " ++ (brackete 2 e) ++ " do " ++ (bracketc 2 c) 

-- a pretty printer for expressions 
ppe (Int z)  = z_to_str z
ppe (Var v)  = v_to_str v
ppe (Bool b) = b_to_str b
ppe (BopExp(Plus,e1,e2)) = case e2 of
                      BopExp(Plus,x,y) -> (brackete 1 e1) ++ "+" ++ ("(" ++ (brackete 1 x) ++ "+" ++ (brackete 1 y) ++ ")")
                      BopExp(Minus,x,y) -> (brackete 1 e1) ++ "-" ++ ("(" ++ (brackete 1 x) ++ "-" ++ (brackete 1 y) ++ ")")
                      _ -> (brackete 1 e1) ++ "+" ++ (brackete 1 e2)
ppe (BopExp(Minus,e1,e2)) = case e2 of
                      BopExp(Minus,x,y) -> (brackete 1 e1) ++ "-" ++ ("(" ++ (brackete 1 x) ++ "-" ++ (brackete 1 y) ++ ")")
                      BopExp(Plus,x,y) -> (brackete 1 e1) ++ "+" ++ ("(" ++ (brackete 1 x) ++ "+" ++ (brackete 1 y) ++ ")")
                      _ -> (brackete 1 e1) ++ "-" ++ (brackete 1 e2)
ppe (BopExp(Times,e1,e2)) = (brackete 3 e1) ++ "*" ++ (brackete 3 e2)
ppe (BopExp(Le,e1,e2)) = (brackete 1 e1) ++ "<" ++ (brackete 1 e2)
ppe (BopExp(Gr,e1,e2)) = (brackete 1 e1) ++ ">" ++ (brackete 1 e2)
ppe (BopExp(LeEq,e1,e2)) = (brackete 1 e1) ++ "<=" ++ (brackete 1 e2)
ppe (BopExp(GrEq,e1,e2)) = (brackete 1 e1) ++ ">=" ++ (brackete 1 e2)
