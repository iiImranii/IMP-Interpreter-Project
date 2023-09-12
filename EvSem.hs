--------------------------------------------------------------------- 
-- SML EVALUATION SEMANTICS FOR IMP                                   
-- Time-stamp: <2023-02-24 12:15:16 >                                                                       
--------------------------------------------------------------------- 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use if" #-}

module EvSem

where

import AST
import Basic

-- Exp_evfn models/encodes deterministic big-step evaluations in 
-- structured operational semantics style 
-- Exp_evfn considers expression cases, and in each non-base case
-- of the form Constructor (e,e') , e.g. 
-- Exp_evfn Constructor (e,e')
-- evaluates e to v then e' to v'
-- and then computes v [[Constructor]] v'
-- where [[Constructor]] is the semantic meaning of Constructor, e.g. 
-- [[Plus]] = + 

-------------------------
-- Evaluating Expressions
-------------------------

exp_evfn :: Exp_conf -> Exp
exp_evfn (Int m,s) = Int m
exp_evfn (Var v,s) = Int (lookUp s v)


exp_evfn (BopExp (Plus, e1, e2), s)  =
  let Int n1 = exp_evfn (e1, s)
      Int n2 = exp_evfn (e2, s)
  in Int (n1 + n2)


exp_evfn (BopExp (Minus, e1, e2), s)  =
  let Int n1 = exp_evfn (e1, s)
      Int n2 = exp_evfn (e2, s)
  in Int (n1 - n2)

exp_evfn (BopExp (Times, e1, e2), s)  =
  let Int n1 = exp_evfn (e1, s)
      Int n2 = exp_evfn (e2, s)
  in Int (n1 * n2)


exp_evfn (BopExp(Gr, e1, e2) , s) = case (exp_evfn (e1, s), exp_evfn (e2, s)) of
  (Int i1, Int i2) -> Bool (i1 > i2)
  _ -> error "Gr expected two integer expressions"

exp_evfn (BopExp(Le, e1, e2) , s) = case (exp_evfn (e1, s), exp_evfn (e2, s)) of
  (Int i1, Int i2) -> Bool (i1 < i2)
  _ -> error "Gr expected two integer expressions"

exp_evfn (BopExp(GrEq, e1, e2) , s) = case (exp_evfn (e1, s), exp_evfn (e2, s)) of
  (Int i1, Int i2) -> Bool (i1 >= i2)
  _ -> error "Gr expected two integer expressions"

exp_evfn (BopExp(LeEq, e1, e2) , s) = case (exp_evfn (e1, s), exp_evfn (e2, s)) of
  (Int i1, Int i2) -> Bool (i1 <= i2)
  _ -> error "Gr expected two integer expressions"



-- ?? .. ?? .. 


----------------------
-- Evaluating Commands
----------------------

type Env = [(String, Int)]

-- | A variable expression

str_to_v :: String -> V
str_to_v s = s


exp_to_v :: Exp -> V
exp_to_v (Var varName) = str_to_v varName
exp_to_v _ = error "Expected variable expression"


exp_to_int :: Exp -> Int
exp_to_int (Int n) = n
exp_to_int e = error ("Cannot convert expression " ++ show e ++ " to an integer")

com_evfn :: Com_conf -> State
com_evfn (Skip,s) = s

com_evfn (Seq(co1, co2), s1) =
    let s2 = com_evfn (co1, s1) in
        com_evfn (co2, s2)


com_evfn (Ass(ex1, ex2), s1) =
    let val = exp_to_int(exp_evfn (ex2, s1))
        v = exp_to_v ex1
    in update s1 v val



com_evfn (If(exp1, com1, com2), s1) =
    case exp_evfn (exp1, s1) of
        Bool True  -> com_evfn (com1, s1)
        Bool False -> com_evfn (com2, s1)
        _          -> error "Expected a boolean expression"


com_evfn (While(exp, com), s) =
  case exp_evfn (exp, s) of
    Bool True ->
      let s' = com_evfn (com, s)
      in com_evfn (While (exp, com), s')
    Bool False -> s
    _ -> error "Expected a boolean expression"





    -- update s1 ex1 (exp_evfn (ex1, s1))


-- ?? .. ?? .. 


