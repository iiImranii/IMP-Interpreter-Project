  
--------------------------------------------------------------------
-- HASKELL IMP LANGUAGE PARSING LIBRARY                   
-- Time-stamp: <2023-02-17 12:21:48 >                                   
--------------------------------------------------------------------

-- *****************************************************************
-- See ExamplesForStudents.hs for examples of the functions below !! 
-- *****************************************************************
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module ImpPar where

import Basic
import AST
import Pretty 
import Tokens
import Parse

----------------------------------
-- Bookkeeping Functions        --
----------------------------------

str_to_z :: String -> Z
str_to_z = read

str_to_v :: String -> V 
str_to_v s = s

str_to_b :: String -> B
str_to_b s = if s == "true" then True else False


----------------------------------
-- Top Level Parsing Functions  --
----------------------------------

-- These functions are instances of reader
-- They are not used explicitly in IMP
-- but you may find them useful for testing




readins :: IMPFile -> Instruction
readins = reader ins 

readstate :: IMPFile -> State 
readstate = reader state

readprog :: IMPFile -> Prog
readprog = reader prog 

readcom :: IMPFile -> Com
readcom  = reader com

readexp :: IMPFile -> Exp 
readexp  = reader expn

readiexp :: IMPFile -> Exp 
readiexp = reader iexp

readbexp :: IMPFile -> Exp 
readbexp = reader bexp



---------------------
-- Build Functions --
---------------------

-- parser build functions for the State
makeEmptystate s = []
makeState  ("[",("(",(v,(",",(z,(")",(s,"]"))))))) =
                (v, str_to_z z) : (map aux s)
                where
                aux (",",("(",(v,(",",(z,")"))))) = (v,str_to_z z)

-- parser build functions for programs
makeProg ("(",(code,(",",(s,")")))) = case code of
                                           C c -> (C c,s)
                                           E e -> (E e,s)
                                           
-- parser build functions for code 
makeCCode :: (String, Com) -> Code
makeCCode ("C", c) = C c

makeECode :: (String, Exp) -> Code
makeECode ("E", e) = E e

-- parser build functions for commands and expressions



makeNegNum :: (String, [Char]) -> [Char]
makeNegNum ("-",n) = "-"++n 


makeVar :: String -> Exp
makeVar = Var . str_to_v


makeInt :: String -> Exp
makeInt = Int . str_to_z 

makeBool :: (Exp, (String, Exp)) -> Exp
makeBool (ie1,(op,ie2)) = case op of
                            "<" -> BopExp(Le, ie1, ie2)
                            ">" -> BopExp(Gr, ie1, ie2)
                            "<=" -> BopExp(LeEq, ie1, ie2)
                            ">=" -> BopExp(GrEq, ie1, ie2)


makeExpFromAtom  (_,(e,_)) = e

makeComFromAtom  ("(",(c,")")) = c

makeNegInt ("-",ie) = (Int (-(str_to_z ie)))

makePMT :: Foldable t => (Exp, t (String, Exp)) -> Exp
makePMT (a, la) = let mPM e1 (op,e2) = 
                       case op of
                          "+" -> BopExp(Plus,e1,e2)
                          "-" -> BopExp(Minus,e1,e2)
                          "*" -> BopExp(Times,e1,e2) 
                      in foldl mPM a la

makeAss :: (String, (String, Exp)) -> Com
makeAss (v,(":=",e)) = Ass (makeVar v,e)     

makeSeq :: Foldable t => (Com, t (String, Com)) -> Com
makeSeq (c, lc) = foldl mSeq c lc
  where
    mSeq c1 (";", c2) = Seq (c1, c2)

makeIfte :: (String, (Exp, (String, (Com, (String, Com))))) -> Com
makeIfte ("if",(be,("then",(c1,("else",c2))))) = If (be, c1, c2)

makeWhile ("while",(be,("do",c1))) = While (be, c1) 
{-- 
makeSkip "skip" = ?? 
--} 



----------------------------
-- The Combinatory Parser --
----------------------------

-- quite a lot of the code is given to you
-- !!! uncomment the given code line by line, otherwise you will get many errors !!! 

-- always reference the BNF grammar in the slides
-- before you code any parsing function, you need an RDP grammar for it
-- first understand how the basic parsers key, idr, num work .. see Parset.hs = Parse.hs  
-- .. and look at the file ExamplesForStudents.hs 
-- start coding parsers low in any RDP grammar, and work up 
-- iexp has an RDP grammar in the slides, so complete this first ... 
-- ... by coding integer, then iatom, then factor, then iexp

-- code the alternative parsers one by one, get them working, only then combine with `alt` (why?) 
-- when first coding iatom, leave out the alternative parser for "("<iexp>")" obviously!! 
-- I suggest you first code parsers without builds, so you can see the results r in Success(r,toks) .. 
-- .. and then code the make functions (if required) and use them in your parser code using `build` to compute Success( makeFunction r,toks )
-- so overall for iexp, code up 
-- iatom (using idr, makeVar, integer, makeInt)
-- factor (using iatom, key"*", makePMT)
-- iexp (using factor, key"+", key"-", makePMT)
-- complete iexp with parser code for "("<iexp>")" as an `alt` of iatom.

-- once you have this working, and understand the strategy for design and coding ..
-- .. you should find writing the remainder of the parser very similar 

-- code the parser for an IMP instruction
ins :: Parse Instruction
ins = key"eval" `next` prog `build` (\("eval",p)-> Eval p)
           `alt`
           key"trans" `next` prog `build` (\("trans",p)-> Trans p)
          `alt`
           key"help" `next` num `build` (\("help", k)-> Help (str_to_z k))
           `alt`
           key"quit" `build` (\i -> Quit)
           `alt` 
           key"help" `build` (\i -> Helpp)
           
-- code the parser for an IMP state
state :: Parse State 
state = key"[]" `build` makeEmptystate
        `alt`
        key"[" `next` 
        key"(" `next` idr `next` key","
        `next`
        integer 
        `next` key")"
        `next`
        many (key"," `next` 
              key"(" `next` idr `next` key"," `next` num `next` key")")
        `next` key"]"
        `build` makeState

-- code the parser for IMP programs
prog :: Parse Prog 
prog toks = (key"(" `next` code `next` key"," `next` state `next` key")" `build` makeProg) toks


-- code the parser for IMP code
code :: Parse Code
code toks = (
  key"C" `next` com `build` makeCCode
  `alt`
  key"E" `next` expn `build` makeECode
  ) toks 



-- code the parser for IMP commands
com :: Parse Com
com toks = 
  (ifORwh `next` many (key";" `next` com) `build` makeSeq
  `alt`
  catom `next` many (key";" `next` com) `build` makeSeq
  `alt`
  catom `next` many (key";" `next` catom) `build` makeSeq
 
  ) toks

ifORwh :: Parse Com
ifORwh toks = (
  key"if" `next` bexp `next` key"then" `next` com `next` key"else" `next` com `build` makeIfte
  `alt`
  key"while" `next` bexp `next` key"do" `next` com `build` makeWhile
 
  ) toks


   
ass :: Parse Com
ass toks = 
  (
    idr `next` key":=" `next` expn `build` makeAss  
  ) toks


catom  :: Parse Com
catom toks =
  (
  key"(" `next` com `next` key")" `build` makeComFromAtom
   `alt`
  ass

  ) toks
  
-- code the parser for IMP expressions
expn :: Parse Exp
expn toks = (
  iexp
 `alt`
  bexp
   ) toks
       
-- code the parser for IMP Boolean expressions
bexp :: Parse Exp
bexp toks = 
  (
    iexp `next` key">" `next` iexp `build` makeBool 
    `alt`
    iexp `next` key">=" `next` iexp `build` makeBool
    `alt`
    iexp `next` key"<=" `next` iexp `build` makeBool
    `alt`
    iexp `next` key"<" `next` iexp `build` makeBool 
    `alt`
    batom
  ) toks


batom :: Parse Exp
batom toks = 
  (key"true"  `build` (Bool . str_to_b)
   `alt`
   key"false" `build` (Bool . str_to_b)
   `alt`
   key"(" `next` bexp `next` key")" `build` makeExpFromAtom
  )  toks
    

-- code the parser for IMP integer expressions
iexp :: Parse Exp
iexp toks = 
  ( 
    factor `next` many ((key"+" `alt` key"-")  `next` factor) `build` makePMT
  
  
  ) toks


factor :: Parse Exp
factor toks = 
  (iatom `next` many(key"*" `next` iatom)  `build` makePMT
   `alt`
   iatom
  ) toks


iatom :: Parse Exp
iatom toks = 
  (idr `build` makeVar 
   `alt` 
   integer `build` makeInt
   `alt` 
   key"(" `next` iexp `next` key")"  `build` makeExpFromAtom
   -- UNCOMMENT THIS THIRD ALTERNATIVE PARSER AS THE FINAL STEP OF CODING iexp:   
  ) toks
--} 




-- code parser for IMP integers
-- NOTE the type
integer :: Parse IMPFile
integer toks = ( 
                num 
                `alt`
                 ( key"-" `next` num `build` makeNegNum ) ) toks 


