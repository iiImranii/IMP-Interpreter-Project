
---------------------------------------------------------------------------
-- HASKELL TOKENS FOR EXPRESSIONS AND COMMANDS FOR IMPERATIVE LANGUAGE IMP                          
-- Time-stamp: <2023-02-24 11:40:51 >
-- File: Tokens.hs (please rename)                             
---------------------------------------------------------------------------

-- *****************************************************************
-- See ExamplesForStudents.hs for examples of the functions below !! 
-- *****************************************************************
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

module Tokens where

import Basic

type IMPFile = String
type IMPword = String
type IMPwords = [IMPword]

-- a token is an identifier, keyword or integer
-- we use Num since Int is a Haskell constructor
data Token = Id IMPword | Key IMPword | Num IMPword
             deriving Show
type Tokens = [Token]


----------------------------------
-- very simple definitions
--

-- these are given to you 
keywords  :: IMPwords
keywords = ["true","false","while","do","if","then","else","skip","eval","trans","help","quit","C","E"]

-- these are given to you 
symbols :: IMPwords
symbols = ["(",")","+","-","*","<=",">=","<",">",";",":=","[",",","]","[]"]

-- is c a letter, digit, etc? Use the IMP BNF grammar! 
-- each function below :: Char -> Bool
is_letter :: Char -> Bool
is_letter c = 'A'<=c && c<='Z' || 'a'<=c && c<='z' 

is_digit :: Char -> Bool
is_digit c = '0'<=c && c<= '9'-- ??

is_neg :: Char -> Bool
is_neg c = '-' == c 
specials = ",!@#$%^&*()_-+=|[]:;'~`<>.?/"
-- use a function from Basic.hs below 
is_special :: Char -> Bool
is_special c = c `mem` specials
--} 

----------------------------------


----------------------------------
-- alpha, numeric and symbolic are used to construct tokens 
--

-- alpha ("roy", "myvar+(77+88)") = ("roymyvar","+(77+88)")
-- alpha ("roy", "8+myvar+(77+88)") = ("roy","8+myvar+(77+88)")
-- see 
-- alpha takes as many letters in sequence from IMPfile as possible ..
-- .. and appends the sequence to the right of the String 
alpha :: (String, IMPword) -> (String, IMPword)
alpha (al, c:cs) = if is_letter c then alpha(al++[c],cs) else (al, c:cs)
alpha (al,[]) = (al, [])

-- same as alpha but takes as many digits as possible 
numeric :: (String, IMPword) -> (String, IMPword)
numeric (num, []) = (num, [])
numeric (num, c:cs) = if is_digit c then numeric (num++[c], cs) else (num, c:cs)
-- symbolic ("roy", "8<=myvar+(77+88)") = ("roy","8<=myvar+(77+88)")
-- symbolic ("roy", "<=myvar+(77+88)") = ("roy","<=myvar+(77+88)")
-- symbolic ("", "<=myvar+(77+88)") = ("<=","myvar+(77+88)")
-- symbolic ("<", "=myvar+(77+88 )") = ("<=","myvar+(77+88)")
-- for symbolic (sy, c:cs)
-- if c is not special, return (sy, c:cs). Else 
-- symbolic takes the head of the IMPfile, c .. 
-- .. and checks whether sy++[c] is in symbols
-- .. if so calls symbolic on (sy++[c],cs)
-- if the check fails, return (sy, c:cs)
symbolic :: (String, IMPword) -> (String, IMPword)

symbolic (sy, c:cs) =
  if is_special c then
    if (sy++[c]) `mem` symbols then
      symbolic (sy++[c], cs) 
    else (sy, c:cs)
  else (sy, c:cs)
symbolic (sy, []) = (sy, [])
--} 

----------------------------------


----------------------------------
-- tokenize defined in terms of scanning 
--

-- scanning ( [] ,   "if true then x:=1 else skip"  )
-- = 
-- [Key "if",Key "true",Key "then",Id "x",Key ":=",Num "1",Key "else",Key "skip"]
-- scanning ( [Key "if",Key "true",Key "then"] ,   "x:=1 else skip"  ) 
-- = 
-- [Key "if",Key "true",Key "then",Id "x",Key ":=",Num "1",Key "else",Key "skip"]

-- check if the head of IMPFile is a digit, letter, or is special
-- in each case, make use of alpha, numeric or symbolic to form a string token ..
-- .. append the datatype token onto the current datatype tokens, and then call scanning recursively.

-- given ("", "45+kk") the head of the IMPFile, '4', is a digit 
-- numeric ("", "45+kk") = ("45","+kk")
-- scanning ([],"45+kk") = scanning ([]++(Num "45"),"+kk") = [Num "45",Key "+",Id "kk"]

-- don't forget "g" = ['g'] :: String !!
-- don't forget "8" = ['8'] :: String !!
scanning :: (Tokens, IMPFile) -> Tokens 
scanning (toks, []) = toks

scanning (toks, c:cs)
  | is_letter c  = let (al, cs2) = alpha([c],cs) in
                   if al `mem` keywords then scanning(toks++[Key al],cs2) else scanning(toks++[Id al],cs2)
  | is_special c = let (si, cs2) = symbolic([c], cs) in
                   if si `mem` symbols then scanning(toks++[Key si],cs2) else scanning(toks, cs2)
  | is_digit c   = let (nu, cs2) = numeric([c], cs) in  scanning(toks++[Num nu],cs2)
  | otherwise = scanning(toks,cs)
  


            
-- tokenize "myvar+(77+88)"
-- = 
-- [Id "myvar",Key "+",Key "(",Num "77",Key "+",Num "88",Key ")"]
-- make a simple use of scanning 

tokenize :: IMPFile -> Tokens 
tokenize impf = scanning([], impf)


--w| is_special c =let (si, cs2) = symbolic([c], cs) in scanning(toks++[Key si],cs2) --else --scanning(toks, cs2)