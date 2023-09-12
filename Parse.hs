--------------------------------------------------------------------
-- HASKELL PARSING LIBRARY                       
-- Time-stamp: <2023-02-24 11:55:35 >                             
--------------------------------------------------------------------

-- *****************************************************************
-- See ExamplesForStudents.hs for examples of the functions below !! 
-- *****************************************************************

-- only to code up
-- idr, num, and next ; next 
-- some code involving next needs uncommenting
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module Parse 

where

import Tokens

infixr 5 `next`
infixl 3 `build`
infixl 0 `alt`

---------------------
-- Type of parsers --
---------------------

-- type of parser output: failing, or success with a result of type a 
data Pout t = FailAs String | Success t
              deriving (Eq,Show) 
-- parser type: a parser takes a list of tokens; we return 
type Parse a  = Tokens -> Pout (a,Tokens)

-------------------
-- Basic parsers --
-------------------

-- Fail on any input.
-- you may not need this 
failP :: Parse a 
failP toks = FailAs "We always fail"

-- Succeed with the value given.
-- you may not need this 
succeed :: a -> Parse a 
succeed val toks = Success (val, toks)

-- parse a specified IMPword keyword or symbol
-- key kws toks looks at the head of toks, and if the head is exactly Key kws,
-- the parse is successful with result kws
-- the constructor Key is removed
key :: IMPword -> Parse IMPword
-- NOTE key kws :: Parse IMPword that is Tokens -> Pout (IMPword,Tokens)
-- NOTE key kws toks :: Pout (IMPword,Tokens)
-- NOTE if parser successful the output is of the form
-- Success (<anIMPword>,<someTokens>)
key kws (Key x : toks) = if kws==x then Success (x,toks) else FailAs ("Found a Key Token .. " ++ x ++ " .. but it does not match the input keyword/symbol")
key kws _ = FailAs "Can't find the keyword/symbol .. "

-- parse an IMPword identifier
-- idr tokens looks at the head of tokens, and if the head is Id x, any x, 
-- the parse is successful with result x 
idr :: Parse IMPword
idr (Id x : toks) = Success (x, toks)
idr _  = FailAs "Cant find any Identifier"

-- parse an IMPword positive integer
-- num similar semantics to idr 
num :: Parse IMPword
num (Num x : toks) = Success (x , toks)
num _ = FailAs "Can't find any num" 

 -------------------------
-- Parsing combinators --
-------------------------

-- A choice between two parsers. The function alt ph1 ph2 returns
-- the result of ph1 whenever it succeeds and the result of ph2
-- otherwise. 
alt :: Parse a -> Parse a -> Parse a
alt ph1 ph2 toks = case ph1 toks of
                     FailAs _ -> ph2 toks
                     Success (r,toks') -> Success (r,toks')

-- Sequencing of parsers. The function next ph1 ph2 returns the
-- result, if any, of applying ph1 to the input and then ph2 to
-- the remainder.
next :: Parse a -> Parse b -> Parse (a,b)
next ph1 ph2  = \toks -> case ph1 toks of
                      FailAs _ -> FailAs "next: ph1 fails"
                      Success (r, toks') -> case ph2 toks' of
                                              FailAs _ -> FailAs "next: ph2 fails"
                                              Success (r1, toks') -> Success ((r,r1), toks')

--} 

-- Repetition. The parser ph is used as many times as possible
-- and the results are returned as a list.

many :: Parse a -> Parse [a] 
many ph  = (ph `next` many ph `build` cons) `alt` (\toks -> Success ([], toks))
             where
             cons (x,xs) = x:xs
             {--
--} 

-- Semantic action. The results from a parser ph are transformed by
-- applying a function f.
build :: Parse a -> (a -> b) -> Parse b 
build ph f toks = case ph toks of
                    FailAs _ -> FailAs "Parser for build fails"  
                    Success (r,toks') -> Success (f r, toks')

-- reader returns a function which maps an IMPfile
-- to a Haskell term of type a (eg Exp, Com, etc).
-- A parser p :: Parse a can be converted to such a
-- function:
-- Note there is an error if the input file is not fully consumed by ph 
reader :: Parse a -> IMPFile -> a
reader ph impf
  = case ph (tokenize impf) of
      Success (result,[]) -> result
      other -> error "parse unsuccessful" 
  

