 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Time-stamp: <2023-02-24 12:17:18 >                                                                         
---------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import AST
import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem
import Help


--------------------------------------------------------------------- 
-- pretty printing 
--------------------------------------------------------------------- 

ppstate :: State -> String
ppstate s = "\n" ++ "  ==>  STATE " ++ show s

ppexpn :: Exp -> String
ppexpn e  = "\n" ++ "  ==>  EXP " ++ ppe e

-- EVALUATE program p and then pretty print the output

ppev :: (Code, State) -> String
ppev p = case p of
             (C c,s) -> let cs = com_evfn(c, s) in ppstate cs
             (E e,s) -> let es = exp_evfn(e, s) in ppexpn es
 

--------------------------------------------------------------------- 
-- IO script
--------------------------------------------------------------------- 





parseIns :: IMPFile -> Pout (Instruction, Tokens)
parseIns inputstr = case ins (tokenize inputstr) of
                    FailAs err -> FailAs err
                    Success (instruction, tokens) -> Success (instruction, tokens)



prompt :: IO ()
prompt = do 
  putStr "\n >IMP> \n"
  inputstr <- getLine -- obtain input from user
  case parseIns inputstr of
    Success (Eval p, []) -> do -- if the input is a program to be evaluated
      putStrLn "Output:"
      putStrLn $ ppev p -- evaluate and pretty print the output
      prompt -- give a fresh prompt
    Success (Trans p, []) -> do
      putStrLn "Transformed program: "
      putStrLn $ ppev p
      prompt
    Success (Help 0, []) -> help 0 >> prompt -- deal with Help 0
    Success (Help k, []) -> help k >> prompt -- deal with Help k (k > 0)
    Success (Quit, []) -> putStrLn "Bye!" -- deal with Quit
    _ -> putStr "\n onk!! Please don't do that.\n" >> prompt


main :: IO()
main = do introduction
          prompt 
