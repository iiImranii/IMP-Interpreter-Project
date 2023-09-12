 
--------------------------------------------------------------------
-- HASKELL ABSTRACT SYNTAX TREES FOR IMP                  
-- Time-stamp: <2023-02-17 11:07:26 >                                    
--------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module AST where 

type V = String
type B = Bool
type Z = Int

data Instruction = Eval Prog | Trans Prog | Helpp | Help Int | Quit 
                       deriving (Eq,Show)

type Prog  = (Code,State)  

-- state is a partial mapping from variables to integers 
type State = [(V, Z)]

data Code  = E Exp | C Com
              deriving (Eq,Show)

-- **** COMPLETE the two datatypes below using Int, Bool, Var, BopExp
-- Skip, Ass, Seq, If, While
-- **** THEN uncomment the remaning code in AST.hs 


data Exp   =  Int Z 
            | Bool B
            | Var V
            | BopExp (Binop, Exp, Exp)
                deriving (Eq, Show)

data Com   = Skip 
            | Ass (Exp, Exp)
            | Seq (Com, Com)
            | If (Exp, Com, Com)
            | While (Exp, Com)
                deriving (Eq, Show)

data Binop = Plus | Minus | Times | Le | Gr | LeEq | GrEq
             deriving (Eq,Show)


-- we put the types for configurations in the AST file
type Exp_conf = (Exp, State)  -- a configuration is an expression and state 
type Com_conf = (Com, State)  -- a configuration is a command and state 

