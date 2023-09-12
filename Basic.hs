 
------------------------------------------------------------------- 
-- HASKELL BASIC LIST AND STRING PROCESSING FUNCTIONS           
-- Time-stamp: <2023-02-17 09:45:26 >
                                
------------------------------------------------------------------- 

module Basic where

----------------------------------------------------------------
-- for the Tokenizer 
----------------------------------------------------------------
-- 

-- mem checks whether x is an element of a list
mem :: Eq a => a -> [a] -> Bool
mem _ [] = False
mem x (y:ys) 
    | x == y  = True
    | otherwise = mem x ys

                    


----------------------------------------------------------------
-- for the execution semantics 
----------------------------------------------------------------
--

-- lookup the value of v::a in a state of type [(a,b)]
-- for IMP a and b will be String and Int, for variables and integers
-- and named (type synonyms) V and Z in AST.hs 
-- give an error if the state is empty
lookUp :: Eq a => [(a,b)] -> a -> b
lookUp [] _ = error("State is empty")
lookUp (y:ys) x 
    | fst y == x = snd y
    | otherwise = lookUp ys x 


-- update a state s, given a variable v::a and value z 
-- if v is already declared in s then z is the new value of v
-- if not then v with value z extends s (cons onto s)
-- for IMP a and b will be types V and Z, variables and integers
-- update [("u",1),("v",2)] "v" 6 = [("u",1),("v",6)] 
update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
update [] v z = [(v,z)]
update ((v,z):pairs) v' z'  
    | v == v' = (v', z') : pairs 
    | otherwise = (v,z) : update pairs v' z'
