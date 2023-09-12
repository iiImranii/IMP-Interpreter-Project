 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL IMP EXAMPLES                              
-- Time-stamp: <2023-03-22 17:10:29 >                                                                      
---------------------------------------------------------------------


----------------------------------
-- using Tokens functions 
--


ghci> alpha ("roy", "myvar+(77+88)")
("roymyvar","+(77+88)")

ghci> alpha ("roy", "8+myvar+(77+88)")
("roy","8+myvar+(77+88)")

ghci> symbolic ("roy", "8<=myvar+(77+88)")
("roy","8<=myvar+(77+88)")

ghci> symbolic ("roy", "<=myvar+(77+88)")
("roy","<=myvar+(77+88)")

ghci> symbolic ("", "<=myvar+(77+88)")
("<=","myvar+(77+88)")

ghci> symbolic ("<", "=myvar+(77+88)")
("<=","myvar+(77+88)")

ghci> scanning ( [Key "if",Key "true",Key "then"] ,   "x:=1 else skip"  )
[Key "if",Key "true",Key "then",Id "x",Key ":=",Num "1",Key "else",Key "skip"]
  
ghci> tokenize "if true then x:=1 else skip"
[Key "if",Key "true",Key "then",Id "x",Key ":=",Num "1",Key "else",Key "skip"]

ghci> tokenize "myvar+(77+88)"
[Id "myvar",Key "+",Key "(",Num "77",Key "+",Num "88",Key ")"]


----------------------------------

----------------------------------
-- using Basic Parsers key idr num 
--

ghci> (key"-") (tokenize "+8")
FailAs "Found a Key Token .. + .. but it does not match the input keyword/symbol"

ghci> (key"+") (tokenize "8+8")
FailAs "Can't find the keyword/symbol .. "

ghci> (key"+") (tokenize "+8")
Success ("+",[Num "8"])

ghci> idr (tokenize "myvar77+8")
Success ("myvar",[Num "77",Key "+",Num "8"])

ghci> num (tokenize "var-77+8")
FailAs "Can't find any Num .. "

ghci> num (tokenize "var-77+8")

ghci> num (tokenize "89")
Success ("89",[])

ghci> num (tokenize "-89")
FailAs "Can't find any Num .. "

ghci> integer (tokenize "-89")
Success ("-89",[])

----------------------------------

----------------------------------
-- using combinator alt
--

ghci> (key"-") (tokenize "+8")
FailAs "Found a Key Token .. + .. but it does not match the input keyword/symbol"

ghci> (key"+") (tokenize "+8")
Success ("+",[Num "8"])

ghci> (key"+" `alt` key"-") (tokenize "+8")
Success ("+",[Num "8"])

ghci> (key"+" `alt` key"-") (tokenize "-8")
Success ("-",[Num "8"])

----------------------------------


----------------------------------
-- using next 
--

-- (num `next` (key"+")) :: Parse (IMPword, IMPword)
ghci> (num `next` (key"+")) [Num "6",Key"+"]
Success (("6","+"),[])

ghci> (num `next` (key"+")) [Id "r",Key"+"]
FailAs "next: ph1 fails "

ghci> (num `next` (key"+")) [Num "6",Key"-"]
FailAs "next: ph2 fails"

--  (num `next` (key"+") `next` num):: Parse (IMPword, (IMPword, IMPword))
ghci> (num `next` (key"+") `next` num) [Num "6",Key"+",Num"8"]
Success (("6",("+","8")),[])

----------------------------------


----------------------------------
-- using many: does not fail; may return []
--

-- many num :: Parse [IMPword]
ghci> many num [Num "6",Num "8",Key"+"]
Success (["6","8"],[Key "+"])

ghci> many num [Id"myvar", Num "6",Num "8",Key"+"]
Success ([],[Id "myvar",Num "6",Num "8",Key "+"])

ghci> many (next (key"+") num) [Key "+",Num "8",Key"+",Num"8"]
Success ([("+","8"),("+","8")],[])

ghci> many (next (key"+") num) [Key "+",Num "8",Key"-",Num"8"]
Success ([("+","8")],[Key "-",Num "8"])

ghci> many (next (key"+") num) [Key "+",Id"myvar",Key"-",Num"8"]
Success ([],[Key "+",Id "myvar",Key "-",Num "8"])

ghci> many (next (key"+") idr) [Key "+",Id"myvar",Key"-",Num"8"]
Success ([("+","myvar")],[Key "-",Num "8"])

-- many (next (key"+") idr) :: Parse [(IMPword, IMPword)]
ghci> many (next (key"+") idr) [Key "-",Id"myvar",Key"-",Num"8"]
Success ([],[Key "-",Id "myvar",Key "-",Num "8"])

----------------------------------


----------------------------------
-- using build  
--

makePlus (n , ("+", m)) = BopExp (Plus, Int (read n), Int (read m))

-- compare  ... 

ghci> (num `next` (key"+") `next` num) (tokenize "6+8")
Success (("6",("+","8")),[])

-- with  ... 

ghci> (num `next` (key"+") `next` num `build` makePlus) [Num "6",Key"+",Num"8"]
Success (BopExp (Plus,Int 6,Int 8),[])

state' = key"[" `next` key"(" `next` idr `next` key"," `next` integer `next` key")"  `next` many (key"," `next`  key"(" `next` idr `next` key"," `next` num `next` key")")  `next` key"]"

-- compare  ... 

ghci> state' (tokenize "[(x,7),(y,9)]" )
Success (("[",("(",("x",(",",("7",(")",([(",",("(",("y",(",",("9",")")))))],"]"))))))),[])

-- with  ... 

ghci> (state' `build` makeState)  (tokenize "[(x,7),(y,9)]" )
Success ([("x",7),("y",9)],[])

----------------------------------

----------------------------------
-- example parsing 
--

-- complete success

-- iexp :: Parse Exp 
ghci>   iexp (tokenize "8*2+(4*3)")
Success (BopExp (Plus,BopExp (Times,Int 8,Int 2),BopExp (Times,Int 4,Int 3)),[])

-- this is code for factor on tokenised "8*2" ; complete success 
ghci>   factor (tokenize "8*2")
Success (BopExp (Times,Int 8,Int 2),[])

-- this is code for (part of) factor on tokenised "8*2" ; complete success
-- (iatom `next` many (key"*" `next` iatom) `build` makePMT) :: Parse Exp
ghci>   (iatom `next` many (key"*" `next` iatom) `build` makePMT) (tokenize "8*2")
Success (BopExp (Times,Int 8,Int 2),[])

-- this is code for (part of) factor on tokenised "8*2*9" ; complete success 
ghci>   (iatom `next` many (key"*" `next` iatom) `build` makePMT) (tokenize "8*2*9")
Success (BopExp (Times,BopExp (Times,Int 8,Int 2),Int 9),[])

-- this is code for (part of) factor without building an Exp with the makeFunction makePMT 
-- (iatom `next` many (key"*" `next` iatom) ) :: Parse (Exp, [(IMPword, Exp)])
ghci>   (iatom `next` many (key"*" `next` iatom) ) (tokenize "8*2*9")
Success ((Int 8,[("*",Int 2),("*",Int 9)]),[])

-- this is code for (part of) factor without using any makeFunction
-- (integer `next` many (key"*" `next` integer) )  :: Parse (IMPFile, [(IMPword, IMPFile)])
ghci> (integer `next` many (key"*" `next` integer) ) (tokenize "8*2*9")
Success (("8",[("*","2"),("*","9")]),[])

-- here is the application of the makeFunction makePMT, which is defined with a foldl
-- makePMT ::(Exp, [(String, Exp)]) -> Exp
ghci> makePMT (Int 8,[("*",Int 2),("*",Int 9)])
BopExp (Times,BopExp (Times,Int 8,Int 2),Int 9)

-- ======

-- partial success

-- factor :: Parse Exp 
ghci>   factor (tokenize "8*2+(4*3)")
Success (   BopExp (Times,Int 8,Int 2)  ,  [Key "+",Key "(",Num "4",Key "*",Num "3",Key ")"]   )

-- this is code for (part of) factor without building an Exp with a makeFunction
-- (iatom `next` many (key"*" `next` iatom)) :: Parse (Exp, [(IMPword, Exp)])
ghci> (iatom `next` many (key"*" `next` iatom)) (tokenize "8*2+(4*3)")
Success (  (Int 8,  [("*", Int 2)])  ,   [Key "+",Key "(",Num "4",Key "*",Num "3",Key ")"]  )
  
-- here is the application of the makeFunction makePMT, which is defined with a foldl 
ghci> makePMT (Int 8,  [("*", Int 2)])
BopExp (Times,Int 8,Int 2)

-- ======

-- * before + 

ghci> iexp (tokenize "1+2*3")
Success (BopExp (Plus,Int 1,BopExp (Times,Int 2,Int 3)),[])

-- calling iexp, without builds, effectively executes the code below
ghci> (integer `next` ( many (key"+" `next`    (integer `next` many (key"*" `next` integer))    ))) (tokenize "1+2*3")
Success (  ("1",  [("+",  ("2",[("*","3")])  )]   ),[])

ghci> iexp (tokenize "1*2+3")
Success (BopExp (Plus,BopExp (Times,Int 1,Int 2),Int 3),[])

-- calling iexp, without builds, effectively executes the code below
ghci> (    (integer `next` many (key"*" `next` integer))     `next`( many (key"+" `next` integer))) (tokenize "1*2+3")
Success (  ( ("1",[("*","2")])  ,  [("+","3")]   ),[])

-- ======

-- iatom only 
ghci> iatom (tokenize "4*3")
Success (Int 4,[Key "*",Num "3"])

-- complete bracket enclosure calls iexp, so complete success 
ghci> iatom (tokenize "(4*3+8*76+8)")
Success (BopExp (Plus,BopExp (Plus,BopExp (Times,Int 4,Int 3),BopExp (Times,Int 8,Int 76)),Int 8),[])
ghci> 

-- other examples below 
ghci>  iatom (tokenize "(3+5*4+(5+7))*8")
Success (BopExp (Plus,BopExp (Plus,Int 3,BopExp (Times,Int 5,Int 4)),BopExp (Plus,Int 5,Int 7)),[Key "*",Num "8"])

ghci> (idr `next` key":=" `next` iexp ) [Id "var",Key ":=",Num "8"]
Success (("var",(":=",Int 8)),[])

ghci> (idr `next` key":=" `next` iexp `build` makeAss) [Id "var",Key ":=",Num "8"]
Success (Ass (Var "var",Int 8),[])

ghci> many (next (key"+") num) [Key "+",Num "8",Key "-",Key "+",Num "8"]
Success ([("+","8")],[Key "-",Key "+",Num "8"])

ghci>  (   ( (integer `build` makeInt)  `next` (many (key"*" `next` (integer `build` makeInt))) `build` makePMT )    ) (tokenize "1*2+3")
Success (BopExp (Times,Int 1,Int 2),[Key "+",Num "3"])

ghci>   ( (integer )  `next` (many (key"*" `next` (integer )))  ) (tokenize "1*2+3")
Success (("1",[("*","2")]),[Key "+",Num "3"])

ghci> ( (integer )  `next` (many (key"*" `next` (integer )))  ) (tokenize "-1*2+-3")
Success (("-1",[("*","2")]),[Key "+",Key "-",Num "3"])

ghci> ( (integer )  `next` (many (key"*" `next` (integer ))) `next` key"+" `next` integer ) (tokenize "-1*2+-3")
Success (("-1",([("*","2")],("+","-3"))),[])

{--
TEXT BELOW NOT TESTED, BUT FEEL FREE TO USE IT AS A BASIS FOR YOUR OWN TESTS

  (integer `build` makeInt) (tokenize "89")

  (factor `next` many ((key"+" `alt` key"-") `next` factor) ) (tokenize "89+67")

  (factor `next` many ((key"+" `alt` key"-") `next` factor) `build` makePMT) (tokenize "89+67")

  (iatom `next` many (key"*" `next` iatom)) (tokenize "1*2+3")

  (iatom `next` many (key"+" `next` iatom)) (tokenize "1*2+3")

  (factor `next` many (key"+" `next` factor)) (tokenize "1*2+3")

  (integer `build` makeInt
   `alt`
  (integer `next` (many (key"*" `next` integer) ) `build` makePMT ) (tokenize "1*2+3")

  (integer `build` makeInt)  (tokenize "1*2+3")
  
  ( (integer `build` makeInt)  `next` (many (key"*" `next` (integer `build` makeInt))) `build` makePMT )  (tokenize "1*2+3")

  ( ( (integer `build` makeInt)  `next` (many (key"*" `next` (integer `build` makeInt))) `build` makePMT )    `alt` (integer `build` makeInt)         ) (tokenize "1*2+3")

  ( (integer `build` makeInt)  `next` (many (key"*" `next` (integer `build` makeInt))) `build` makePMT )  (tokenize "*2+3")

  ( (integer `build` makeInt)  `next` (many (key"*" `next` (integer `build` makeInt))) `build` makePMT )  (tokenize "*2+3")

  (many ((key"+" `alt` key"-") `next` factor)) (tokenize "+(4*3)")

  (iatom `next` many (key"*" `next` iatom)) (tokenize "(4*3)") -- NOTE "(4*3)" *IS* an iatom ... 

  (key"(" `next` iexp `next` key")") (tokenize "(4*3)") -- and so this <-- code is applied

  iexp (tokenize "8*2+(4*3)+8+9*7*7") 

--}


----------------------------------

----------------------------------
-- using reader 
--

ghci> readiexp "8+1*2"
BopExp (Plus,Int 8,BopExp (Times,Int 1,Int 2))

ghci> readcom "if true then a:=4 else (while false do a:=4)"
If (Bool True,Ass (Var "a",Int 4),While (Bool False,Ass (Var "a",Int 4)))

ghci> readcom "while true do x:=x+1 ; y := y+1 ; z:=z+1"
Seq (Seq (While (Bool True,Ass (Var "x",BopExp (Plus,Var "x",Int 1))),Ass (Var "y",BopExp (Plus,Var "y",Int 1))),Ass (Var "z",BopExp (Plus,Var "z",Int 1)))

====

readcom "while true do x:=1 ; y:=2 ; z:=3"
readcom "((while true do (x:=1)) ; (y:=2)) ; (z:=3)"
Seq (    Seq (    While (Bool True,Ass (Var "x",Int 1))   ,  Ass (Var "y",Int 2)  )    ,   Ass (Var "z",Int 3))

readcom "z := 3 ; while true do x:=1 ; y := 2"
readcom "((z:=3) ; (while true do (x:=1))) ; (y:=2)"
Seq (Seq (Ass (Var "z",Int 3),While (Bool True,Ass (Var "x",Int 1))),Ass (Var "y",Int 2))

com  (tokenize   "while true do (x:=1 ; y:=2 ; z:=3)")
= 
readcom "while true do (((x:=1) ; (y:=2)) ; (z:=3))"
Success (While (Bool True,Seq (Seq (Ass (Var "x",Int 1),Ass (Var "y",Int 2)),Ass (Var "z",Int 3))),[])

readcom "if true then x:=4 else x:=5 ; while true do y:=7 ; z:=9"
readcom "((if true then (x:=4) else (x:=5)) ; (while true do (y:=7))) ; (z:=9)"
Seq (Seq (If (Bool True,Ass (Var "x",Int 4),Ass (Var "x",Int 5)),While (Bool True,Ass (Var "y",Int 7))),Ass (Var "z",Int 9))

readcom "((if true then (x:=4) else (x:=5)) ; (while true do (y:=7))) ; (z:=9)"
Seq (Seq (If (Bool True,Ass (Var "x",Int 4),Ass (Var "x",Int 5)),While (Bool True,Ass (Var "y",Int 7))),Ass (Var "z",Int 9))

readcom "if true then x:=4 else (x:=5 ; (while true do y:=7 ; z:=9))"
If (Bool True,Ass (Var "x",Int 4),Seq (Ass (Var "x",Int 5),Seq (While (Bool True,Ass (Var "y",Int 7)),Ass (Var "z",Int 9))))

=====

ghci> reader iexp "8+1*2"
BopExp (Plus,Int 8,BopExp (Times,Int 1,Int 2))

ghci> reader prog "(C x:=8,[(x,9)])"
(C (Ass (Var "x",Int 8)),[("x",9)])

ghci> reader ins "eval (E 5+7,[])"
Eval (E (BopExp (Plus,Int 5,Int 7)),[])

ghci> readins "eval (E (4+n)+6,[(n,-1)])"
Eval (E (BopExp (Plus,BopExp (Plus,Int 4,Var "n"),Int 6)),[("n",-1)])

ghci> reader (idr `next` key":=" `next` iexp `build` makeAss) "var := 8"
Ass (Var "var",Int 8)

ghci> reader (iatom `next` many (key"*" `next` iatom) `build` makePMT `alt` iatom) "8+1*2"
*** Exception: parse unsuccessful

ghci> reader (iatom `next` many (key"+" `next` iatom) `build` makePMT `alt` iatom) "8+1+2"
BopExp (Plus,BopExp (Plus,Int 8,Int 1),Int 2)

----------------------------------


----------------------------------
-- using exp_evfn and com_evfn
--

ghci> exp_evfn (BopExp(Plus, BopExp(Plus,Int 4,Var "n") , Int 6),[("n",-1)])
Int 9

ghci> com_evfn (Seq (Ass (Var "x",Int 34),Ass (Var "y",Int 8)) , []  )
[("x",34),("y",8)]


----------------------------------


----------------------------------
-- using pretty printing 
--

ghci> ppe (Int 9) 
"9"

ghci>   readexp "8+1*2"
BopExp (Plus,Int 8,BopExp (Times,Int 1,Int 2))

ghci>   ppe (BopExp (Plus,Int 8,BopExp (Times,Int 1,Int 2)))
"8+1*2"

ghci>   readcom "x:=34 ; y:=8"
Seq (Ass (Var "x",Int 34),Ass (Var "y",Int 8))

ghci>   ppc (Seq (Ass (Var "x",Int 34),Ass (Var "y",Int 8)))
"x:=34 ; y:=8"

----------------------------------


----------------------------------
-- using IMP eval 
--

-------
 >IMP> 
eval (E (4+n)+6,[(n,-1)])

  ==>  EXP 9

ghci> exp_evfn (BopExp(Plus, BopExp(Plus,Int 4,Var"n") , Int 6),[("n",-1)])
Int 9
-------

>IMP> 
eval (C fac:=1 ; (while n>=2 do (if n<=1 then fac:=1 else fac := fac*n ; n:=n-1))  , [(n,5)] )

  ==>  STATE [("n",1),("fac",120)]

com (tokenize "(fac:=1 ; (while n>=2 do (if n<=1 then fac:=1 else fac := fac*n ; n:=n-1)))")

ghci> readcom "(fac:=1 ; (while n>=2 do (if n<=1 then fac:=1 else fac := fac*n ; n:=n-1)))"
Seq (Ass (Var "fac",Int 1),While (BopExp (GrEq,Var "n",Int 2),Seq (If (BopExp (LeEq,Var "n",Int 1),Ass (Var "fac",Int 1),Ass (Var "fac",BopExp (Times,Var "fac",Var "n"))),Ass (Var "n",BopExp (Minus,Var "n",Int 1)))))

ghci> com_evfn ( Seq (Ass (Var "fac",Int 1),While (BopExp (GrEq,Var "n",Int 2),Seq (If (BopExp (LeEq,Var "n",Int 1),Ass (Var "fac",Int 1),Ass (Var "fac",BopExp (Times,Var "fac",Var "n"))),Ass (Var "n",BopExp (Minus,Var "n",Int 1)))))     , [("n",5)]       )
[("n",1),("fac",120)]

-------

>IMP> trans (E (3+y)+7-9*x , [(x,10),(y,100)])

  -->   EXP 3+y+7-9*x   STATE  [("x",10),("y",100)]

  -->   EXP 3+100+7-9*x   STATE  [("x",10),("y",100)]

  -->   EXP 103+7-9*x   STATE  [("x",10),("y",100)]

  -->   EXP 110-9*x   STATE  [("x",10),("y",100)]

  -->   EXP 110-9*10   STATE  [("x",10),("y",100)]

  -->   EXP 110-90   STATE  [("x",10),("y",100)]

  -->   EXP  20
  
eval (E (3+y)+7-9*x , [(x,10),(y,100)])
