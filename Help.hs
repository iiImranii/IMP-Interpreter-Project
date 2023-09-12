 
---------------------------------------------------------------------
-- HASKELL HELP FILE FOR IMP              
-- Time-stamp: <2022-12-08 10:13:22 >                              
                                        
---------------------------------------------------------------------

module Help where

introduction = (putStr . unlines) [

 "\n ",
 "                           I         M              M         PPPP                   ",
 "                           I         MM           M M         P    P                 ",
 "                           I         M  M       M   M         P     P                ",
 "                           I         M    M   M     M         P     P                ",
 "                           I         M      M       M         P    P                 ",
 " Welcome to the world of   I         M              M         PPPP                   ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",
 "                           I         M              M         P                      ",

 "\n Copyright (c) Leicester University, 2023 ",

 "\n Please type help for help    ...    recursion is everywhere  ... " ]


help 0 = (putStr . unlines) [

 "\n please type 'help 1' for a description of IMP ",

 "\n please type 'help 2' for the BNF grammars constituting IMP programs",

 "\n please type 'help 3' to see some example programs",

 "\n That's it folks! You're on yer own now ...." ]


help 1 = (putStr . unlines) [

 "\n This Haskell program parses and interprets a language IMP.",

 " The parser is combinatory. The interpreter is a coding of both evaluation", 
 " and transition style operational semantics",

 "\n An IMP program takes the form:",

 "\n      (<code>, <state>)  ",

 "\n The semantics take the form", 
 
 "\n evaluation  (<code>, <state>) ==>  <state>",

 "\n transition  (<code>, <state>) --> (<code>, <state>)",

 "\n where ==> is the same as -->^*",

 "\n See help 2 for BNF grammars"
 ]


help 2 = (putStr . unlines) [
  
  "\n The IMP grammar for state <s> is",

  "\n <s> ::= [(<v1>, <z1>) , ... , (<vk>,<zk>)] ", 

  "\n The IMP grammar for program <p> is",

  "\n <p> ::= (<code>,<s>)", 

  "\n The IMP grammar for code <code> is",

  "\n <code> ::= C <c> | E <e>  ", 

  "\n The IMP grammar for command <c> and expression <e> is",

  "\n <c> ::= skip | <v> := <e> | c ; c | if <e> then c else c | while <e> do c ",
  "         true | false | <z> | <v> | <e> + <e> | <e> - <e> | <e> * <e> | <e> = <e> | <e> <= <e> | <e> >= <e> | <e> < <e> | <e> > <e>",

  "\n where v : AlphaString , z : Int "]


help 3 = (putStr . unlines) [

   "\n PLEASE TYPE IN EITHER",

   "\n eval <p> OR trans <p>",

   "\n for example",
   
   "\n eval (C x:=4+5+6,[])",

   "\n trans (E (4+n)+6,[(n,-1)])", 

   "\n FOLLOWED BY 'hit return', where <p> is defined in help 2",

   "\n more examples can be found in Examples.hs"]





