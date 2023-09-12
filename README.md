
# IMP Interpreter 

This program was created using Haskell and the IMP programming language, a simple imperative programming language to gain a better understanding of different programming concepts like; Sequential execution, error handling, AST building, and parser logic. 

The program is a parser for the IMP language, it is designed to read and understand programs written in the IMP language and converted into data that can be processed. Some of these representations can be found below:

Seq: Sequence
BopExp: Boolean expression
eval: Evaluate
alt: or
idr: Identifier
com: Command
iatom: Integer Atom
iexp: Integer expression
alpha: Alphanumeric



## Usage

1. Install GHCup if you havent already and dont forget to add it to your Path variable. ( https://www.haskell.org/ghcup/ )
2. Open VSCode and make sure you have the haskell extension installed.
3. Go into your terminal and make sure your in the root directory of the project.
4. Enter ghci in your terminal and you should now see ghci>.
5. Now load the scripts in using: ":load Main.hs" this should load in all the neccesary files.
6. You can see the different commands and its respective supposed outputs in the ExampleForStudents.hs file to better understand what the program does step by step.
7. To exit out of the IMP use Ctrl+C, and to exit out of ghci use ":q"

## Authors and acknowledgment
Special thanks to my professor Roy L. Crole for his guidance and support during the development of this project as most of the code was
provided I was tasked with finishing the major functions and components of the IMP interpreter.
