# GLisp Language |  Common Lisp Lexer & Parser

### Description
***
**GLisp** is an **interpreted, imperative, non object oriented** language. It was given as a project in the Programming Languages lecture in the Gebze Techinal University.

The repository contains the lexer and the parser of the **GLisp** language, details given below, written in the Common Lisp language.
***

#### Syntax
* **Keywords :**
	* and, or, not, equal, append, concat, set, deffun, for, while, if, exit
	
* **Operators :**
	*  +, -, /, *, (, ), ***

* **Terminal Words :**

	* Keywords, Operators
	* Numeric Literals -> [0-9]
	* BinaryValue -> True | False
	* IntegerValue -> [-]\* [1-9]\* [0-9]+
	* Id -> [a-zA-Z]+

* **Non Terminals :**
	* START, INPUT, EXPLISTI, EXPI, EXPB

***
#### BNF Grammar of GLisp Language
Below the syntax of the GLisp languge is given using all of the definitions above.


	START -> INPUT
	INPUT -> EXPI | EXPLISTI

	EXPI -> (+ EXPI EXPI) | (- EXPI EXPI) | (* EXPI EXPI) | (/ EXPI EXPI) | Id | IntegerValue | (Id EXPLISTI)
	EXPB -> (and EXPB EXPB) | (or EXPB EXPB) | (not EXPB) | (equal EXPB EXPB) | (equal EXPI EXPI) | BinaryValue
	EXPLISTI -> (concat EXPLISTI EXPLISTI) | (append EXPI EXPLISTI) | ListValue | Null
	
	LISTVALUE -> ‘(VALUES) | ‘() | Null
	VALUES -> VALUES IntegerValue | IntegerValue 
***
With the **GLisp** language, **basic programming language requirements** are met like ;

* For an assignment ;

	- **EXPI -> (set Id EXPI)**

		An IntegerValue produced by the EXPI is assigned to the Id

* For function definition ;

	- ** EXPI -> (deffun Id IDLIST EXPLISTI)**
	
		A function named Id taking IDLIST parameters and EXPLISTI function body.
	
	- **EXPI -> (Id EXPLISTI)**
	
		Function defined above can be called with this definition
	
* For control statements ;

	- **EXPI -> (if EXBP EXPLISTI)**
	
		A basic if statement is defined like this. 
		A BinaryValue, EXPB, as a condition and the set of statements, EXPLISTI, to be executed.
	
	- **EXPI -> (if EXBP EXPLISTI EXPLISTI)**
		
		A statement with if - else situation.
		A BinaryValue, EXPB, as a condition and the set of statements, EXPLISTI, to be executed depending on the condition.

* For loop statements ;

	- **EXPI -> (while (EXPB) EXPLISTI)
		**
		A basic while loop. Looping until the EXPB is false, executing the EXPLISTI.
	
	- **EXPI -> (for (Id EXPI EXPI) EXPLISTI)**
		
		A basic for loop. Looping until the condition is false, executing the EXPLISTI.
	
***

#### Examples

Below given an example function definiton and then its lexemes and the parse tree results.

	(deffun factorial (x)
		(if (eq x 0)
			1
			(* x (factorial (- x 1)))
		)
	)

The function calculates the factorial of the given number recursively and returns it.

The **GLisp** lexer produces the below lexemes for this function definiton.

	(("operator" "(") ("keyword" "deffun") ("identifier" "factorial") ("operator" "(") ("identifier" "x")
	 ("operator" ")") ("operator" "(") ("keyword" "if") ("operator" "(") ("identifier" "eq")
	 ("identifier" "x") ("integer" "0") ("operator" ")") ("integer" "1") ("operator" "(")
	 ("operator" "*") ("identifier" "x") ("operator" "(") ("identifier" "factorial") ("operator" "(")
	 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")") ("operator" ")")
	 ("operator" ")") ("operator" ")") ("operator" ")"))


	
The **GLisp** parser takes these lexemes and produces the parse trees as below.

	"START
	     INPUT
		 EXPI
		     (
		     deffun
		     ID
		         factorial
		     IDLIST
		         (
		         IDLIST
		             ID
		                 x
		         )
		     EXPLISTI
		         EXPI
		             (
		             if
		             EXPB
		                 (
		                 ID
		                     equal
		                 EXPI
		                     ID
		                         x
		                 EXPI
		                     VALUES
		                         IntegerValue
		                             0
		                 )
		             EXPLISTI
		                 EXPI
		                     VALUES
		                         IntegerValue
		                             1
		             EXPLISTI
		                 EXPI
		                     (
		                     *
		                     EXPI
		                         ID
		                             x
		                     EXPI
		                         (
		                         ID
		                             factorial
		                         EXPLISTI
		                             EXPI
		                                 (
		                                 -
		                                 EXPI
		                                     ID
		                                         x
		                                 EXPI
		                                     VALUES
		                                         IntegerValue
		                                             1
		                                 )
		                         )
		                     )
		             )
		     )"

***
#### Installation

In order to use the GLisp ***parser***, for now, **clisp repl tool** must be installed.

*	For GNU/Linux, "sudo apt-get install clisp".
*	For Windows, https://sourceforge.net/projects/clisp/

After installing the **clisp**, open its terminal based repl ;

1. 	Load the lexer using **(load "lexer.lisp")**
2.	Call GLisp lexer using **(lexer "input.txt")**, where the GLisp script is written. The GLisp lexer will produce the list of lexemes.
3.	After retrieving the lexemes, load the parser using **(load "parser.lisp")**
4.	Then call the parser with the lexemes produced by the lexer using **(parser '(lexemes))**
5. 	The GLisp parser will then produce the corresponding parse tree.

***
#### TODO
1. GLisp project needs an evaluator to produce the results of the written GLisp scripts.
2. The project needs to be compact, used as a one tool.