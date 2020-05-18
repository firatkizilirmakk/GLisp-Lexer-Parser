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
With the **GLisp **language, **basic programming language requirements** are met like ;

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

	(deffun sumup (x)
    		(if (equal x 0)
    			0
    			(+ x (sumup (- x 1)))
    		)
	)

Here a function named sumup, taking one parameter named x is defined. The function calculates the sum of the numbers up to the positive number x in a recursive form.

The **GLisp** lexer produces the below lexemes for this function definiton.

	(("operator" "(") ("keyword" "deffun") ("identifier" "sumup") ("operator" "(") ("identifier" "x")
	 ("operator" ")") ("operator" "(") ("keyword" "if") ("operator" "(") ("keyword" "equal")
	 ("identifier" "x") ("integer" "0") ("operator" ")") ("integer" "1") ("operator" "(")
	 ("operator" "+") ("identifier" "x") ("operator" "(") ("identifier" "sumup") ("operator" "(")
	 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")") ("operator" ")")
	 ("operator" ")") ("operator" ")") ("operator" ")"))
	
The **GLisp** parser takes these lexemes and produces the parse trees.
