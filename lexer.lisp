;;define constants to be used in the language
;;define keywords and operators as constants
(defun define-const ()
  (defconstant LETTER "letter" "character class letter")
  (defconstant DIGIT "digit" "character class digit")
  (defconstant UNKNOWN "unknown" "character class unknown")

  ;;keywords
  (defconstant KEY_AND "and" "character class keyword")
  (defconstant KEY_OR "or" "character class keyword")
  (defconstant KEY_NOT "not" "character class keyword")
  (defconstant KEY_EQUAL "equal" "character class keyword")
  (defconstant KEY_APPEND "append" "character class keyword")
  (defconstant KEY_CONCAT "concat" "character class keyword")
  (defconstant KEY_SET "set" "character class keyword")
  (defconstant KEY_DEF "deffun" "character class keyword")
  (defconstant KEY_FOR "for" "character class keyword")
  (defconstant KEY_WHILE "while" "character class keyword")
  (defconstant KEY_IF "if" "character class keyword")
  (defconstant KEY_EXIT "exit" "character class keyword")
  
  ;;operators
  (defconstant PLUS_OP "+" "character class operator")
  (defconstant MINUS_OP "-" "character class operator")
  (defconstant DIV_OP "/" "character class operator")
  (defconstant MUL_OP "*" "character class operator")
  (defconstant LEFT_PAREN "(" "character class operator")
  (defconstant RIGHT_PAREN ")" "character class operator")
  (defconstant EXPO_OP "**" "character class operator")
  
  ;;binary values
  (defconstant TRUE_VAL "true" "character class binary value")
  (defconstant FALSE_VAL "false" "character class binary value")
  )

;;take a file name
;;scan the whole file
;;do the lexical analysis according to
;;given grammar
;;return a token list containing every token with its lexeme
(defun lexer (filename)
  (defvar *tokens* nil)
  (defvar *lexemes* nil)
  (setf *lexemes* nil)
  (setf *tokens* nil)
  (define-const)
  (with-open-file (stream filename)
    (let ((content (make-string (file-length stream)))
	  (prevchar-class nil));;previous character stored
      (read-sequence content stream)
      ;;traverse the whole string
      ;;pass the whitespaces and newlines
      (lex content prevchar-class 0)
      ))
  *tokens*
  )

;;traverse the whole string by recursion
;;check if it is newline,tab,or space
;;if not,find the class the charater belongs to
;;store previous character class to compare with current one
;;add lexemes to lexeme list
;;when tokenization required add lexeme with corresponding token
;;to token list and make lexeme list empty
(defun lex (lst-string prevchar-class index)
  (if (< index (length lst-string))
      (let ((char (char lst-string index)))
	(if (and (not (char-equal char #\ )) (not (char-equal char #\Newline )) (not (char-equal char #\Tab)))
	       (let ((char-class (check-lexeme char)))
		 (if (not (string-equal char-class prevchar-class))
		     (if (not (null prevchar-class))
			 (if (not (and (string-equal prevchar-class "MINUS_OP") (string-equal char-class DIGIT)))
			     (get-token)))
		     (if (and (string-equal char-class "RIGHT_PAREN"))
			 (get-token))
		     )
		 (setf *lexemes* (add-last *lexemes* (make-string 1 :initial-element char)));;add charater to lexeme as string
		 (setf prevchar-class char-class)
		 )
	(get-token))
	(lex lst-string prevchar-class (+ index 1))
      )
  )
)

;;compare given string and return corresponding keyword
(defun find-keyword (str)
  (cond ((string-equal str KEY_AND) KEY_AND)
	((string-equal str KEY_OR) KEY_OR)
	((string-equal str KEY_NOT) KEY_NOT)
	((string-equal str KEY_EQUAL) KEY_EQUAL)
	((string-equal str KEY_APPEND) KEY_APPEND)
	((string-equal str KEY_CONCAT) KEY_CONCAT)
	((string-equal str KEY_SET) KEY_SET)
	((string-equal str KEY_DEF) KEY_DEF)
	((string-equal str KEY_FOR) KEY_FOR)
	((string-equal str KEY_WHILE) KEY_WHILE)
	((string-equal str KEY_IF) KEY_IF)	
	((string-equal str KEY_EXIT) KEY_EXIT)
	(t nil)
	)
)


;;;get the current lexeme
;;;check its type
;;;if it is one of the keywords return token keyword
;;;if it is one of the operators return token operator
;;;if it is an integer defined in regexp -> [-]* [1-9]* [0-9]+ return token integer
;;;if it is an identifier consisting of only letters return token identifier
;;;if no condition is satisfied then return token error
(defun get-token ()
  (if (not (null *lexemes*))
      (cond ((not (null (find-keyword (format nil "~{~a~}" *lexemes*))))
	      (setf *tokens* (add-last *tokens* (list "keyword" (format nil "~{~a~}" *lexemes*)))))
	     ((not (null (lookup (format nil "~{~a~}" *lexemes*))))
	      (setf *tokens* (add-last *tokens* (list "operator" (format nil "~{~a~}" *lexemes*)))))
	     ((not (null (is-binaryval (format nil "~{~a~}" *lexemes*))))
	      (setf *tokens* (add-last *tokens* (list "binary-value" (format nil "~{~a~}" *lexemes*)))))	      
	     ((is-integerval *lexemes*)
	      (setf *tokens* (add-last *tokens* (list "integer" (format nil "~{~a~}" *lexemes*)))))	      
	     ((not (null (is-id *lexemes*)))
	      (setf *tokens* (add-last *tokens* (list "identifier" (format nil "~{~a~}" *lexemes*)))))
	     (t (setf *tokens* (add-last *tokens* (list "error" (format nil "~{~a~}" *lexemes*)))))
	     ))
  (setf *lexemes* nil)
  )

;;;check if given string consists of only letters
;;;regexp -> [a-zA-Z]
(defun is-id (lst)
  (cond ((null lst) t)
	((alpha-char-p (char (car lst) 0))
	    (is-id (cdr lst)))))

;;check if given lexeme list contains integer values
;;its regexp -> [-]* [1-9]* [0-9]+
(defun is-integerval (lst)
  (if (or (and (string-equal (car lst) MINUS_OP) (is-num (car (cdr lst)) 0)) (is-num (car lst) -1))
      t))

;;check if given lexeme string consists of
;;string true or false      
(defun is-binaryval (str)
	(cond ((string-equal str TRUE_VAL) TRUE_VAL)
			((string-equal str FALSE_VAL) FALSE_VAL)
			(t nil)))

;;helper func to check number range
(defun is-num (num from)
  (let ((number (char num 0)))
  (cond ((numberp (digit-char-p number))
	 (if (and (< (digit-char-p number) 10) (> (digit-char-p number) from))
	     t)))))

;;;add given elm to end of the lst
(defun add-last (lst elm)
  (if (= (length lst) 0)
      (cons elm nil)
      (cons (car lst) (add-last (cdr lst) elm))))


;;;check given lexeme
;;;return whether it is a letter,digit or any other character
;;;like paranthesis or operators
(defun check-lexeme (next-char)
  (cond ((and (alphanumericp next-char) (not (digit-char-p next-char)))
	 LETTER)
	((digit-char-p next-char)
	 DIGIT)
	(t (lookup next-char))))


;;;looks up if next character is one of the operator
;;;returns its value
;;;TODO:add error case
(defun lookup (next-char)
  (cond ((string-equal next-char PLUS_OP) "PLUS_OP")
	((string-equal next-char MINUS_OP) "MINUS_OP")
	((string-equal next-char DIV_OP) "DIV_OP")
	((string-equal next-char MUL_OP) "MUL_OP")
	((string-equal next-char LEFT_PAREN) "LEFT_PAREN")
	((string-equal next-char RIGHT_PAREN) "RIGHT_PAREN")
	((string-equal next-char EXPO_OP) "EXPO_OP")
	(t nil)
	))
