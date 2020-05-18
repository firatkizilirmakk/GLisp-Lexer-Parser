;;; define non terminals of the grammar
(defun define-consts()
  (defconstant CONST_OPERATOR "operator" "token type 1")
  (defconstant CONST_KEYWORD "keyword" "token type 2")
  (defconstant CONST_IDENTIFIER "identifier" "token type 3")
  (defconstant CONST_INTEGER "integer" "token type 4")
  (defconstant CONST_BOOLEAN "binary" "token type 5")
  (defconstant CONST_NULL "null" "token type 6")
  (defconstant CONST_OP_NOT_EVAL "'(" "operator type 0")
  (defconstant CONST_OP_LEFT_PAREN "(" "operator type 1")
  (defconstant CONST_OP_RIGHT_PAREN ")" "operator type 2")
  (defconstant CONST_OP_SUM "+" "operator type 3")
  (defconstant CONST_OP_SUB "-" "operator type 4")
  (defconstant CONST_OP_MUL "*" "operator type 5")
  (defconstant CONST_OP_DIV "/" "operator type 6")
  (defconstant CONST_KEY_AND "and" "keyword type 0")
  (defconstant CONST_KEY_OR "or" "keyword type 1")
  (defconstant CONST_KEY_NOT "not" "keyword type 2")
  (defconstant CONST_KEY_EQ "equal" "keyword type 3")
  (defconstant CONST_KEY_CONCAT "concat" "keyword type 4")
  (defconstant CONST_KEY_APPEND "append" "keyword type 5")
  (defconstant CONST_KEY_SET "set" "keyword type 6")
  (defconstant CONST_KEY_DEFFUN "deffun" "keyword type 7")
  (defconstant CONST_KEY_DEFVAR "defvar" "keyword type 8")
  (defconstant CONST_KEY_IF "if" "keyword type 9")
  (defconstant CONST_KEY_WHILE "while" "keyword type 10")
  (defconstant CONST_KEY_FOR "for" "keyword type 11")
  (defconstant CONST_KEY_NULL "null" "keyword type 11")
  (defconstant SYM_ERROR "error" "error symbol 1")
  (defconstant SYM_START "START" "start symbol nonterminal 2")
  (defconstant SYM_INPUT "INPUT" "input symbol nonterminal 3")
  (defconstant SYM_EXPI "EXPI" "expi symbol nonterminal 4")
  (defconstant SYM_EXP_LISTI "EXPLISTI" "expi_list symbol nonterminal 5")
  (defconstant SYM_EXPB "EXPB" "expb symbol nonterminal")
  (defconstant SYM_VALUES "VALUES" "values symbol nonterminal")
  (defconstant SYM_ID_LIST "IDLIST" "idlist symbol nonterminal")
  (defconstant SYM_ID "ID" "ID symbol nonterminal")
  (defconstant SYM_OP "operator" "op symbol nonterminal")
  (defconstant SYM_INTEGER "IntegerValue" "op symbol nonterminal")
  (defconstant SYM_BOOLEAN "BinaryValue" "op symbol nonterminal")
  )

;;; get the next token according to
;;; token counter
(defun get-next-token (tokens index)
  (if (>= index (length tokens))
      nil
      (if (= index 0)
	  (get-token tokens)
	  (get-next-token (cdr tokens) (- index 1)))))
;;; get the token and increment
;;; counter
(defun get-token (tokens)
  (setf *token-count* (+ *token-count* 1))
  (car tokens))

;;get the token type
;;state whether it is operator,keyword etc
(defun get-token-type(token)
  (cond ((string-equal (car token) CONST_OPERATOR)
	 CONST_OPERATOR)
	((string-equal (car token) CONST_KEYWORD)
	  CONST_KEYWORD)
	 ((string-equal (car token) CONST_IDENTIFIER)
	  CONST_IDENTIFIER)
	 ((string-equal (car token) CONST_INTEGER)
	  CONST_INTEGER)
	 ((string-equal (car token) CONST_NULL)
	  CONST_NULL)
	 ((string-equal (car token) CONST_BOOLEAN)
	  CONST_BOOLEAN)
	 (t nil))
	)

;;get the operator type
;;state whether it is left paranthesis,sum etc
(defun get-operator-type(operator)
  (cond ((string-equal operator CONST_OP_LEFT_PAREN)
	 CONST_OP_LEFT_PAREN)
	((string-equal operator CONST_OP_RIGHT_PAREN)
	 CONST_OP_RIGHT_PAREN)
	((string-equal operator CONST_OP_SUM)
	 CONST_OP_SUM)
	((string-equal operator CONST_OP_SUB)
	 CONST_OP_SUB)
	((string-equal operator CONST_OP_MUL)
	 CONST_OP_MUL)
	((string-equal operator CONST_OP_DIV)
	 CONST_OP_DIV)
	((string-equal operator CONST_OP_NOT_EVAL)
	 CONST_OP_NOT_EVAL)
	(t nil)
	))

;;get the keyword type
;;state whether it is and,or etc
(defun get-keyword-type(keyword)
  (cond ((string-equal keyword CONST_KEY_AND)
	 CONST_KEY_AND)
	((string-equal keyword CONST_KEY_OR)
	 CONST_KEY_OR)
	((string-equal keyword CONST_KEY_NOT)
	 CONST_KEY_NOT)
	((string-equal keyword CONST_KEY_EQ)
	 CONST_KEY_EQ)
	((string-equal keyword CONST_KEY_CONCAT)
	 CONST_KEY_CONCAT)
	((string-equal keyword CONST_KEY_APPEND)
	 CONST_KEY_APPEND)
	((string-equal keyword CONST_KEY_SET)
	 CONST_KEY_SET)
	((string-equal keyword CONST_KEY_DEFFUN)
	 CONST_KEY_DEFFUN)
	((string-equal keyword CONST_KEY_DEFVAR)
	 CONST_KEY_DEFVAR)
	((string-equal keyword CONST_KEY_IF)
	 CONST_KEY_IF)
	((string-equal keyword CONST_KEY_WHILE)
	 CONST_KEY_WHILE)
	((string-equal keyword CONST_KEY_FOR)
	 CONST_KEY_FOR)
	((string-equal keyword CONST_KEY_NULL)
	 CONST_KEY_NULL)
	(t nil)
	))

;;true if given token is associated with arithmetic operation
(defun is-arith-op (token)
  (let ((op-type (get-operator-type (car (cdr token)))))
    (if (or (string-equal op-type CONST_OP_SUM) (string-equal op-type CONST_OP_SUB)
	    (string-equal op-type CONST_OP_DIV) (string-equal op-type CONST_OP_MUL))
	t
	nil
	)))

;;true if given token is associated with boolean key
(defun is-bool-key (token)
  (let ((key-type (get-keyword-type (car (cdr token)))))
    (if (or (string-equal key-type CONST_KEY_AND) (string-equal key-type CONST_KEY_OR)
	    (string-equal key-type CONST_KEY_NOT) (string-equal key-type CONST_KEY_EQ))
	    t
	    nil)))

;;true if given token is left paranthesis
(defun is-left-paren-op (token)
  (if (string-equal (get-token-type token) CONST_OPERATOR)
      (if (string-equal (get-operator-type (car (cdr token))) CONST_OP_LEFT_PAREN)
	  t)))

;;true if given token is right paranthesis
(defun is-right-paren-op (token)
  (if (string-equal (get-token-type token) CONST_OPERATOR)
      (if (string-equal (get-operator-type (car (cdr token))) CONST_OP_RIGHT_PAREN)
	  t)))




;;call explist two times for concat explisti explisti
(defun make-one-explisti (sub-tree next-token tokens space)
  (setf sub-tree (add-child sub-tree (sym-keyword next-token space) space))
  (setf sub-tree (expi tokens sub-tree space))
  (setf sub-tree (explisti tokens sub-tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child sub-tree (sym-operator token space) space)
	(add-child sub-tree SYM_ERROR space))
    ))


;;call explist two times for( concat explisti explisti)
(defun make-two-explisti (sub-tree next-token tokens space)
  (setf sub-tree (add-child sub-tree (sym-keyword next-token space) space))
  (setf sub-tree (explisti tokens sub-tree space))
  (setf sub-tree (explisti tokens sub-tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child sub-tree (sym-operator token space) space)
	(add-child sub-tree SYM_ERROR space))
	))

;;;if after ( ,values comes or append or concat comes  
(defun make-values (sub-tree token tokens space)
  (if (string-equal (get-token-type token) CONST_INTEGER)
      (setf *token-count* (- *token-count* 1))
      (setf sub-tree (add-child sub-tree (sym-operator token space) space)))
  (let ((next-token (get-next-token tokens *token-count*)))
    (if (is-explisti-keyword next-token)
	(if (string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_CONCAT)	;;(concat) (append)
	    (setf sub-tree (make-two-explisti sub-tree next-token tokens space))
	    (setf sub-tree (make-one-explisti sub-tree next-token tokens space)))
	(setf sub-tree (call-get-values sub-tree tokens space))));;'(values)
  sub-tree)

;;calls get-values for explisti
(defun call-get-values (sub-tree tokens space)
  (setf *token-count* (- *token-count* 1))
  (get-values sub-tree tokens space)
  )

;;for values in explisti
(defun get-values (sub-tree tokens space)
  (let ((token (get-next-token tokens *token-count*)))
    (cond ((is-right-paren-op token )
	   (add-child sub-tree (sym-operator token space) space ))
	  ((string-equal (get-token-type token) CONST_INTEGER)
	   (setf sub-tree (get-values (add-child sub-tree (sym-values token space) space) tokens space)))
	  (t (add-child sub-tree SYM_ERROR space))
	  )))

;;called for explisti parsing processes
;;expects as starting with ( or '
;; ' is accepted as a seperate keyword
(defun explisti (tokens tree space)
  (let ((token (get-next-token tokens *token-count*)))
        (setf tree (add-child tree SYM_EXP_LISTI space))
    (cond ((string-equal (get-operator-type (car (cdr token))) CONST_OP_NOT_EVAL)
	   (let ((next-token (get-next-token tokens *token-count*)));;;starts with '(
	     (setf tree (add-child tree (sym-operator token (+ space 1)) (+ space 1)))
	     (cond ((is-right-paren-op next-token)
		    (setf tree (add-child tree (sym-operator next-token (+ space 1)) (+ space 1) )));; '()
		    ((string-equal (get-token-type next-token) CONST_INTEGER)
		     (setf tree (make-values tree next-token tokens (+ space 1))))
		    (t
		     (setf tree (add-child tree SYM_ERROR (+ space 1))))  ;; error 'not left)
		   )))
	  ((and (string-equal (get-operator-type (car (cdr token))) CONST_OP_LEFT_PAREN )  (is-next-token-val-or-key tokens))
	   (setf tree (make-values tree token tokens (+ space 1))))
	  ((string-equal (get-keyword-type (car (cdr token))) CONST_KEY_NULL)
	   (setf tree (add-child tree (sym-keyword token (+ space 1)) (+ space 1))))
	  (t (setf tree (call-expi-from-explist tokens tree space))))
    )
  tree)


;;calls expi for explisti
(defun call-expi-from-explist (tokens tree space)
  (setf *token-count* (- *token-count* 1))
  (expi tokens tree (+ space 1))
     )
;;returns true if next token is integer value or explisti keyword
(defun is-next-token-val-or-key (tokens)
  (let ((token (get-next-token tokens *token-count*)))
    (setf *token-count* (- *token-count* 1))
    (if (or (string-equal (get-token-type token) CONST_INTEGER) (is-explisti-keyword token))
	t
	nil
	)
    )
  )

;;called by start function
;;stating input symbol
;;decides where to go from input
(defun input (tokens tree space)
  (let ((token (get-next-token tokens *token-count*)))
    (setf tree (add-child tree SYM_INPUT space))
    (setf *token-count* (- *token-count* 1))
    (cond ((is-left-paren-op token)
	   (setf *token-count* (+ *token-count* 1))
	   (let ((next-token (get-next-token tokens *token-count*)))
	     (setf *token-count* (- *token-count* 2))
	     (cond ((is-explisti-keyword next-token)
		    (setf tree (explisti tokens tree (+ space 1))))
		   (t
		    (setf tree (expi tokens tree (+ space 1))));;call expi by decreasing token count 2,starting from (
		   )))
	  ((or (string-equal (get-token-type token) CONST_IDENTIFIER) (string-equal (get-token-type token) CONST_INTEGER))
	   (setf tree (expi tokens tree (+ space 1))))
	  ((string-equal (get-operator-type (car (cdr token))) CONST_OP_NOT_EVAL)
	   (setf tree (explisti tokens tree (+ space 1))))
	  ((string-equal (get-keyword-type (car (cdr token))) CONST_KEY_NULL)
	   (setf tree (explisti tokens tree (+ space 1))))))
  tree)


;;symbol keyword returns the keyword
(defun sym-keyword (token space)
  (car (cdr token)))

;;symbol operator returns the operator 
(defun sym-operator (token space)
  (car (cdr token))
  )
;;symbol values returns as values -> integer value -> value
(defun sym-values (token space)
  (add-child (add-child (format nil "~a" SYM_VALUES) (format nil "~a" SYM_INTEGER) (+ space 1)) (car (cdr token)) (+ space 2) ))

;;symbol binary return binary value -> value
(defun sym-binary (token space)
    (add-child (format nil "~a" SYM_BOOLEAN) (car (cdr token)) (+ space 1) ))

;;symbol id return id and identifier itself
(defun sym-id (token space)
  (add-child (format nil "~a" SYM_ID) (car (cdr token)) (+ space 1) )
  )



;;checks whether given token is one of the expi keyword
;;such as if,deffun
(defun is-expi-keyword (token)
  (let ((key-type (get-keyword-type (car (cdr token)))))
    (if (or (string-equal key-type CONST_KEY_SET) (string-equal key-type CONST_KEY_DEFFUN)
	    (string-equal key-type CONST_KEY_DEFVAR) (string-equal key-type CONST_KEY_IF) (string-equal key-type CONST_KEY_WHILE) (string-equal key-type CONST_KEY_FOR))
	t
	nil
	)))

;;checks whether given token is one of the explisti keyword
;;such as concat,append
(defun is-explisti-keyword (token)
  (let ((key-type (get-keyword-type (car (cdr token)))))
    (if (or (string-equal key-type CONST_KEY_CONCAT) (string-equal key-type CONST_KEY_APPEND))
	t
	nil
	)))

;;check if given token is identifier
(defun is-id (token)
  (string-equal (car token) CONST_IDENTIFIER))

;;(set id expi) or (defvar id expi)
(defun one-id-expi (tree tokens space)
  (let ((next-token (get-next-token tokens *token-count*)))
    (if (is-id next-token)
	(setf tree (one-id-expi-handler tree next-token tokens space))
	(add-child tree SYM_ERROR space)
    )))

;;called by one-id-expi
(defun one-id-expi-handler (tree next-token tokens space)
  (setf tree (add-child tree (sym-id next-token space) space))
  (setf tree (expi tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child tree (sym-operator token space) space)
	(add-child tree SYM_ERROR space))
    )
  )

;;;called for (deffun ID IDLIST EXPLISTI)
(defun one-idlist-explisti (tree tokens space)
  (let ((next-token (get-next-token tokens *token-count*)))
    (if (is-id next-token)
	(setf tree (one-idlist-explisti-handler tree next-token tokens space))
	(add-child tree SYM_ERROR space)
	)))

;;;called by one-idlist-explisti
(defun one-idlist-explisti-handler (tree next-token tokens space)
  (setf tree (add-child tree (sym-id next-token space) space));;;add id
  (setf tree (ident tokens tree space 0));;idlist
  (setf tree (explisti tokens tree space));;explisti
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child tree (sym-operator token space) space)
	(add-child tree SYM_ERROR space))
    )  
  )


;;;after discovering there is one more explist after if expb explist
;;;check for last paranthesis
(defun handle-last-explisti (tokens tree space)
  (setf tree (explisti tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child tree (sym-operator token space) space)
	(add-child tree SYM_ERROR space)))
  )
;; (if expb explist) or (if expb explisti explisti)
(defun expi-if (tree tokens space)
  (setf tree (expb tokens tree space))
  (setf tree (explisti tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (setf *token-count* (- *token-count* 1))
    (if (is-right-paren-op token)
	(add-child tree (sym-operator token space) space)
	(setf tree (handle-last-explisti tokens tree space))))
  )
;;for checking if next tokens state coming
;;tokens would be explisti
(defun is-explisti (tokens token)
  (cond ((string-equal (get-token-type token) CONST_NULL)
	 (setf *token-count* (- *token-count* 1)));;next token null
	((string-equal (get-operator-type (car (cdr token))) CONST_OP_NOT_EVAL)
	 (setf *token-count* (- *token-count* 1)));;next token '
	((string-equal (get-operator-type (car (cdr token))) CONST_OP_LEFT_PAREN);;look for next token
	 (let ((next-token (get-next-token tokens *token-count*)))
	   (if (is-explisti-keyword next-token)
	       (setf *token-count* (- *token-count* 2))
	       nil))
	 )
	((string-equal (get-token-type token) CONST_INTEGER)
	 (setf *token-count* (- *token-count* 1)))
	((string-equal (get-token-type token) CONST_IDENTIFIER)
	 (setf *token-count* (- *token-count* 1)))
	(t nil))
  )

;;(while EXPB EXPLISTI)
(defun expi-while (tree tokens space)
  (setf tree (expb tokens tree space))
  (setf tree (explisti tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if  (not (null (is-explisti tokens token)))
	 (handle-last-explisti tokens tree space)
	 (if (is-right-paren-op token)
	     (add-child tree (sym-operator token space) space)
	     (add-child tree SYM_ERROR space)))
    )
  )

;(for (ID EXPI EXPI) EXPLISTI)
(defun expi-for (tree tokens space)
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-left-paren-op token)
	(setf tree (expi-for-handler tree token tokens space))
	(add-child tree SYM_ERROR space)
	)
    ))
;;called by expi for
(defun expi-for-handler (tree token tokens space)
  (setf tree (add-child tree (sym-operator token space) space));;add (
  (let ((next-token (get-next-token tokens *token-count*)))
      (if (string-equal (get-token-type next-token) CONST_IDENTIFIER)
	  (setf tree (expi-for-last-explisti tree next-token tokens space))
	  (add-child tree SYM_ERROR space)
	)
    )
  )
;;for explist in for expression
(defun expi-for-last-explisti (tree next-token tokens space)
  (setf tree (make-arith-and-for-expi tree next-token tokens space))
  (setf tree (explisti tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if  (not (null (is-explisti tokens token)))
	 (handle-last-explisti tokens tree space)
	 (if (is-right-paren-op token)
	     (add-child tree (sym-operator token space) space)
	     (add-child tree SYM_ERROR space)))
    )
  )
;;;called when expi encounters an expi key
(defun expi-key-handler(tree next-token tokens space)
  (setf tree (add-child tree (sym-keyword next-token space) space))
  (cond ((or (string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_SET) (string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_DEFVAR))
	 (setf tree (one-id-expi tree tokens space)));;;(set ID EXPI) or (defvar ID EXPI)
	((string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_IF);;; (if EXPB EXPLISTI) 
	 (setf tree (expi-if tree tokens space)))
	((string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_DEFFUN)
	 (setf tree (one-idlist-explisti tree tokens space)));;(deffun ID IDLIST EXPLISTI)
	((string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_WHILE);(while (EXPB) EXPLISTI)
	 (setf tree (expi-while tree tokens space)))
	((string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_FOR);(for (ID EXPI EXPI) EXPLISTI)
	 (setf tree (expi-for tree tokens space)))
	)
  tree)


;;all expi rules are parsed through this function
(defun expi (tokens tree space)
  (let ((token (get-next-token tokens *token-count*)))
    (setf tree (add-child tree SYM_EXPI space))
    (if (not (is-left-paren-op token))
	(setf *token-count* (- *token-count* 1));;if not left paren look it again
	(setf tree (add-child tree (sym-operator token (+ space 1)) (+ space 1))));;if left paren add to tree
    (let ((next-token (get-next-token tokens *token-count*)))
      (cond ((is-arith-op next-token)
	     (setf tree (make-arith-and-for-expi tree next-token tokens (+ space 1))))
	    ((is-expi-keyword next-token)
	     (setf tree (expi-key-handler tree next-token tokens (+ space 1)))) ;; expi keyword like set,deffun,defvar
	    ((string-equal (get-token-type next-token) CONST_INTEGER);;if only integer
	     (setf tree (add-child tree (sym-values next-token (+ space 1)) (+ space 1))))
	    ((string-equal (get-token-type next-token) CONST_IDENTIFIER)
	     (if (is-left-paren-op token)
		 (setf tree (one-id-explist tree next-token tokens (+ space 1)))
		 (setf tree (add-child tree (sym-id next-token (+ space 1)) (+ space 1))));;if only identifier
	     )
	    ))
    tree
    )
)
  
;;(ID EXPLISTI)
(defun one-id-explist (tree next-token tokens space)
  (setf tree (add-child tree (sym-id next-token space ) space))
  (setf tree (explisti tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child tree (sym-operator token space) space)
	(add-child tree SYM_ERROR space))
    )
  )
;(+ EXPI EXPI) , (- EXPI EXPI) , (* EXPI EXPI) , (/ EXPI EXPI)
;(for (ID EXPI EXPI) EXPLISTI)
(defun make-arith-and-for-expi (tree next-token tokens space)
  (if (string-equal (get-token-type next-token) CONST_OPERATOR)
      (setf tree (add-child tree (sym-operator next-token space ) space));;operator
      (if (string-equal (get-token-type next-token) CONST_KEYWORD)
	  (setf tree (add-child tree (sym-keyword next-token space ) space));;keyword
	  (setf tree (add-child tree (sym-id next-token space ) space));;identifier
      ))
  (setf tree (expi tokens tree space))
  (setf tree (expi tokens tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child tree (sym-operator token space) space)
	(add-child tree SYM_ERROR space))
    )
  )


;;;parses idlist
(defun ident (tokens tree space count)
  (let ((token (get-next-token tokens *token-count*)))
    (setf tree (add-child tree SYM_ID_LIST space))
    (cond ((is-left-paren-op token);;if starts with (
	   (let ((next-token (get-next-token tokens *token-count*)))
	     (setf *token-count* (- *token-count* 1))
	     (if (is-right-paren-op next-token);;if ( and ) comes
		 (if (= count 0)
		     (setf tree (add-child tree SYM_ERROR space))
		     (setf tree (add-child tree (sym-operator next-token space) space ));;add )
		     )
		 (setf tree (id-list tokens token tree (+ space 1) (+ count 1))))))
	  ((string-equal (get-token-type token) CONST_IDENTIFIER)
	   (setf tree (id-list tokens token tree (+ space 1) (+ count 1)))))
	  )
    )
;;called by ident and call ident
(defun id-list (tokens token tree space count)
  (if (string-equal (get-token-type token) CONST_IDENTIFIER)
      (setf tree (add-child tree (sym-id token space) space))
      (setf tree (add-child tree (sym-operator token space) space)))
  (let ((next-token (get-next-token tokens *token-count*)))
    (setf *token-count* (- *token-count* 1))
    (if (string-equal (get-token-type next-token) CONST_IDENTIFIER)
	(setf tree (ident tokens tree space count))
	(if (is-right-paren-op next-token)
	    (setf tree (id-list-right-paren tokens tree next-token space count))
	    tree))
    )
  )

;;for id list right paren checking
(defun id-list-right-paren (tokens tree next-token space count)
  (setf *token-count* (+ *token-count* 1))
  (add-child tree (sym-operator next-token (- space (- count 1))) (- space (- count 1)))
  )

;;for ids in explisti
(defun get-ids (sub-tree tokens space)
  (let ((token (get-next-token tokens *token-count*)))
    (cond ((is-right-paren-op token)
	   (add-child sub-tree (sym-operator token space) space ))
	  ((string-equal (get-token-type token) CONST_IDENTIFIER)
	   (setf sub-tree (get-ids (add-child sub-tree (sym-id token space) space) tokens space)))
	  (t (add-child sub-tree SYM_ERROR space))
	  )    
    ))


;;called when there are values
(defun get-values (sub-tree tokens space)
  (let ((token (get-next-token tokens *token-count*)))
    (cond ((is-right-paren-op token )
	   (add-child sub-tree (sym-operator token space) space ))
	  ((string-equal (get-token-type token) CONST_INTEGER)
	   (setf sub-tree (get-values (add-child sub-tree (sym-values token space) space) tokens space)))
	  (t (add-child sub-tree SYM_ERROR space))
	  )))

;;called (and EXPB EXPB) (or EXPB EXPB) (equal EXPB EXPB)
(defun make-two-expb (sub-tree next-token tokens space)
  (setf sub-tree (add-child sub-tree (sym-keyword next-token space) space))
  (setf sub-tree (expb tokens sub-tree space))
  (setf sub-tree (expb tokens sub-tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child sub-tree (sym-operator token space) space)
	(add-child sub-tree SYM_ERROR space))
	))

;(equal EXPB EXPB)
(defun make-one-expb (sub-tree next-token tokens space)
  (setf sub-tree (add-child sub-tree (sym-keyword next-token space) space))
  (setf sub-tree (expb tokens sub-tree space))
  (let ((token (get-next-token tokens *token-count*)))
    (if (is-right-paren-op token)
	(add-child sub-tree (sym-operator token space) space)
	(add-child sub-tree SYM_ERROR space)
	)))


;;for (equal EXPI EXPI) or (equal EXPB EXPB)
(defun make-expb-or-expi (tree next-token tokens space)
  (if (is-next-expi tokens)
      (setf tree (make-arith-and-for-expi tree next-token tokens space))
      (setf tree (make-two-expb tree next-token tokens space))))

;;to make next parsing for expi or expb
(defun is-next-expi (tokens)
  (let ((token (get-next-token tokens *token-count*)))
  (setf *token-count* (- *token-count* 1))
  (cond ((is-left-paren-op token)
	 (let ((next-token (get-next-token tokens *token-count*)))	 ;;look one more token
	   (setf *token-count* (- *token-count* 1))
	   (if (or (is-bool-key next-token) (string-equal (car next-token) CONST_BOOLEAN))
	       nil
	       t)
	   ))
	((string-equal (car token) CONST_BOOLEAN)
	 nil)
	(t t))
	))

;parses all expbs
(defun expb (tokens tree space)
  (let ((token (get-next-token tokens *token-count*)))
    (setf tree (add-child tree SYM_EXPB space))
    (if (is-left-paren-op token)
	(setf tree (add-child tree (sym-operator token (+ space 1)) (+ space 1)))
	(setf *token-count* (- *token-count* 1)));;not starts with left paren,look again
    (let ((next-token (get-next-token tokens *token-count*)));starts with left paren,keep parsing
      (cond ((or (string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_AND)
		 (string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_OR))
	     (setf tree (make-two-expb tree next-token tokens (+ space 1))))
	    ((string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_NOT)
	     (setf tree (make-one-expb tree next-token tokens (+ space 1))))
	    ((string-equal (get-keyword-type (car (cdr next-token))) CONST_KEY_EQ)
	     (setf tree (make-expb-or-expi tree next-token tokens (+ space 1))))
	    ((string-equal (get-token-type next-token) CONST_BOOLEAN)
	     (setf tree (add-child tree (sym-binary next-token (+ space 1)) (+ space 1))))
	    ))
        tree)
  )

;main function
;;calls input and parses whole input
(defun parser(tokens)
  (defvar *token-count* 0);;for getting next token
  (define-consts)
  (let ((parse-tree (format nil "~a" SYM_START)))
    (setf parse-tree (input tokens parse-tree 1))
    (setf *token-count* 0)
    (write-to-file '"151044073.tree" parse-tree)
    parse-tree
    )
  )

;;writes to file
(defun write-to-file (name content)
  (setf content (concatenate 'string (format nil "; DIRECTIVE: parse tree~%") content))
    (with-open-file (stream name
        :direction :output
        :if-exists :supersede
        :if-does-not-exist :create)
    (format stream content)))


;;add child to the tree according to given space
(defun add-child (tree child space)
  (concatenate 'string (format nil "~a~%" tree) (concatenate 'string (add-space (+ (* space 4) 1)) child)))

;;add space to tree to state child relationship  according to space
(defun add-space (space)
  (if (= space 0)
      (concatenate 'string "" "")
      (concatenate 'string " " (add-space (- space 1)))
      ))
