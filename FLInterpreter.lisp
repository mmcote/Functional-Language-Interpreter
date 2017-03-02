; CMPUT 325 Assignment 02
; Michael Cote (ccid: mmcote)

; Argument Counters ----------------------------------------------------------
; Which are used to compare expressions and user defined functions. These are
; needed to differentiate function signatures as they can share similar
; function names.
;
; expression_arg_num is used to determine the number of 
; arguments given in a expression
; Test
; (expression_arg_num '(1 2 3))
; = 3
(defun expression_arg_num (args)
    (expression_arg_counter args 0)
)

(defun expression_arg_counter (args count)
    (cond
        ((null args) count)
        (T (+ (expression_arg_counter (cdr args) count) 1))
    )
)

; header_arg_num is used to determine the number of
; arguments given in a header
; Test
; (function_definition_arg_num '(f X Y Z = (+ X Y Z)))
; = 3
(defun function_definition_arg_num (function_definition)
    (header_arg_counter (cdr function_definition) 0)
)

(defun header_arg_counter (header count)
    (cond
        ((eq (car header) '=) count)
        (T (+ (header_arg_counter (cdr header) count) 1))
    )
)


; get functions --------------------------------------------------------------
; used to abstract different portions of user defined functions
;
; get_body is used to seperate body of a function definition
; Test
; (get_body '(f X Y Z = (+ X Y Z)))
; = (+ X Y Z)
(defun get_body (function_definition)
    (cond
        ((null function_definition) Nil)
        ((eq (car function_definition) '=) (cadr function_definition))
        (T (get_body (cdr function_definition)))
    )
)

; get_args is used to seperate arguments of a function definition
; Test
; (get_args '(f X Y Z = (+ X Y Z)))
; = (X Y Z)
(defun get_args (function_definition)
    (seperate_args (cdr function_definition))
)

(defun seperate_args (header)
    (cond
        ((eq (car header) '=) NIL)
        (T (cons (car header) (seperate_args (cdr header)) ))
    )
)


; user_function_exists -------------------------------------------------------
; simply checks that there is a matching function signature
; (defined simply in this case by the name and number of arguments)
; Test
; (user_function_exists 'h 3 '( (h X = (+ X 1)) (g J K = (/ K J)) (h X Y Z = (* X Y Z))))
; ((Z Y X) (* X Y Z))
(defun user_function_exists (function num_args given_functions)
    (cond
        ((null given_functions) Nil)
        ((and (eq function (caar given_functions)) (eq num_args (function_definition_arg_num (car given_functions))))
            (list (get_args (car given_functions)) (get_body (car given_functions)))
        )
        (T (user_function_exists function num_args (cdr given_functions)))
    )
)


; AOR ------------------------------------------------------------------------
; Applicative order reduction (innermost-leftmost)
(defun AOR (E P context)
    (if (not (null E)) 
        (cons (fl-interp-context (car E) P context) (AOR (cdr E) P context))
        NIL
    )  
)

; This will return the value of a given name from the associated tuple in the
; context
(defun get_value_from_context (param context)
	(cond
   		((null context) (list NIL NIL))
        (T (let ((pair (car context))) 
            (cond 
                ((equal param (car pair)) (list T (cadr pair)))
                (T (get_value_from_context param (cdr context)))
            )
        ))
    )
)


; Creates a new context that merges both the names and values list into
; individual tuples
(defun generate_context (params values)
    (if (not (or (null params) (null values)))
        (cons (list (car params) (car values)) (generate_context (cdr params) (cdr values)))
        NIL
    )
)

; This is the originally called fun language interpreter, as can be seen we 
; start with an empty context and extend it from each user defined function.
(defun fl-interp (E P)
    (fl-interp-context E P NIL)
)

(defun fl-interp-context (E P context)
    (let ((value_from_context (get_value_from_context E context)))
        (cond
            ; the given expression may have its binded value in the context passed
            ((car value_from_context) (cadr value_from_context))
            ((atom E) E)   ; this includes the case where expr is nil
            (T
                (let ((f (car E)) (arg (cdr E)))
                    (cond 
                        ; handle primitive built-in functions
                        (
                            (or 
                                (eq f '+) (eq f '-) (eq f '*) (eq f '/) (eq f '<) (eq f '>) (eq f '=) (eq f '<=)
                                (eq f '>=) (eq f 'eq) (eq f 'equal) (eq f 'cons) (eq f 'null) (eq f 'not) (eq f 'atom)
                            ) (apply f (AOR arg P context))
                        )
                        ((eq f 'isnumber) (numberp (car (AOR arg P context))))      
                                          
                        ; handle primitive list functions
                        ((or (eq f 'first) (eq f 'car)) (caar (AOR arg P context)))
                        ((or (eq f 'rest) (eq f 'cdr)) (cdar (AOR arg P context)))
                        ; handle and
                        ((eq f 'and) (if (fl-interp-context (car arg) P context)
                                        (fl-interp-context (cadr arg) P context)
                                        NIL
                                    )
                        )
                        ; handle or
                        ((eq f 'or) (if (fl-interp (car arg) P)
                                        T
                                        (fl-interp (cadr arg) P)
                                    )
                        )
                        ; handle if
                        ((eq f 'if) (if (fl-interp-context (car arg) P context) 
                                        (fl-interp-context (cadr arg) P context)
                                        (fl-interp-context (caddr arg) P context)
                                    )
                        )
                        ; User-Defined functions
                        (T (let ((user_defined_function (user_function_exists f (expression_arg_num arg) P)))
                                (if (not (null user_defined_function))
                                    (fl-interp-context (cadr user_defined_function) P (append (generate_context (car user_defined_function) (AOR arg P context)) context))
                                    E
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)


; TEST ################################################
#|
(equal (fl-interp '(+ 10 5) nil) 15) ; > '15
(equal (fl-interp '(- 12 8) nil) 4); > '4
(equal (fl-interp '(* 5 9) nil) 45); > '45
(equal (fl-interp '(> 2 3) nil) nil); > 'nil
(equal (fl-interp '(< 1 131) nil) t); > 't
(equal (fl-interp '(= 88 88) nil) t); > 't
(equal (fl-interp '(and nil t) nil) nil); > 'nil
(equal (fl-interp '(or t nil) nil) t); > 't
(equal (fl-interp '(not t) nil) nil); > 'nil

(equal (fl-interp '(isnumber 354) nil) t); > 't
(equal (fl-interp '(equal (3 4 1) (3 4 1)) nil) t); > 't
(equal (fl-interp '(if nil 2 3) nil) 3); > '3
(equal (fl-interp '(null ()) nil) t); > 't
(equal (fl-interp '(atom (3)) nil) nil); > 'nil
(equal (fl-interp '(eq x x) nil) t); > 't
(equal (fl-interp '(first (8 5 16)) nil) 8); > '8
(equal (fl-interp '(rest (8 5 16)) nil) '(5 16)); > '(5 16)
(equal (fl-interp '(cons 6 3) nil) '(6 . 3)); > '(6 . 3)

(equal (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) 12); > '12
(equal (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2))) (not (= 3 2))) nil) t); > 't
(equal (fl-interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil) nil); > 'nil
(equal (fl-interp '(if (not (null (first (a c e)))) (if (isnumber (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) '((a c e) . d)); > '((a c e) . d)

(equal (fl-interp '(greater 3 5) '((greater x y = (if (> x y) x (if (< x y) y nil))))) 5); > '5
(equal (fl-interp '(square 4) '((square x = (* x x)))) 16); > '16
(equal (fl-interp '(simpleinterest 4 2 5) '((simpleinterest x y z = (* x (* y z))))) 40); > '40
(equal (fl-interp '(xor t nil) '((xor x y = (if (equal x y) nil t)))) t); > 't

(equal (fl-interp '(last (s u p)) '((last x = (if (null (rest x)) (first x) (last (rest x)))))) 'p); > 'p
(equal (fl-interp '(push (1 2 3) 4) '((push x y = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4)); > '(1 2 3 4)
(equal (fl-interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) '(1 2)); > '(1 2)
(equal (fl-interp '(power 4 2) '((power x y = (if (= y 1) x (power (* x x) (- y 1)))))) 16); > '16
(equal (fl-interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1))))))) 24); > '24
(equal (fl-interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) 6); > '6
|#