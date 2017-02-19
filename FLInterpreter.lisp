; CMPUT 325 Assignment 02
; Michael Cote (ccid: mmcote)

(defun arg-length (args count)
    (cond
        ((null args) count)
        (T (+ (arg-length (cdr args) count) 1))
    )
)

(defun arg-length-till-equals (args count)
    (cond
        ((eq (car args) '=) count)
        (T (+ (arg-length-till-equals (cdr args) count) 1))
    )
)

(defun get-body (function_definition)
    (cond
        ((null function_definition) Nil)
        ((eq (car function_definition) '=) (cdr function_definition))
        (T (get-body (cdr function_definition)))
    )
)

(defun get-args (function_definition)
    (cond
        ((eq (car function_definition) '=) NIL)
        (T (append (get-args (cdr function_definition)) (list (car function_definition))))
    )
)

(defun find-function (function num_args given_functions)
    (cond
        ((null given_functions) Nil)
        ((and (eq function (caar given_functions)) (eq num_args (arg-length-till-equals (cdar given_functions) 0))) 
            (list (get-args (cdar given_functions)) (get-body (car given_functions)))
        )
        (T (find-function function num_args (cdr given_functions)))
    )
)


(defun fl-interp (E P)
    (cond
        ((atom E) E)   ;this includes the case where expr is nil
        (T
            (let ( (f (car E))  (arg (cdr E)) )
                (cond 
                    ; handle built-in functions
                    (
                        (or 
                            (eq f '+)
                            (eq f '-)
                            (eq f '*)
                            (eq f '/)
                            (eq f '<)
                            (eq f '>)
                            (eq f '=)
                            (eq f '<=)
                            (eq f '>=)
                            (eq f 'eq)
                            (eq f 'equal)
                            (eq f 'cons)
                        ) (funcall f (fl-interp (car arg) P) (fl-interp (cadr arg) P))
                    )
                    (
                        (or
                            (eq f 'first)
                            (eq f 'car)
                            (eq f 'rest)
                            (eq f 'cdr)
                        ) (funcall f (fl-interp (car arg) P))
                    )
                    ((eq f 'if) (if (fl-interp (car arg) P) 
                                    (fl-interp (cadr arg) P)
                                    (fl-interp (caddr arg) P)
                                    ))
                    ; User-Defined functions
                    ((find-function f (arg-length arg 0) P))



                    (T E)

                    ; if f is a user-defined function,
                        ;    then evaluate the arguments 
                        ;         and apply f to the evaluated arguments 
                        ;             (applicative order reduction) 
                        ;.....

                        ; otherwise f is undefined; in this case,
                        ; E is returned as if it is quoted in lisp
                )
            )
        )
    )
)