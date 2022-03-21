#lang racket

(require "simpleParser.rkt")
(provide interpret)

;; ***********************************************************
; Used to run txt files
; (interpret "name.txt") will convert to the parse tree and begin running our interpreter

(define interpret
  (lambda (filename)
    (evaluateParseTree (parser filename) initialState)))

(define evaluateParseTree
  (lambda (parseTree state)
    (call/cc
     (lambda (return)
       (cond
         ((null? parseTree) state)
         (else (evaluateParseTree (rest parseTree) (M_state (first parseTree) state return
                                                            (lambda (v) (error 'error "continue outside of loop"))
                                                            (lambda (v2) (error 'error "break outside of loop"))
                                                            (lambda (v3) (error 'error "throw outside of loop"))
                                                            ))))))))

;; ***********************************************************
; Basic types: values, boolean
(define M_value
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((and (not (pair? statement)) (assigned? statement state)) (varValue statement state))
      ((and (not (pair? statement)) (declared? statement state)) (error 'error "using before assigning"))
      ((eq? 'true statement) #t)
      ((eq? 'false statement) #f)
      ((not (pair? statement)) (error 'error "using before declaring"))
      ((eq? (operator statement) '+) (+ (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '-) (cond
                                       ((null? (operand statement)) (- 0 (M_value (leftOperand statement) state)))
                                       (else(- (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))))
      ((eq? (operator statement) '*) (* (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '/) (quotient (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '%) (remainder (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      (else (M_boolean statement state)))))

(define M_boolean
  (lambda (statement state)
    (cond
      ((or (eq? 'true statement) (eq? #t statement)) #t)
      ((or (eq? 'false statement) (eq? #f statement)) #f)
      ((eq? (operator statement) '>) (> (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '>=) (>= (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '<) (< (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '<=) (<= (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '!=) (not (eq? (M_value (leftOperand statement) state) (M_value (rightOperand statement) state))))
      ((eq? (operator statement) '==) (eq? (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '||) (or (M_boolean (M_value (leftOperand statement) state) state) (M_boolean (M_value (rightOperand statement) state) state)))
      ((eq? (operator statement) '&&) (and (M_boolean (M_value (leftOperand statement) state) state) (M_boolean (M_value (rightOperand statement) state) state)))
      ((eq? (operator statement) '!) (not (M_boolean (M_value (leftOperand statement) state) state))))))

;; ***********************************************************
; States and function states
(define M_state
  (lambda (statement state return continue break throw)
    (cond
      ((eq? (function statement) 'return) (return (M_state-return statement state)))
      ((eq? (function statement) 'var) (M_state-declaration statement state))
      ((eq? (function statement) '=) (assign statement state))
      ((eq? (function statement) 'while) (M_state-while statement state return continue break throw))
      ((eq? (function statement) 'if) (M_state-ifElse statement state return continue break throw))
      ((eq? (function statement) 'begin) (M_state-block statement state return continue break throw))
      ((eq? (function statement) 'continue) (continue state))
      ((eq? (function statement) 'break) (break (popLayer state)))
      ((eq? (function statement) 'throw) (throw (add 'exception (M_value (throwValue statement) state) state)))
      ((eq? (function statement) 'try) (M_state-try statement state return continue break throw))
      ((eq? (function statement) 'catch) (M_state-catch statement state return continue break throw))
      ((eq? (function statement) 'finally) (M_state-finally statement state return continue break throw))
      (else (error 'error "Invalid statement")))))

(define M_state-return
  (lambda (statement state)
    (cond
      ((eq? (M_value (returnExpression statement) state) #t) 'true)
      ((eq? (M_value (returnExpression statement) state) #f) 'false)
      (else (M_value (returnExpression statement) state)))))

(define M_state-declaration
  (lambda (statement state)
    (cond
      ((declared? (variable statement) state) (error 'error "redefining already defined variable"))
      ((value? statement) (add (variable statement) (M_value (val statement) state) state))
      (else (add (variable statement) 'null state)))))

(define M_state-while
  (lambda (statement state return continue break throw)
    (call/cc
     (lambda (break)
       (cond
         ((M_boolean (condition statement) state) (M_state statement (call/cc (lambda (continue) (M_state (body statement) state return continue break throw))) return continue break throw))
         ((not (M_boolean (condition statement) state)) state))))))

(define M_state-ifElse
  (lambda (statement state return continue break throw)
    (cond
      ((M_boolean (condition statement) state) (M_state (statement1 statement) state return continue break throw))
      ((and (not (M_boolean (condition statement) state)) (null? (else statement))) state)
      ((not (M_boolean (condition statement) state)) (M_state (statement2 statement) state return continue break throw)))))

(define M_state-block
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) (popLayer state))
      ((eq? (function statement) 'begin) (M_state-block (rest statement) (pushLayer state) return continue break throw))
      (else (M_state-block (rest statement) (M_state (first statement) state return continue break throw) return continue break throw)))))

(define M_state-try
  (lambda (statement state return continue break throw)
    (cond
      ((and (null? (catchBlock statement)) (null? (finallyBlock statement))) (error 'error "try without catch and finally"))
      (else (M_state-finally (finallyBlock statement) (M_state-catch (catchBlock statement) (call/cc (lambda (throw) (try-helper statement (tryBody statement) state return continue break throw))) return continue break throw) return continue break throw)))))

(define M_state-catch
  (lambda (catchBlock state return continue break throw)
       (cond
         ((null? catchBlock) state)
         ((and (eq? (function catchBlock) 'catch) (declared? 'exception state)) (M_state-catch (catchStatement catchBlock) (rename (getException catchBlock) state) return continue break throw))
         ((eq? (function catchBlock) 'catch) state)
         (else (M_state-catch (rest catchBlock) (M_state (first catchBlock) state return continue break throw) return continue break throw)))))

(define M_state-finally
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      (else (M_state-finally (rest statement) (M_state (first statement) state return continue break throw) return continue break throw)))))

;; ***********************************************************
(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((declared?-helper var (first state)) #t)
      (else (declared? var (rest state))))))
; helper is used to go into local variables
(define declared?-helper
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? (firstVar state) var) #t)
      (else  (declared?-helper var (rest state))))))

(define value?
  (lambda (statement)
    (cond
      ((null? (value statement)) #f)
      (else #t))))

(define assigned?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((assigned?-helper var (first state)) #t)
      (else (assigned? var (rest state))))))
(define assigned?-helper
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((and (eq? (firstVar state) var) (not (eq? (getValue state) 'null))) #t)
      (else (assigned?-helper var (rest state))))))

      
(define assign
  (lambda (statement state)
    (cond
      ((declared? (variable statement) state) (insert (variable statement) (M_value (val statement) state) state))
      (else(error 'error "using before declaring")))))
; insert value of assigned variable into the state
(define insert
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((declared?-helper var (first state)) (cons (insert-helper var value (first state)) (rest state)))
      (else (cons (first state) (insert var value (rest state)))))))
(define insert-helper
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((eq? var (varName state)) (cons (cons var (cons value '())) (insert-helper var value (rest state))))
      (else (cons (first state) (insert-helper var value (rest state)))))))
; add variable into state
(define add
  (lambda (var value state)
    (cons (append (first state) (cons (cons var (cons value '())) '())) (rest state))))

; take value of variable 
(define varValue
  (lambda (var state)
    (cond
      ((null? state) (error 'error "using before declaring"))
      ((number? (varValue-helper var (first state))) (varValue-helper var (first state)))
      ((eq? (varValue-helper var (first state)) #t) #t)
      ((eq? (varValue-helper var (first state)) #f) #f)
      (else (varValue var (rest state))))))
(define varValue-helper
  (lambda (var state)
    (cond
      ((null? state) '())
      ((eq? var (varName state)) (getValue state))
      (else (varValue-helper var (rest state))))))

(define try-helper
  (lambda (try statement state return continue break throw)
    (cond
      ((null? statement) state)
      ((eq? 'return (function (first statement))) (M_state (first statement) (M_state-finally (finallyBlock try) state return continue break throw) return continue break throw))
      ((eq? 'break (function (first statement))) (M_state (first statement) (M_state-finally (finallyBlock try) state return continue break throw) return continue break throw))
      (else (try-helper try (rest statement) (M_state (first statement) state return continue break throw) return continue break throw)))))
; returns exception
(define rename
  (lambda (exception state)
    (cons (rename-helper exception (topLayer state)) (nextLayer state))))
(define rename-helper
  (lambda (e thislayer)
    (cond
      ((null? thislayer) '())
      ((eq? (name thislayer) 'exception) (cons (cons e (cons (getValue thislayer) '())) (rest thislayer)))
      (else (cons (first thislayer) (rename-helper e (rest thislayer))))))) 

;; ***********************************************************
; Abstractions
(define initialState '(()))
(define first car)
(define rest cdr)

; looking for "var", "if", "while", "=", etc
(define function car)

(define returnExpression cadr)

; Follows (- 1 2) format
(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)

(define condition cadr)
(define statement1 caddr)
(define statement2 cadddr)

; used for negative operations, such as (- (* 4 2)), meaning that there is no rightOperand, so we can't take (car (cdr (cdr)))
(define operand cddr)

(define variable cadr)
(define value cddr)

; need to get x from ((x 0) (...))
(define firstVar caar)

(define val caddr)
(define varName caar)
(define getValue cadar)

(define body caddr)
(define else cdddr)

(define popLayer cdr)
(define pushLayer
  (lambda (state)
    (cons '() state)))

(define throwValue cadr)

(define topLayer car)
(define nextLayer cdr)
(define name caar)

(define tryBody cadr)

(define catchStatement caddr)
(define getException caadr)

(define catchBlock
  (lambda (statement)
    (cond
      ((null? (caddr statement)) '())
      (else (caddr statement)))))
(define finallyBlock
  (lambda (cmd)
    (cond
      ((null? (cadddr cmd)) '())
      (else (cadr (cadddr cmd))))))