#lang racket

(require "simpleParser.rkt")

;; ***********************************************************
; Used to run txt files
; (interpret "name.txt") will convert to the parse tree and begin running our interpreter

(define interpret
  (lambda (filename)
    (evaluateParseTree (parser filename) '(()))))

(define evaluateParseTree
  (lambda (parseTree state)
    (call/cc
     (lambda (return)
       (cond
         ((null? parseTree) state)
         (else (evaluateParseTree (next parseTree) (M_state (first parseTree) state return))))))))

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
  (lambda (statement state return)
    (cond
      ((eq? (function statement) 'return) (return (M_state-return statement state)))
      ((eq? (function statement) 'var) (M_state-declaration statement state))
      ((eq? (function statement) '=) (assign statement state))
      ((eq? (function statement) 'while) (M_state-while statement state return))
      ((eq? (function statement) 'if) (M_state-ifElse statement state return))
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
      ((value? statement) (add (variable statement) (M_value (value statement) state) state))
      (else (add (variable statement) 'novalue state)))))

(define M_state-while
  (lambda (statement state return)
    (call/cc
     (lambda (break)
       (cond
         ((M_boolean (condition statement) state) (M_state statement
                                                           (call/cc
                                                            (lambda (continue)
                                                              (M_state (body statement) state return))) return))
         ((and (not (M_boolean (condition statement) state)) (null? (checkElse statement))) state)
         ((not (M_boolean (condition statement) state)) state))))))

(define M_state-ifElse
  (lambda (statement state return)
    (cond
      ((M_boolean (condition statement) state) (M_state (statement1 statement) state return))
      ((and (not (M_boolean (condition statement) state)) (null? (checkElse statement))) state)
      ((not (M_boolean (condition statement) state)) (M_state (statement2 statement) state return)))))

;; ***********************************************************
(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((declared?-helper var (car state)) #t)
      (else (declared? var (next state))))))
(define declared?-helper
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? (firstVar state) var) #t)
      (else  (declared?-helper var (next state))))))

(define value?
  (lambda (statement)
    (cond
      ((null? (assignedValue statement)) #f)
      (else #t))))

(define assigned?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((assigned?-helper var (current state)) #t)
      (else (assigned? var (next state))))))
(define assigned?-helper
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((and (eq? (firstVar state) var) (not (eq? (getValue state) 'novalue))) #t)
      (else (assigned?-helper var (next state))))))

      
(define assign
  (lambda (statement state)
    (cond
      ((declared? (variable statement) state) (insert (variable statement) (M_value (value statement) state) state))
      (else(error 'error "using before declaring")))))

(define insert
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((declared?-helper var (car state)) (cons (insert-helper var value (current state)) (next state)))
      (else (cons (current state) (insert var value (next state)))))))
(define insert-helper
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((eq? var (varName state)) (cons (cons var (cons value '())) (insert-helper var value (next state))))
      (else (cons (current state) (insert-helper var value (next state)))))))

(define add
  (lambda (var value state)
    (cons (append (first state) (cons (cons var (cons value '())) '())) (next state))))

(define varValue
  (lambda (var state)
    (cond
      ((null? state) (error 'error "Undeclared Variable"))
      ((number? (varValue-helper var (car state))) (varValue-helper var (current state)))
      ((eq? (varValue-helper var (car state)) #t) #t)
      ((eq? (varValue-helper var (car state)) #f) #f)
      (else (varValue var (next state))))))
(define varValue-helper
  (lambda (var state)
    (cond
      ((null? state) '())
      ((eq? var (varName state)) (getValue state))
      (else (varValue-helper var (next state))))))

;; ***********************************************************
; Abstractions
(define next cdr)

(define returnExpression cadr)

(define function car)

; Follows (- 1 2) format
(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)
; used for negative operations, such as (- (* 4 2)), meaning that there is no rightOperand, so we can't take (car (cdr (cdr)))
(define operand cddr)

(define variable cadr)
(define firstVar caar)
(define value caddr)
(define assignedValue cddr)
(define first car)
(define varName caar)
(define current car)
(define getValue cadar)
(define condition cadr)
(define body caddr)
(define checkElse cdddr) ;CHANGE ME
(define statement1 caddr)
(define statement2 cadddr)