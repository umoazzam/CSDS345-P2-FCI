#lang racket

(require "simpleParser.rkt")

;; ***********************************************************
; Used to run txt files
; (interpret "name.txt") will convert to the parse tree and begin running our interpreter

(define interpret
  (lambda (filename)
    (evaluateParseTree (parser filename) initialState)))

(define evaluateParseTree
  (lambda (parseTree state)
    (cond
      ((null? parseTree) state)
      (else(evaluateParseTree (rest parseTree) (M_state (first parseTree) state))))))

;; ***********************************************************
; Basic types: values, boolean
(define M_value
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((eq? 'true statement) #t)
      ((eq? 'false statement) #f)
      ((not (list? statement)) (if (null? (checkDeclaredVariables statement state))
                              (error 'error "using variable before declaring")
                              (checkDeclaredVariables statement state)))
      ((eq? (operator statement) '+) (+ (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '-) (cond
                                       ;; abstract ME!
                                       ((null? (operand statement)) (- 0 (M_value (leftOperand statement) state)))
                                       (else(- (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))))
      ((eq? (operator statement) '*) (* (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '/) (quotient (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '%) (remainder (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '>) (> (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '>=) (>= (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '<) (< (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '<=) (<= (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '!=) (not (eq? (M_value (leftOperand statement) state) (M_value (rightOperand statement) state))))
      ((eq? (operator statement) '==) (eq? (M_value (leftOperand statement) state) (M_value (rightOperand statement) state)))
      ((eq? (operator statement) '||) (or (M_boolean (M_value (leftOperand statement) state) state) (M_boolean (M_value (rightOperand statement) state) state)))
      ((eq? (operator statement) '&&) (and (M_boolean (M_value (leftOperand statement) state) state) (M_boolean (M_value (rightOperand statement) state) state)))
      ((eq? (operator statement) '!) (not (M_boolean (M_value (leftOperand statement) state) state)))
      (else (error 'badop "Bad operator")))))

(define M_boolean
  (lambda (condition state)
    (cond
      ((or (eq? 'true condition) (eq? #t condition)) #t)
      ((or (eq? 'false condition) (eq? #f condition)) #f)
      (else (M_value condition state)))))

;; ***********************************************************
; States and function states

(define M_state
  (lambda (statement state)
    (cond
      ((eq? (function statement) 'return) (M_state-return (returnExpression statement) state))
      ((eq? (function statement) 'var) (M_state-declaration statement state))
      ((eq? (function statement) '=) (M_state-assign (leftOperand statement) (expression statement) state))
      ((eq? (function statement) 'if)
       (cond
         ((eq? (length statement) 4)(M_state-ifElse (ifCondition statement)(statement1 statement) (statement2 statement) state))
         (else (M_state-if (ifCondition statement)(statement1 statement) state))))
      ((eq? (function statement) 'while) (M_state-while (whileCondition statement) (whileStatement statement) state))
      (else (M_value statement state)))))

(define M_state-return
  (lambda (statement state)
    (cond
      ((eq? (M_value statement state) #t) 'true)
      ((eq? (M_value statement state) #f) 'false)
      (else (M_value statement state)))))

(define M_state-declaration
  (lambda (statement state)
    (cond
      ((declared? (newVariable statement) (declaredVariables state)) (error 'error "redefining already defined variable"))
      ((null? (value statement)) (list (cons (newVariable statement) (declaredVariables state)) (cons '() (declaredValues state))))
      (else (list(cons (newVariable statement) (declaredVariables state)) (cons (M_value (newValue statement) state) (declaredValues state)))))))

(define M_state-assign
  (lambda (variable statement state)
    (cond
      ((declared? variable (declaredVariables state)) (insert variable (M_value statement state) (remove variable state)))
      (else(error 'error "using before declaring")))))

(define M_state-if
  (lambda (condition ifStatement state)
    (cond
      ((M_boolean condition state) (M_state ifStatement state))
      (else state))))

(define M_state-ifElse
  (lambda (condition ifStatement elseStatement state)
    (cond
      ((M_boolean condition state) (M_state ifStatement state))
      (else (M_state elseStatement state)))))

(define M_state-while
  (lambda (condition statement state)
    (cond
      ((M_boolean condition state) (M_state-while condition statement (M_state statement state)))
      (else state))))
    

;; ***********************************************************
; checks if variable is already declared
(define declared?
  (lambda (var variables)
    (cond
      ((null? variables) #f)
      ((eq? var (car variables)) #t)
      (else (declared? var (rest variables))))))

; looks through all declared variables
(define checkDeclaredVariables
  (lambda (var state)
    (cond
      ((null? (declaredVariables state)) (error 'error "using before declaring"))
      ((eq? var (firstVariable state)) (firstValue state))
      (else (checkDeclaredVariables var (check state))))))

; insert newly declared variable's value
(define insert
  (lambda (var val state)
    (cond
      ((not (declared? var (declaredVariables state))) (error 'error "using before declaring"))
      ((eq? var (firstVariable state)) (cons (declaredVariables state) (cons (cons val (remainingValues state)) '())))
      (else (list (cons (firstVariable state) (declaredVariables (insert var val (check state))))
                  (cons (firstValue state) (declaredValues (insert var val (check state)))))))))


;; ***********************************************************
; Abstractions

(define initialState '(() ()))
(define first car)
(define rest cdr)

; looking for "var", "if", "while", "=", etc
(define function car)

(define expression caddr)
(define returnExpression cadr)

; Follows (- 1 2) format
(define operator car)
(define leftOperand cadr)
(define rightOperand caddr)
; used for negative operations, such as (- (* 4 2)), meaning that there is no rightOperand, so we can't take (car (cdr (cdr)))
(define operand cddr)

; used for when variable is declared but not given value
(define value cddr)

; used when variable is declared with value
(define newVariable cadr)
(define newValue caddr)
(define declaredVariables car)
(define declaredValues cadr)

(define firstVariable caar)
(define firstValue caadr)
(define remainingValues cdadr)

; check if variable is declared somewhere in state
(define check
  (lambda (state)
    (cons (cdar state) (cons (cdadr state) '()))))

(define ifCondition cadr)
(define statement1 caddr)
(define statement2 cadddr)

(define whileCondition cadr)
(define whileStatement caddr)

