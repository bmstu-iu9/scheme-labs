(load "stream.scm")
(load "trace.scm")

; Подмножество языка Scheme
; <sequence> ::= <term> <sequence> | <empty>
; <empty> ::=
; <term> ::= <define> | <expr>
; <define> ::= (DEFINE VAR <expr>)
; <expr> ::= VAR | (<complex-const>)
; <complex-constr> ::= <lambda> | <call>
; <lambda> ::= LAMBDA <varlist> <sequence>
; <call> ::= <expr> <exprs>
; <exprs> ::= <empty> | <expr> <exprs>
;
; Лексика:
; <tokens> ::= <token> <tokens>
;            | <spaces> <tokens>
;            | <empty>
; <spaces> ::= SPACE <spaces> | <empty>
; <token> ::= "(" | ")" | <variable-or-keyword>
; <variable-or-keyword> ::= LETTER <variable-tail>
; <variable-tail> ::= <empty>
;                   | LETTER <variable-tail>
;                   | DIGIT <variable-tail>


; ==================
; Лексический анализ
; ==================

(define (scan str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))
    
    (call-with-current-continuation
     (lambda (error)
       (define result (tokens stream error))
       (and (equal? (peek stream) EOF)
            result)))))


; <tokens> ::= <spaces> <tokens>
;            | <token> <tokens>
;            | <empty>
;
; (tokens stream error) -> list of tokens
(define (tokens stream error)
  (define (start-token? char)
    (or (char-letter? char)
        (char-digit? char)
        (equal? char #\()
        (equal? char #\))))
  
  (cond ((char-whitespace? (peek stream))
         (spaces stream error)
         (tokens stream error))
        ((start-token? (peek stream))
         (cons (token stream error)
               (tokens stream error)))
        (else '())))

; <spaces> ::= SPACE <spaces> | <empty>
;
; (spaces stream error) -> <void>
(define (spaces stream error)
  (cond ((char-whitespace? (peek stream))
         ;(if (char-whitespace? (peek stream))
         ;    (next stream)
         ;    (error #f))
         (next stream))
        (else #t)))

(define char-letter? char-alphabetic?)
(define char-digit? char-numeric?)


; <token> ::= "(" | ")" | <variable-or-keyword>
;
; (token stream error) -> token
(define (token stream error)
  (cond ((equal? (peek stream) #\() (next stream))
        ((equal? (peek stream) #\)) (next stream))
        ((char-letter? (peek stream))
         (variable-or-keyword stream error))
        (else (error #f))))

; <variable-or-keyword> ::= LETTER <variable-tail>
;
; (variable-or-keyword stream error) -> SYMBOL
(define (variable-or-keyword stream error)
  (cond ((char-letter? (peek stream))
         ;(if (char-letter? (peek stream))
         ;    (next stream)
         ;    (error #f))
         (string->symbol
          (list->string (cons (next stream)
                              (variable-tail stream error)))))
        (else (error #f))))

; <variable-tail> ::= LETTER <variable-tail>
;                   | DIGIT <variable-tail>
;                   | <empty>
;
; (variable-tail stream error) -> List of CHARs
(define (variable-tail stream error)
  (cond ((char-letter? (peek stream))
         (cons (next stream)
               (variable-tail stream error)))
        ((char-digit? (peek stream))
         (cons (next stream)
               (variable-tail stream error)))
        (else '())))


; =====================
; Синтаксический анализ
; =====================

(define (parse tokens)
  (define stream (make-stream tokens))
  
  (call-with-current-continuation
   (lambda (error)
     (sequence stream error)
     (equal? (peek stream) #f))))

; <sequence> ::= <term> <sequence> | <empty>
(define (sequence stream error)
  (cond ((start-term? (peek stream))
         (term stream error)
         (sequence stream error))
        (else #t)))

(define (start-term? token)
  (or (equal? token #\() (variable? token)))

(define (variable? token)
  (and (symbol? token)
       (not (equal? token 'define))
       (not (equal? token 'lambda))))

; <term> ::= <define> | <expr>
(define (term stream error)
  (cond ((start-define? (peek2 stream))
         (parse-define stream error))
        ((start-expr? (peek2 stream))
         (expr stream error))
        (else (error #f))))

(define (start-define? pair)
  (and (list? pair)
       (= (length pair) 2)
       (equal? (car pair) #\()
       (equal? (cadr pair) 'define)))

(define (start-expr? pair)
  (or (start-lambda? pair)
      (start-call? pair)
      (variable? (car pair))))

; <define> ::= (DEFINE VAR <expr>)
(define (parse-define stream error)
  (cond ((equal? (peek stream) #\()
         (next stream)                      ; съедаем (
         (if (equal? (peek stream) 'define) ; съедаем define
             (next stream)
             (error #f))
         (if (variable? (peek stream))      ; съедаем имя переменной
             (next stream)
             (error #f))
         (expr stream error)
         (if (equal? (peek stream) #\))     ; съедаем )
             (next stream)
             (error #f)))
        (else (error #f))))

; <expr> ::= VAR | <lambda> | <call>
(define (expr stream error)
  (cond ((variable? (peek stream)) (next stream))
        ((start-lambda? (peek2 stream))
         (parse-lambda stream error))
        ((start-call? (peek2 stream))
         (call stream error))
        (else (error #f))))

(define (start-lambda? pair)
  (and (list? pair)
       (= (length pair) 2)
       (equal? (car pair) #\()
       (equal? (cadr pair) 'lambda)))

(define (start-call? pair)
  (and (list pair)
       (= (length pair) 2)
       (equal? (car pair) #\()
       (or (variable? (cadr pair))
           (equal? (cadr pair) #\())))

; <lambda> ::= (LAMBDA <varlist> <sequence>)
(define (parse-lambda stream error)
  (cond ((equal? (peek stream) #\()
         (next stream)                      ; съедаем (
         (if (equal? (peek stream) 'lambda) ; съедаем lambda
             (next stream)
             (error #f))
         (varlist stream error)
         (sequence stream error)
         (if (equal? (peek stream) #\))
             (next stream)
             (error #f)))
        (else (error #f))))

; <varlist> ::= (<vars>)
(define (varlist stream error)
  (cond ((equal? (peek stream) #\()
         (next stream)
         (vars stream error)
         (if (equal? (peek stream) #\))
             (next stream)
             (error #f)))
        (else #f)))

; <vars> ::= VAR <vars> | <empty>
(define (vars stream error)
  (cond ((variable? (peek stream))
         (next stream)
         (vars stream error))
        (else #t)))

; <call> ::= (<expr> <exprs>)
(define (call stream error)
  (if (equal? (peek stream) #\()
      (next stream)
      (error #f))
  (expr stream error)
  (exprs stream error)
  (if (equal? (peek stream) #\))
      (next stream)
      (error #f)))

; <exprs> ::= <empty> | <expr> <exprs>
(define (exprs stream error)
  (cond ((start-expr? (peek2 stream))
         (expr stream error)
         (exprs stream error))
        (else #t)))
