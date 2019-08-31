# Пример: `map`

Так можно реализовать аналог `map` для процедуры одного аргумента и одного списка:

```scheme
(define (maps proc xs)
  (if (null? xs)
      (list)
      (cons (proc (car xs))
            (maps proc (cdr xs)))))
```

А так на основе этого определения можно реализовать полный аналог `map` для процедуры с произвольным числом аргументов и произвольного числа списков:


```scheme
(define (mapm proc . xss)
  (define (helper yss)
    (if (null? (car yss))
        (list)
        (cons (apply proc (maps car yss))
              (helper (maps cdr yss)))))
  (helper xss))
```

