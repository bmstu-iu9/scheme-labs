(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (begin
       (write 'expr)
       (display " => ")
       (let ((v expr))
         (write v)
         v)))))