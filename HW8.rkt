;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))

(define extend-env2
  (lambda (listOfLists env)
    (cond
      ((null? listOfLists) env)
      (else (extend-env2 (cdr listOfLists) (cons (car listOfLists) env))))))


(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))


(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))

(define boolean-expression-parser
  (lambda (boolean-expression)
    (cond
     ((eq? (car boolean-expression) '<)
         (list 'less-than (no-parser (cadr boolean-expression)) (no-parser (caddr boolean-expression))))
     ((eq? (car boolean-expression) '>)
         (list 'greater-than (no-parser (cadr boolean-expression)) (no-parser (caddr boolean-expression))))
     ((eq? (car boolean-expression) '<=)
         (list 'less-than-or-equal (no-parser (cadr boolean-expression)) (no-parser (caddr boolean-expression))))
     ((eq? (car boolean-expression) '>=)
         (list 'greater-than-or-equal (no-parser (cadr boolean-expression)) (no-parser (caddr boolean-expression))))
     ((eq? (car boolean-expression) '==)
         (list 'equal-to (no-parser (cadr boolean-expression)) (no-parser (caddr boolean-expression))))
     ((eq? (car boolean-expression) '!=)
         (list 'not-equal-to (no-parser (cadr boolean-expression)) (no-parser (caddr boolean-expression))))
     (else "Not a valid boolean expression"))))

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      
      ((eq? (car no-code) 'let) 
       (list 'let-exp
             (cadr no-code)
             (no-parser (caddr no-code))))


      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))


(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))

(define run-parsed-boolean-code
  (lambda (parsed-boolean-code env)
    (cond
      ((eq? (car parsed-boolean-code) 'less-than)
          (< (run-parsed-code (cadr parsed-boolean-code) env) (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-than)
          (> (run-parsed-code (cadr parsed-boolean-code) env) (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'less-than-or-equal)
          (<= (run-parsed-code (cadr parsed-boolean-code) env) (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-than-or-equal)
          (>= (run-parsed-code (cadr parsed-boolean-code) env) (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'equal-to)
          (= (run-parsed-code (cadr parsed-boolean-code) env) (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'not-equal-to)
          (not (= (run-parsed-code (cadr parsed-boolean-code ) env) (run-parsed-code (caddr parsed-boolean-code) env))))
      (else #f))))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
          (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
          (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
           (cadr parsed-no-code)
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
          (if (run-parsed-boolean-code (cadr parsed-no-code) env)
              (run-parsed-code (caddr parsed-no-code) env)
              (run-parsed-code (cadddr parsed-no-code) env)))
      
      ((eq? (car parsed-no-code) 'let-exp)
       (run-parsed-code (caddr parsed-no-code) (extend-env2 (cadr parsed-no-code) env)))

     
        
      (else
          (run-parsed-function-code
              (cadr parsed-no-code)
              (extend-env
                  (cdr (cadr (cadr parsed-no-code))) ;(x y)
                  (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
                  env))))))


(define letSample
  (lambda (d)
    (let ((a 1) (b 2) (c 3)) (+ d (* b c)))))

(letSample 2)

(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(ask (< b 5) 1 otherwise 2))
(define parsed-no-code (no-parser sample-no-code))

(run-parsed-code parsed-no-code env)
(run-parsed-boolean-code (boolean-expression-parser '(> 5 2)) env)

(define sampleWithLet '(ask (< 5 10) (let ((b 2) (c 3)) (do-mathy-stuff + b c)) otherwise c))
(no-parser sampleWithLet)
(run-parsed-code (no-parser sampleWithLet) env)

