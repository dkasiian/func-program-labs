#lang racket
; Lab #3. Variant #5. Task: Розв’язати нелінійне рівняння x=cos(x) методами перебору та хорд, визначивши інтервал [a, b], на якому існує рішення рівняння. Порівняти результати розв’язків двома методами. 

; Метод перебору
(define (enumerative-technique x stp)
  (if (> (* (- (cos x) x) (+ x stp)) 0)
      (enumerative-technique (+ x stp) stp)
      (list x (+ x stp))))

(enumerative-technique 0 0.0001)


; Метод хорд 
(define (find-roots f a b 
                    #:divisions [N 10]
                    #:method [method secant])
  (define h (/ (- b a) N))
  (for*/list ([x1 (in-range a b h)] 
              [x2 (in-value (+ x1 h))]
              #:when (or (root? f x1)
                         (includes-root? f x1 x2)))
    (find-root f x1 x2 #:method method)))
 
; Знаходимо корінь функції f в заданому інтервалі [a b].
(define (find-root f a b #:method [method secant])
  (cond 
    [(root? f a) a]
    [(root? f b) b]
    [else (and (includes-root? f a b) (method f a b))]))
 
; Повертає #t, якщо x - корінь функції f з абсолютною точністю (допуском).
(define (root? f x) (almost-equal? 0 (f x)))
 
; Повертає #t, якщо інтервал (a b) містить корінь (або непарну кількість коренів) функції f.
(define (includes-root? f a b) (< (* (f a) (f b)) 0))
 
; Повертає #t, якщо a і b рівні щодо відносної точності (допуску).
(define (almost-equal? a b)
  (or (< (abs (+ b a)) (tolerance))      
      (< (abs (/ (- b a) (+ b a))) (tolerance))))
 
(define tolerance (make-parameter 5e-16))
 
(define (secant f a b)
  (let next ([x1 a] [y1 (f a)] [x2 b] [y2 (f b)] [n 50])
    (define x3 (/ (- (* x1 y2) (* x2 y1)) (- y2 y1)))
    (cond
      [(almost-equal? x3 x2) x3]
      [else (next x2 y2 x3 (f x3) (sub1 n))])))

(find-roots (lambda (x) (- (cos x) x)) 0 1 #:divisions 10)
