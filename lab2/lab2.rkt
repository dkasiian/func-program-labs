; Lab #2. Variant #5. Task: Для заданого числа визначити рекурсивні процедури для обчислення суми та кількості його цифр, максимальної та мінімальної цифри. 
(define (sum-of-digits num)
    (if (< num 10)
        num
        (+ (remainder num 10) (sum-of-digits (/ (- num (remainder num 10)) 10)))))

(sum-of-digits 2798)


(define (count-digits num)
  (if (< num 10)
      1
      (+ 1 (count-digits (/ num 10)))))

(count-digits 2798)


(define (find-max-digit num)
  (if (= num 0)
      0
      (if (> (remainder num 10) (find-max-digit (quotient num 10)))
          (remainder num 10)
          (find-max-digit (quotient num 10)))))

(find-max-digit 2798)


(define (find-min-digit num)
  (if (= num 0)
      9
      (if (< (remainder num 10) (find-min-digit (quotient num 10)))
          (remainder num 10)
          (find-min-digit (quotient num 10)))))

(find-min-digit 2798)
