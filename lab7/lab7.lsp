(defun simplify (p)
 (if (null (cdr p)) p
     (if (zerop (car (last p))) (simplify (butlast p)) p)))
 
 
(defun poly-add (p1 p2)
   (let* ((n1 (- (length p1) 1))
          (n2 (- (length p2) 1))
          (n (max n1 n2))
          (res nil))
    (dotimes (i (+ 1 n) (simplify (reverse res)))      
      (cond ((and (<= i n1) (<= i n2)) (push (+ (nth i p1) (nth i p2)) res))
            ((<= i n1) (push (nth i p1) res))
            (t (push (nth i p2) res))))))
     
 
(defun poly-sub (p1 p2)
   (let* ((n1 (- (length p1) 1))
          (n2 (- (length p2) 1))
          (n (max n1 n2))
          (res nil))
    (dotimes (i (+ 1 n) (simplify (reverse res)))      
      (cond ((and (<= i n1) (<= i n2)) (push (- (nth i p1) (nth i p2)) res))
            ((<= i n1) (push (nth i p1) res))
            (t (push (- (nth i p2)) res))))))

     
(defun poly-by-mon (p c m) ;; с-коэфф, m-степень
   (append (make-list m :initial-element 0) (mapcar #'(lambda (q) (* c q)) p)))
 
 
(defun poly-mult (p1 p2 &optional (c 0) (r (list 0)))
  (cond ((null p1) r)
        (t (let ((pp2 (poly-by-mon p2 (car p1) c)))
                 (poly-mult (cdr p1) p2 (+ c 1) (poly-add r pp2))))))
 
 
(defun poly-div (p1 p2 &optional (d nil))
  (let ((n1 (- (length p1) 1))
        (n2 (- (length p2) 1)))
     (if (< n1 n2) 
         (list (simplify d) p1)
         (if (zerop n2) (mapcar (lambda (x) (/ x (car p2))) p1)
             (let ((m (- n1 n2))
                   (q (/ (nth n1 p1) (nth n2 p2))))
                  (poly-div (poly-sub p1 (poly-by-mon p2 q m)) p2 (cons q d)))))))
 
;; Наибольший общий делитель
 
(defun poly-gcd (p1 p2)
  (cond ((> (length p2) (length p1)) (poly-gcd p2 p1))
        ((equal p2 (list 0)) p1)
        (t (poly-gcd p2 (cadr (poly-div p1 p2))))))

(PRINT (poly-gcd '(11 -22 12 -2 1) '(13 -21 3 5)))
