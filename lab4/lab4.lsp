(SETF MATRIX1 #2A((1 2 3)(4 5 6)(7 8 9)))
(SETQ LIST_NUM1 '(10 20))
(SETQ ROW1 3)
(SETQ COL1 3)

;Множення матриці на число L
(DEFUN MULT_NUMBER (MATR ROW COL L)
(DECLARE (SPECIAL RES_MATRIX))
(SETQ RES_MATRIX (MAKE-ARRAY (LIST ROW COL) :ELEMENT-TYPE 'INTEGER :INITIAL-ELEMENT 0))
(DO
((I 0))
((>= I ROW))
(DO
((J 0))
((>= J COL))
(SETF (AREF RES_MATRIX I J) (* L (AREF MATR I J)))
(SETQ J (+ J 1)))
(SETQ I (+ I 1)))
RES_MATRIX)

;DOT PRODUCT
(defun dot-product (a b)
  (cond ((null a) (if (null b) 0 (error "invalid length")))
        ((null b) (error "invalid length"))
        (t (+ (* (first a) (first b))
              (dot-product (rest a) (rest b))))))


;Apply "Множеня матриці на число" для списку
(DEFUN MULT1 (NUM)
(PRINT (MULT_NUMBER MATRIX1 ROW1 COL1 NUM)))

;Множення матриці на число
(PRINT (LIST 'MATRIX_1 MATRIX1))
(PRINT (LIST 'NUMBERS LIST_NUM1))
(MAPCAR 'MULT1 LIST_NUM1)

(PRINT (dot-product '(1 2) '(3 4)))
