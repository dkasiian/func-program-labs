(in-package #:cl-user)
(defpackage #:phrase
  (:use #:cl)
  (:shadow :count)
  (:export #:word-count))
(in-package #:phrase)

(defun word-count (phrase)
  (count
   (remove-blank
    (downcase
     (remove-punctuation
      (split-into-words phrase))))))

(defun count (words)
  (reduce #'collate-word words :initial-value (list)))

(defun remove-blank (words)
  (remove "" words :test #'string=))

(defun downcase (words)
  (mapcar #'string-downcase words))

(defun remove-punctuation (words)
  (mapcar #'(lambda (word) (remove-if-not #'alphanumericp word)) words))

(defun split-into-words (phrase)
  (split-string phrase #\Space))

;; The gory details
(defun split-string (string char &optional (result (list)))
  (let ((idx (position char string)))
    (if idx
        (split-string (subseq string (1+ idx)) #\Space
                      (push (subseq string 0 idx) result))
        (push string result))))

(defun collate-word (counts word)
  (if #1=(assoc word counts :test #'string=)
      (progn
        (setf (cdr #1#) (incf (cdr #1#)))
        counts)
      (acons word 1 counts)))

(PRINT (word-count "Tom can't taste the difference between expensive wine and cheap wine."))