(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list))
 
(defun make-queue ()
  "Returns an empty queue."
  (%make-queue))
 
(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))
 
(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))))
 
(defun dequeue (queue)
  "Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
    (error "Cannot dequeue from empty queue.")
    (pop (queue-items queue))))

(defun minimum (list)
 "recursive function to return the minimum value of a list of numbers"
 (cond ((null list)
        nil)
       ((null (rest list))
        (first list))
       ((< (first list) (second list))
        (minimum (cons (first list)
                       (rest (rest list)))))
       (t
        (minimum (rest list)))))

(defun maximum (list)
 "recursive function to return the maximum value of a list of numbers"
 (cond ((null list)
        nil)
       ((null (rest list))
        (first list))
       ((> (first list) (second list))
        (maximum (cons (first list)
                       (rest (rest list)))))
       (t
        (maximum (rest list)))))

(defun avernum (x)
    "Average value"
    (defun sumup (x)
        (if (equal x nil) 0
            (+ (car x) (sumup (cdr x)) )
        )
    )
    (if 
        (equal x nil) 0
        (/ (sumup x) (list-length x) )
    )
)



(PRINT (setq queue (enqueue 5 (make-queue))))
(PRINT (setq queue (enqueue 6 queue)))
(PRINT (setq queue (enqueue 14 queue)))
(PRINT (setq queue (enqueue 2 queue)))
(PRINT (setq queue (enqueue -6 queue)))

(PRINT (dequeue queue))
(PRINT (dequeue queue))

(PRINT :MIN)
(PRINT (setq min (minimum '(5 6 14 2 -6))))
(PRINT :MAX)
(PRINT (setq max (maximum '(5 6 14 2 -6))))
(PRINT :AVG)
(PRINT (avernum (list min max)))