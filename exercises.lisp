(defun test (x) (* x 2))
(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
;;; how would you implement a hashmap in lisp?
;;; how would you implement an O(1) access / push array in lisp? 
;;; that's pretty easy actually (use `nth x '(1, 2, 3)`)
(defun range (max) 
  (let ((range-array (make-array max))) 
  (progn
    (dotimes (i max)
      (setf (aref range-array i) i))
    range-array
  )))

(defun time-array-access (n) 
  (let ((x (range n)))
    (time (aref x (- n 1)))))