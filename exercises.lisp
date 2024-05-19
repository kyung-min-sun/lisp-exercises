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

(defun behave (animal) (funcall (get animal 'behavior)))
(setf (get 'dog 'behavior) #'(lambda () (print "5")))

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun count-instances (obj lsts)
  (labels ((instances-in (lst) 
    (if (consp lst) 
      (+ (if (eq (car lst) obj) 1 0)
         (instances-in (cdr lst)))
      0)))
      (mapcar #'instances-in lsts)))

(defun our-length (lst)
  (labels ((rec (lst acc)
    (if (null lst)
      acc 
      (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(defun good-reverse (lst)
  (labels ((rev (lst acc) 
    (if (null lst) 
      (values 0 acc)
      (multiple-value-bind (len acc2) (rev (cdr lst) (cons (car lst) acc))
      (values acc2 (+ 1 len))))))
    (rev lst nil)))

(defun our-remove-if-not (lst val)
  (if (null lst) lst
    (if (= (car lst) val)
      (cons (car lst) (our-remove-if-not (cdr lst) val))
  (our-remove-if-not (cdr lst) val))))

(defun filter (expr lst)
  (if (null lst) 
    lst
    (if (funcall expr (car lst)) 
      (cons (car lst) (filter expr (cdr lst)))
      (filter expr (cdr lst)))))

(defun group (source n)
  (if (zerop n) (error "zero length")) 
  (labels ((rec (source acc)
    (let ((rest (nthcdr n source))) 
      (if (consp rest)
        (rec rest (cons (subseq source 0 n) acc))
        (nreverse (cons source acc)))))) 
    (if source (rec source nil) nil)))
  
(defun flatten (lst)
  (labels ((rec (x acc)
    (cond ((null x) acc)
    ((atom x) (cons x acc))
      (t (rec (car x) (rec (cdr x) acc)))
    )))
  (rec lst nil)))
