(defun test (x) (* x 2))
(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
;;; how would you implement a hashmap in lisp?
;;; how would you implement an O(1) access / push array in lisp? 
;;; that's pretty easy actually (use `nth x '(1, 2, 3)`)
(defun vec-range (max)
  (let ((range-array (make-array max)))
    (progn
     (dotimes (i max)
       (setf (aref range-array i) i))
     range-array)))

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
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec lst nil)))

(defun before (x y lst)
  (let ((z (car lst)))
    (cond ((null x) nil)
          ((null y) nil)
          ((null lst) nil)
          ((eql x z) lst)
          ((eql y z) nil)
          (t (before x y (cdr lst))))))

(defun best (fn lst)
  (if (null lst) nil
      (let ((max-value (car lst)))
        (dolist (obj (cdr lst) max-value)
          (cond ((funcall fn obj max-value) (setq max-value obj)))))))

(defun mkstr (&rest args) (with-output-to-string (s)
                            (dolist (a args) (princ a s))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win val
              (setf (gethash args cache) (apply fn args)))))))

(defun range (n)
  (loop for i from 0 below n
        collect i))

(defun potter-kata (books &optional (discounts '(.75 .80 .90 .95)))
  "Solution to discounted Harry Potter book problem. https://codingdojo.org/kata/Potter/"
  ;; you want to maximize the weighted average discount
  ;; reverse sort the array
  ;; maximize maximum grouping until you're left with 1 copy
  ;; minimize two cases: 
  ;; 1. you group this 1 copy 
  ;; - decrement the rest of the books by 1
  ;; - recurse to (cdr books) (cdr discounts)
  ;; 2. you don't group this 1 copy
  ;; - add this book to the next copy (equivalent to a duplicate book)
  ;; *** key insight: any k-1 matching's bottleneck will be the next copy (bc it has the lowest quantity)
  ;; - recurse to (cdr books) (cdr discounts)
  (progn
   (print books)
   (cond ((null books) 0)
         ((null discounts) (reduce #'+ books))
         ((= 1 (length books)) (reduce #'+ books))
         (t (let ((k (1+ (length discounts)))
                  (sorted-books (sort books #'<)))
              (print sorted-books)
              (if (<= (car sorted-books) 0)
                  (potter-kata (cdr sorted-books) (cdr discounts))
                  (min (+ (* k (car discounts)) (potter-kata (mapcar #'1- (cdr sorted-books)) (cdr discounts)))
                    (potter-kata (cons (+ (car sorted-books) (cdar sorted-books)) (cddr sorted-books)) (cdr discounts)))))))))
