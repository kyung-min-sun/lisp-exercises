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

(defun range (n &optional (min 0))
  (loop for i from min below n
        collect i))

(defun potter-kata-helper (books &optional (discounts '(.75 .80 .90 .95)))
  "Solution to discounted Harry Potter book problem. https://codingdojo.org/kata/Potter/"
  (let* ((k (length books))
         (sorted-books (sort (copy-list books) #'<))
         (max-book (car sorted-books)))
    (cond ((null sorted-books) 0)
          ((null discounts) (reduce #'+ sorted-books))
          ((= 1 k) (reduce #'+ sorted-books))
          ((= 0 max-book) (potter-kata-helper (cdr sorted-books) (cdr discounts)))
          (t (labels ((iter (n)
                            (let* ((grouped (mapcar #'(lambda (x) (- x n)) (cdr sorted-books)))
                                   (shifted (cons (+ (- max-book n) (car grouped)) (cdr grouped))))
                              (+ (* k (car discounts) n)
                                 (potter-kata-helper shifted (cdr discounts))))))
               (reduce #'min (mapcar #'iter (range (+ 1 max-book)))))))))

(defun process-book-list (books)
  (reduce
      #'(lambda (acc n) (progn (incf (nth n acc)) acc))
    books
    :initial-value (list 0 0 0 0 0)))

(defun potter-kata (books)
  (if (null books) 0
      (* 8 (potter-kata-helper (process-book-list books)))))

(defun kata-test (fn args expected)
  (progn
   (format t "Testing ~A... " args)
   (let ((value (funcall fn args)))
     (if (eql value expected) (format t "Passed~%")
         (format t "Failed.~%Got ~A, expected ~A~%" value expected)))))

(defun test-potter-kata ()
  (progn
   ;; basic
   (kata-test 'potter-kata '() 0)
   (kata-test 'potter-kata '(1) 8)
   (kata-test 'potter-kata '(2) 8)
   (kata-test 'potter-kata '(3) 8)
   (kata-test 'potter-kata '(4) 8)
   (kata-test 'potter-kata '(1 1 1) (* 8 3))
   ;; simple discounts
   (kata-test 'potter-kata '(0 1) (* 8 2 .95))
   (kata-test 'potter-kata '(0 2 4) (* 8 3 .9))
   (kata-test 'potter-kata '(0 1 2 4) (* 8 4 .8))
   (kata-test 'potter-kata '(0 1 2 3 4) (* 8 5 .75))
   ;; several discounts
   (kata-test 'potter-kata '(0 0 1) (+ 8 (* 8 2 .95)))
   (kata-test 'potter-kata '(0 0 1 1) (* 2 (* 8 2 .95)))
   (kata-test 'potter-kata '(0 0 1 2 2 3) (+ (* 8 4 .8) (* 8 2 .95)))
   (kata-test 'potter-kata '(0 1 1 2 3 4) (+ 8 (* 8 5 .75)))
   ;; edge cases
   (kata-test 'potter-kata '(0 0 1 1 2 2 3 4) (* 2 (* 8 4 .8)))
   (kata-test 'potter-kata
              '(0 0 0 0 0
                  1 1 1 1 1
                  2 2 2 2
                  3 3 3 3 3
                  4 4 4 4)
              (+ (* 2 (* 8 4 .8)) (* 3 (* 8 5 .75))))))