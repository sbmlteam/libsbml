(in-package "CPARSE")

;;; test parameters

(defparameter +int1+ (make-instance 'int-const :value 123))
(defparameter +int2+ (make-instance 'int-const :value 456))

(defun test1 ()
  (let ((result (c+ *target-compiler* +int1+ +int2+)))
    (assert (eq (type-of result) 'int-const))
    (assert (eql (value result) 579))))

(defparameter +char1+ (make-instance 'unsigned-char-const :value 128))

(defun test2 ()
  (let ((result (c+ *target-compiler* +int1+ +char1+)))
    (assert (eq (type-of result) 'int-const))
    (assert (eql (value result) 251))))

(defparameter +double1+ (make-instance 'double-const :value 1.0d0))

(defun test3 ()
  (let ((result (c+ *target-compiler* +int1+ +double1+)))
    (assert (eq (type-of result) 'double-const))
    (assert (= (value result) 124.0))))

(defparameter +float1+ (make-instance 'cfloat-const :value 2.0))

(defun test4 ()
  (let ((result (c+ *target-compiler* +int1+ +float1+)))
    (assert (eq (type-of result) 'cfloat-const))
    (assert (= (value result) 125.0))))

(defparameter +long1+ (make-instance 'long-const :value 1234567))
(defparameter +unsigned1+ (make-instance 'unsigned-int-const :value 23456))

(defun test5 ()
  (let ((result (c+ *target-compiler* +unsigned1+ +long1+)))
    (assert (eq (type-of result) 'unsigned-long-const))
    (assert (= (value result) 1258023))))

;;; Overflow

(defparameter +long2+ (make-instance 'long-const :value #X7fffffff))
(defparameter +unsigned2+ (make-instance 'unsigned-int-const :value #Xabc))

(defun test6 ()
  (let ((result (c+ *target-compiler* +long2+ +unsigned2+)))
    (assert (eq (type-of result) 'unsigned-long-const))
    (assert (= (value result) #X80000abb))))

(defparameter +long3+ (make-instance 'long-const :value #X7fffffff))
(defparameter +int3+ (make-instance 'int-const :value 1))

(defun test7 ()
  (let ((result (c+ *target-compiler* +long3+ +int3+)))
    (assert (eq (type-of result) 'long-const))
    (assert (= (value result) -2147483648))))

(defun test8 ()
  (let ((result (c- *target-compiler* +int1+ +int2+)))
    (assert (eq (type-of result) 'int-const))
    (assert (eql (value result) -333))))

(defun test9 ()
  (let ((result (c- *target-compiler* +int1+ +char1+)))
    (assert (eq (type-of result) 'int-const))
    (assert (eql (value result) -5))))

(defun test10 ()
  (let ((result (c- *target-compiler* +int1+ +double1+)))
    (assert (eq (type-of result) 'double-const))
    (assert (= (value result) 122.0))))

(defun test11 ()
  (let ((result (c- *target-compiler* +int1+ +float1+)))
    (assert (eq (type-of result) 'cfloat-const))
    (assert (= (value result) 121.0))))

(defun test12 ()
  (let ((result (c- *target-compiler* +unsigned1+ +long1+)))
    (assert (eq (type-of result) 'unsigned-long-const))
    (assert (= (value result) 4293756185))))

(defun test13 ()
  (let ((result (c- *target-compiler* +long2+ +unsigned2+)))
    (assert (eq (type-of result) 'unsigned-long-const))
    (assert (= (value result) 2147480899))))

(defun test14 ()
  (let ((result (c- *target-compiler* +int3+ +long3+)))
    (assert (eq (type-of result) 'long-const))
    (assert (= (value result) -2147483646))))

(defun test15 ()
  (let ((result (c* *target-compiler* +float1+ +int1+)))
    (assert (eq (type-of result) 'cfloat-const))
    (assert (= (value result) 246.0))))

(defun test16 ()
  (let ((result (c/ *target-compiler* +int2+ +int1+)))
    (assert (eq (type-of result) 'int-const))
    (assert (eql (value result) 3))))

(defun test17 ()
  (let ((result (c/ *target-compiler* +double1+ +float1+)))
    (assert (eq (type-of result) 'double-const))
    (assert (= (value result) .5))))

(defparameter +int4+ (make-instance 'int-const :value -1))
(defparameter +int5+ (make-instance 'int-const :value 1))

(defun test18 ()
  (let ((result (c>> *target-compiler* +int4+ +int5+)))
    (assert (eq (type-of result)  'int-const))
    (assert (eql (value result) -1))))

