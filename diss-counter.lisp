(defpackage #:diss-counter
  (:use :cl)
  (:export #:make #:next #:diss-counter))

(in-package #:diss-counter)

(defclass diss-counter ()
  ((elems
    :initarg :elems
    :accessor elems)
   (prob-fun
    :initarg :prob-fun
    :accessor prob-fun)
   (prob-sum
    :initarg :prob-sum
    :accessor prob-sum
    :type double-float)))

(defstruct entry
  elem
  (count 0.0d0 :type double-float)
  (prob 0.0d0 :type double-float))

(defun power-fun (x count)
  (declare (ignore x)
           (double-float count))
  (the double-float (* count count)))

(defun make (elems &key (prob-fun #'power-fun) (initial-count 1.0d0))
  (declare (optimize speed))
  (let* ((size (length elems))
         (sum 0.0d0)
         (table (map-into (make-array size)
                          #'(lambda (x)
                              (let ((prob (funcall prob-fun x initial-count)))
                                (declare (double-float prob))
                                (incf sum prob)
                                (make-entry :elem x :count initial-count
                                            :prob prob)))
                          elems)))
    (make-instance 'diss-counter :elems table :prob-fun prob-fun
                   :prob-sum sum)))

(defun entry-increase (entry f)
  (incf (entry-count entry))
  (setf (entry-prob entry)
        (funcall f (entry-elem entry) (entry-count entry))))

(defun entry-zero (entry f)
  (setf (entry-count entry) 0.0d0
        (entry-prob entry)
        (funcall f (entry-elem entry) 0.0d0)))

(defun next (dc)
  (with-slots (elems prob-fun prob-sum) dc
    (let ((r (random prob-sum))
          (chosen nil))
      (setf prob-sum 0)
      (dotimes (i (array-total-size elems))
        (let ((entry (svref elems i)))
          (incf prob-sum
                (cond
                  (chosen
                   (entry-increase entry prob-fun))
                  ((< (decf r (entry-prob entry)) 0)
                   (setf chosen (entry-elem entry))
                   (entry-zero entry prob-fun))
                  (t
                   (entry-increase entry prob-fun))))))
      chosen)))

(defun time-dc (dc iter-n)
  (time (dotimes (i iter-n)
          (next dc))))
