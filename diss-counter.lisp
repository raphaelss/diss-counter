(defpackage #:diss-counter
  (:use :cl)
  (:export #:diss-counter #:next #:diss-counter))

(in-package #:diss-counter)

(defun square-count (x count)
  (declare (ignore x)
           (double-float count))
  (the double-float (* count count)))

(defstruct entry
  elem
  (count 0.0d0 :type double-float)
  (prob 0.0d0 :type double-float))

(defclass diss-counter ()
  ((elems
    :accessor elems)
   (prob-fun
    :initarg :prob-fun
    :initform #'square-count
    :accessor prob-fun)
   (prob-sum
    :accessor prob-sum
    :type double-float)))

(defmethod initialize-instance :after ((dc diss-counter) &key objs)
  (let* ((prob-fun (slot-value dc 'prob-fun))
         (size (length objs))
         (sum 0.0d0)
         (table (map-into (make-array size)
                          #'(lambda (x)
                              (let ((prob (funcall prob-fun x 1.0d0)))
                                (declare (double-float prob))
                                (incf sum prob)
                                (make-entry :elem x :count 1.0d0
                                            :prob prob)))
                          objs)))
    (setf (slot-value dc 'elems) table
          (slot-value dc 'prob-sum) sum)))

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
