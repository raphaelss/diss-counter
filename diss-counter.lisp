(defpackage #:diss-counter
  (:use :cl)
  (:export #:diss-counter #:next #:prob-fun #:reset-counts #:map-dc #:ref
           #:size #:square-count #:prob-sum #:ref-count #:elem-count))

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
  ((elems)
   (prob-fun
    :initarg :prob-fun
    :initform #'square-count
    :reader prob-fun)
   (prob-sum
    :accessor prob-sum
    :type double-float)))

(defmacro do-entries ((var dc &optional return) &body body)
  (let ((arr (gensym "ELEM-ARRAY")))
    `(let ((,arr (slot-value 'elems ,dc)))
       (dotimes (i (length ,arr) ,return)
         (let ((,var (aref ,arr i)))
           ,@body)))))

(defmethod initialize-instance :after ((dc diss-counter) &key objs)
  (let* ((prob-fun (slot-value dc 'prob-fun))
         (size (length objs))
         (sum 0.0d0)
         (table (map-into (make-array size)
                          #'(lambda (x)
                              (let ((prob (funcall prob-fun x 1.0d0)))
                                (declare (double-float prob))
                                (incf sum prob)
                                (make-entry :elem x :count 1.0d0 :prob prob)))
                          objs)))
    (setf (slot-value dc 'elems) table
          (slot-value dc 'prob-sum) sum)))

(defun entry-update-prob (entry f)
  (setf (entry-prob entry)
        (funcall f (entry-elem entry) (entry-count entry))))

(defun entry-update-count (entry count f)
  (setf (entry-count entry) count)
  (entry-update-prob entry f))

(defun entry-increase (entry f)
  (entry-update-count entry (1+ (entry-count entry)) f))

(defun entry-zero (entry f)
  (entry-update-count entry 0.0d0 f))

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

(defun update-probs (dc)
  (let ((prob-sum 0.0d0)
        (f (prob-fun dc)))
    (do-entries (entry dc)
      (incf prob-sum (entry-update-prob entry f)))
    (setf (slot-value 'prob-sum dc) prob-sum)))

(defun (setf prob-fun) (f dc)
  (setf (slot-value 'prob-fun dc) f)
  (update-probs dc))

(defun reset-counts (dc &optional (count 1))
  (let ((prob-sum 0.0d0)
        (f (prob-fun dc)))
    (do-entries (entry dc)
      (incf prob-sum (entry-update-count entry count f)))
    (setf (slot-value 'prob-sum dc) prob-sum)))


(defun size (dc)
  (length (slot-value 'elems dc)))

(defun ref (dc i)
  (let ((entry (aref (slot-value dc 'elems) i)))
    (values (entry-elem entry) (entry-count entry) (entry-prob entry))))

(defun ref-count (dc i)
  (entry-count (aref (slot-value dc 'elems) i)))

(defun update-count (dc entry count)
  (let ((old (entry-prob entry))
        (new (entry-update-count entry count (prob-fun dc))))
    (incf (slot-value dc 'prob-sum)
          (- new old))))

(defun (setf ref-count) (count dc i)
  (let ((entry (aref (slot-value dc 'elems) i)))
    (update-count dc entry count)))

(defun find-entry (elem dc test key)
  (if key
      (let ((elem-key (funcall key elem)))
        (do-entries (entry dc)
          (when (funcall test elem-key (funcall key (entry-elem entry)))
            (return-from find-entry entry))))
      (do-entries (entry dc)
        (when (funcall test elem (entry-elem entry))
          (return-from find-entry entry)))))

(defun elem-count (dc elem &key (test #'eql) key)
  (let ((entry (find-entry elem dc test key)))
    (when entry
      (entry-count entry))))

(defun (setf elem-count) (count dc elem &key (test #'eql) key)
  (let ((entry (find-entry elem dc test key)))
    (when entry
      (update-count dc entry count))))

(defun map-dc (dc f)
  (do-entries (entry dc dc)
    (funcall f (entry-elem entry) (entry-count entry) (entry-prob entry))))
