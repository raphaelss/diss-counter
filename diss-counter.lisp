;;; A dissonant counterpoint algorithm implementation in Common Lisp
;;; Copyright (C) 2015 Raphael Santos, http://www.raphaelss.com
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:diss-counter
  (:use :cl)
  (:export #:diss-counter #:next #:prob-fun #:reset-counts #:map-dc #:dcref
           #:size #:square-count #:prob-sum #:dcref-count #:elem-count))

(in-package #:diss-counter)

(defun square-count (x count)
  "Default prob-fun that returns the square of the count as probability."
  (declare (ignore x)
           (double-float count))
  (the double-float (* count count)))

(defstruct entry
  elem
  (count 0.0d0 :type double-float)
  (prob 0.0d0 :type double-float))

(defstruct (diss-counter (:constructor %make-diss-counter (elems prob-fun)))
  elems
  (prob-fun #'square-count)
  (prob-sum 0.0d0 :type double-float))

(defmacro do-entries ((var dc &optional result) &body body)
  "Iterates over entries of a diss-counter object."
  (let ((arr (gensym "ELEM-ARRAY")))
    `(let ((,arr (slot-value 'elems ,dc)))
       (dotimes (i (length ,arr) ,result)
         (let ((,var (aref ,arr i)))
           ,@body)))))

(defun make-diss-counter (elems &optional (prob-fun #'square-count

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
  "Calls f on the object and current count of entry to update its probability.
Returns the probability."
  (setf (entry-prob entry)
        (funcall f (entry-elem entry) (entry-count entry))))

(defun entry-update-count (entry count f)
  "Sets the entry count and calls f on the object and count of entry to update
its probability. Returns the probability."
  (setf (entry-count entry) count)
  (entry-update-prob entry f))

(defun entry-increase (entry f)
  "Sets count of entry to (1+ count) and updates its probability using f.
Returns the probability."
  (entry-update-count entry (1+ (entry-count entry)) f))

(defun entry-zero (entry f)
  "Sets count of entry to 0 and updates its probability using f. Returns the
probability."
  (entry-update-count entry 0.0d0 f))

(defun next (dc &optional (random-state *random-state*))
  (with-slots (elems prob-fun prob-sum) dc
    (let ((i (1- (array-total-size elems)))
          (new-sum 0.0d0))
      (let ((chosen
             (do* ((entry (svref elems i) (svref elems i))
                   (r (- (random prob-sum random-state)
                         (entry-prob entry))
                      (- r (entry-prob entry))))
                  ((> r 0.0d0) entry)
               (incf new-sum (entry-increase entry prob-fun))
               (decf i))))
        (incf new-sum (entry-zero chosen prob-fun))
        (do ()
            ((>= i 0) (setf prob-sum new-sum) (entry-elem chosen))
          (incf new-sum (entry-increase (svref elems i) prob-fun))
          (decf i))))))

(defun update-probs (dc)
  "Updates the probability of every entry in the diss-counter object and its
prob-sum."
  (let ((prob-sum 0.0d0)
        (f (prob-fun dc)))
    (do-entries (entry dc)
      (incf prob-sum (entry-update-prob entry f)))
    (setf (slot-value 'prob-sum dc) prob-sum)))

(defun (setf prob-fun) (f dc)
  "Sets the prob-fun used by the diss-counter object and updates probabilities."
  (setf (slot-value 'prob-fun dc) f)
  (update-probs dc))

(defun reset-counts (dc &optional (count 1))
  "Sets the count of every element to count."
  (let ((prob-sum 0.0d0)
        (f (prob-fun dc)))
    (do-entries (entry dc)
      (incf prob-sum (entry-update-count entry count f)))
    (setf (slot-value 'prob-sum dc) prob-sum)))

(defun size (dc)
  "Returns the number of elements held in the diss-counter object."
  (length (slot-value 'elems dc)))

(defun dcref (dc i)
  "Returns (values elem count prob) of the element entry at index i."
  (let ((entry (aref (slot-value dc 'elems) i)))
    (values (entry-elem entry) (entry-count entry) (entry-prob entry))))

(defun dcref-count (dc i)
  "Returns the count of the element at index i."
  (entry-count (aref (slot-value dc 'elems) i)))

(defun update-count (dc entry count)
  (let ((old (entry-prob entry))
        (new (entry-update-count entry count (prob-fun dc))))
    (incf (slot-value dc 'prob-sum)
          (- new old))))

(defun (setf dcref-count) (count dc i)
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
  "Returns the count of elem or nil if elem is not present in dc."
  (let ((entry (find-entry elem dc test key)))
    (when entry
      (entry-count entry))))

(defun (setf elem-count) (count dc elem &key (test #'eql) key)
  "Sets the count of elem and updates its probability if elem is present in dc.
Returns the probability or nil if elem is not present in dc."
  (let ((entry (find-entry elem dc test key)))
    (when entry
      (update-count dc entry count))))

(defun map-dc (dc f)
  "Applies f as in (f elem count probality) to every element in the obj."
  (do-entries (entry dc dc)
    (funcall f (entry-elem entry) (entry-count entry) (entry-prob entry))))
