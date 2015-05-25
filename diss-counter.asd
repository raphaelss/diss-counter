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

(in-package :asdf-user)

(defsystem "diss-counter"
  :description "Dissonant counterpoint algorithm implementation"
  :version "0.0.1"
  :author "Raphael Santos <contact@raphaelss.com>"
  :license "AGPLv3 License"
  :components ((:file "diss-counter")))
