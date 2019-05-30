(in-package :cl-user)
(defpackage :optera.option
  (:use 
    :cl)
  (:export 
    :option
    :defoption
    :defoptions))
(in-package :optera.option)

(deftype kind-type ()
  '(member :flag :argument))

(defstruct option 
  (short #\- :type character) 
  (long "" :type string) 
  (required nil :type boolean)
  (kind :flag :type kind-type) 
  default
  (coercion (lambda (x) x) :type function)
  (validity (lambda (x) t) :type function)
  (description "" :type string))

(defun defoption (short long 
                  &key kind default 
                  coercion required
                  validity description)
  "Define a command line option"
  (let ((opt (make-option :short short :long long)))
    (progn
      (when kind (setf (option-kind opt) kind))
      (when default (setf (option-default opt) default))
      (when coercion (setf (option-coercion opt) coercion))
      (when required (setf (option-required opt) required))
      (when validity (setf (option-validity opt) validity))
      (when description (setf (option-description opt) description))
      opt)))

(defun defoptions (&rest options)
  "Define command line options."
  (labels ((defoptions (os acc)
           (flet ((accumulate () (list* (apply #'defoption (first os)) acc)))
           (case (rest os)
                 ((nil) (funcall #'accumulate))
                 (otherwise (defoptions (rest os) (funcall #'accumulate)))))))
    (defoptions options (list))))

