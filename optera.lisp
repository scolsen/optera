(in-package :cl-user)
(defpackage :optera
  (:use 
    :cl
    :optera.option
    :optera.cli)
  (:export 
    :defoptions
    :defoption
    :defcli
    :*CLI*))
(in-package :optera)

(defvar *CLI*)

;;(defun valid-short? (ch option) 
;;  (eql ch (option-short option)))
;;
;;(defun valid-long? (str option) 
;;  (equal str (option-long option)))
;;

(defun valid-arg? (arg option)
  "Checks whether an option argument is valid."
  (let ((f (option-coercion option))
         (g (option-validity option)))
    (funcall g (funcall f arg))))
