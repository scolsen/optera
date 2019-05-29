(in-package :cl-user)
(defpackage :optera
  (:use 
    :cl)
  (:export 
    :defoptions
    :defoption
    :defcli
    :*CLI*))
(in-package :optera)

(defvar *CLI*)

(defstruct option 
  short
  long
  args
  default-arg
  coercion
  required
  validity
  description)

(defstruct cli
  name
  help
  version
  options)

(defun defoption 
  (short long &key (args nil) (default-arg nil) 
              (coercion (lambda (x) x)) (required nil)
              (validity (lambda (x) t)) (description "")) 
  "Define a command line option"
  (cond ((not (typep short 'character)) 
         (error "Option short identifiers must be characters."))
        ((not (typep long 'string)) 
         (error "Option long identifiers must be strings."))
        ((not (typep args 'boolean)) 
         (error "Arg setting must be a boolean."))
        ((not (typep coercion 'function)) 
         (error "Option coercion checks must be functions."))
        ((not (typep required 'boolean)) 
         (error "Required setting must be a boolean."))
        ((not (typep validity 'function)) 
         (error "Option validity checks must be functions."))
        ((not (typep description 'string)) 
         (error "Option descriptions must be strings."))
        (t (make-option :short short
               :long long
               :args args
               :default-arg default-arg
               :coercion coercion
               :required required
               :validity validity
               :description description))))

(defun defoptions (&rest options)
  "Define command line options."
  (labels ((defoptions (os acc)
           (flet ((accumulate () (list* (apply #'defoption (first os)) acc)))
           (case (rest os)
                 ((nil) (funcall #'accumulate))
                 (otherwise (defoptions (rest os) (funcall #'accumulate)))))))
    (defoptions options (list))))

(defun defcli (name help version &rest options)
  "Define a command line interface."
  (defvar *CLI* (make-cli :name name
                          :help help
                          :version version
                          :options (apply #'defoptions options))))

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
