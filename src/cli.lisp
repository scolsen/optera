(in-package :cl-user)
(defpackage :optera.cli
  (:use
    :cl
    :optera.option)
  (:export
    :defcli))
(in-package :optera.cli)

(deftype bad-option-handler ()
  '(member :error :ignore))

(defstruct cli
  (name "" :type string) 
  (help "" :type string) 
  (version "" :type string)
  options 
  (on-bad-option :error :type bad-option-handler))

(defun defcli (name &key help version on-bad-option options)
  "Define a command line interface."
  (let ((cli (make-cli :name name))) 
    (progn 
      (when help (setf (cli-help cli) help))
      (when version (setf (cli-version cli) version))
      (when on-bad-option (setf (cli-on-bad-option cli) on-bad-option))
      (when options (setf (cli-options cli) (apply #'defoptions options)))
      (defvar *CLI* cli))))
