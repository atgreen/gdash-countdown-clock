;; -*- mode: lisp; -*-

(in-package :asdf)

(defsystem :example
    :version "0.1.0"
    :depends-on (cl-stomp)
    :components ((:file "example")))
