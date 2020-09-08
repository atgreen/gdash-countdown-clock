;; -*- mode: lisp; indent-tabs-mode: nil; -*-
;;
;; Available under MIT-style License. see COPYING.
;;

(in-package :asdf)

(defsystem :cl-stomp
  :description "Implements the STOMP protocol for connecting to a message broker."
  :author "Keith Irwin, Matt Reklaitis"
  :licence "MIT-style License"
  :depends-on (:usocket :babel :bordeaux-threads)
  :components ((:file "cl-stomp")))
