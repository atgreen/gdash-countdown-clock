;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2020  Anthony Green <green@moxielogic.com>
;;;                         
;;; gdash-countdown-clock is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; gdash-countdown-clock is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with gdash-countdown-clock; see the file COPYING3.  If not
;;; see <http://www.gnu.org/licenses/>.

(asdf:defsystem #:gdash-countdown-clock
  :description "An HTML countdown clock for Google Calendar meetings"
  :author "Anthony Green <green@moxielogic.com>"
           :version "0"
  :serial t
  :components ((:file "package")
	       (:file "gdash-countdown-clock"))
  :depends-on (:hunchentoot :spinneret :smackjack :cl-stomp :cl-base64 :log4cl))

