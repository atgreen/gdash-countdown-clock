;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GDASH-COUNTDOWN-CLOCK; Base: 10 -*-
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

;; Top level for gdash-countdown-clock

(in-package :gdash-countdown-clock)

;; Our server....

(defvar *hunchentoot-server* nil)

(defvar *stomp* nil)
(defparameter *amq-host* "amq-broker")

(defparameter *gcal-agenda* "/topic/gcal-agenda")

(defparameter *ajax-pusher*
  (make-instance 'smackjack:ajax-pusher :server-uri "/ajax-push"))

(defun-push push-next-meeting () (*ajax-pusher*)
  (initialize-clock "clockdiv" (ps:new (-Date (+ (* 1000 (* 60 (* 60 (* 24 10))))
                                                 (ps:chain -Date (parse (ps:new -Date))))))))

(defun gcal-agenda-callback (frame)
  (log:info ">> [~a]~%" (stomp:frame-body frame))
  (let ((hunchentoot:*acceptor* *hunchentoot-server*))
    (push-next-meeting)))

;; Start the web app.
(defun start-gdash-countdown-clock ()
  "Start the web application and start the AMQ connection."
  (format t "** Starting hunchentoot on 8080~%")
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (setf *stomp* (stomp:make-connection *amq-host* 61613))
  (setq *hunchentoot-server* (hunchentoot:start 
			      (make-instance 'hunchentoot:easy-acceptor 
					     :port 8080)))
  (reset-session-secret)
  (push (create-ajax-dispatcher *ajax-pusher*) *dispatch-table*)
  (stomp:register *stomp* #'gcal-agenda-callback *gcal-agenda*)
  (stomp:start *stomp*))

(defun stop-gdash-countdown-clock ()
  "Stop the web application."
  (hunchentoot:stop *hunchentoot-server*))

(defun countdown-js ()
  (parenscript:ps
    (defun get-time-remaining (endtime)
      (let* ((total (- (ps:chain -Date (parse endtime))
		       (ps:chain -Date (parse (ps:new (-Date))))))
	     (seconds (floor (mod (/ total 1000) 60)))
	     (minutes (floor (mod (/ (/ total 1000) 60) 60)))
	     (hours (floor (mod (/ total (* 1000 60 60)) 24)))
	     (days (floor (/ total (* 1000 60 60 24)))))
	(values total seconds minutes hours days)))
    
    (defun initialize-clock (id endtime)
      (let* ((clock ((ps:@ document get-element-by-id) id))
	     (day-span ((ps:@ clock query-selector) ".days"))
	     (hour-span ((ps:@ clock query-selector) ".hours"))
	     (minute-span ((ps:@ clock query-selector) ".minutes"))
	     (second-span ((ps:@ clock query-selector) ".seconds")))
	(flet ((update-clock ()
		 (multiple-value-bind (total seconds minutes hours days)
		     (get-time-remaining endtime)
		   (let ((rt (get-time-remaining endtime)))
		     (setf (ps:inner-html day-span) days)
		     (setf (ps:inner-html hour-span) ((ps:@ (+ "0" hours) slice) -2))
		     (setf (ps:inner-html minute-span) ((ps:@ (+ "0" minutes) slice) -2))
		     (setf (ps:inner-html second-span) ((ps:@ (+ "0" seconds) slice) -2))
					;				   (when (<= total 0)
					;				     (clear-interval timeinterval)))))
		     ))))
	  (update-clock)
	  (set-interval update-clock 1000))))
	
    (defvar *deadline* (ps:new (-Date (+ (* 1000 (* 60 (* 60 (* 24 .001)))) (ps:chain -Date (parse (ps:new -Date)))))))
    (initialize-clock "clockdiv" *deadline*)))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)

  (hunchentoot:define-easy-handler (say-yo :uri "/clock") (name)
    (spinneret:with-html-string
	(:doctype)
      (:html
       (:head
	(:link :rel "stylesheet" :href "gdash-countdown-clock.css")
	(:raw (generate-prologue *ajax-pusher*)))
       (:body
	:onload (ps-inline (chain smackpusher (start-poll)))
	(:h1 "Countdown Clock")
	(:div :id "clockdiv"
	      (:div (:span :class "days")
		    (:div :class "smalltext" "Days"))
	      (:div (:span :class "hours")
		    (:div :class "smalltext" "Hours"))
	      (:div (:span :class "minutes")
		    (:div :class "smalltext" "Minutes"))
	      (:div (:span :class "seconds")
		    (:div :class "smalltext" "Seconds"))))
       	(:script (countdown-js)))))
  
  (hunchentoot:define-easy-handler (status :uri "/status") ()
    (setf (hunchentoot:content-type*) "text/plain")
    (format nil "It's all good"))

  )
