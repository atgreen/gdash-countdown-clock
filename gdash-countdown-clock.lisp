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

(defun getenv (var)
  (let ((val (uiop:getenv var)))
    (when (null val)
      (error "Environment variable ~A is not set." var))
    val))

(defun-push push-next-meeting (datestring) (*ajax-pusher*)
  (setf *deadline* (if (equal 0 datestring)
		       nil
		       (ps:chain -Date (parse datestring)))))

(defun gcal-agenda-callback (frame)
  (let ((in (make-string-input-stream (cl-base64:base64-string-to-string (stomp:frame-body frame))))
	(now (local-time:now)))
    (loop for line = (read-line in nil)
	  while line
	  until (let* ((data (ppcre:split #\tab line))
		       (timestring-gmt (format nil "~AT~A:00.000000" (car data) (cadr data)))
		       (timestamp-gmt (local-time:parse-timestring timestring-gmt))
		       (timestring (format nil "~A~A"
					  timestring-gmt
					  (local-time:format-timestring nil timestamp-gmt
									:format '(:gmt-offset))))
		       (timestamp (local-time:parse-timestring timestring)))
		  (if (local-time:timestamp>= timestamp now)
		      (progn
			(log:info ">> matching ~a" line)
			(let ((hunchentoot:*acceptor* *hunchentoot-server*))
			  (push-next-meeting timestring)
			  t))
		      nil))
	  finally (unless line
		    (let ((hunchentoot:*acceptor* *hunchentoot-server*))
		      (push-next-meeting 0))))))
			
(defun root-dir ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

;; Start the web app.
(defun start-gdash-countdown-clock ()
  "Start the web application and start the AMQ connection."

  ;; Set the default timezone based on ${TZ} (eg. America/Toronto)
  (local-time:reread-timezone-repository)
  (setf local-time:*default-timezone*
	(local-time:find-timezone-by-location-name (getenv "TZ")))
  
  (format t "** Starting hunchentoot on 8080~%")
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (setf *stomp* (stomp:make-connection *amq-host* 61613))
  (setq *hunchentoot-server* (hunchentoot:start 
			      (make-instance 'hunchentoot:easy-acceptor 
					     :port 8080))) 
  (reset-session-secret)
  (push (create-ajax-dispatcher *ajax-pusher*) *dispatch-table*)
  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/css/" (fad:pathname-as-directory
		  (make-pathname :name "css"
				 :defaults (root-dir))))
	*dispatch-table*)

  (stomp:register *stomp* #'gcal-agenda-callback *gcal-agenda*)
  (stomp:start *stomp*))
  
(defun stop-gdash-countdown-clock ()
  "Stop the web  application."
  (hunchentoot:stop *hunchentoot-server*))
 
(defun countdown-js ()
  (parenscript:ps

    (defun get-time-remaining ()
      (let* ((total (- *deadline* (ps:chain -Date (parse (ps:new (-Date))))))
	     (seconds (floor (mod (/ total 1000) 60)))
	     (minutes (floor (mod (/ (/ total 1000) 60) 60)))
	     (hours (floor (/ total (* 1000 60 60)))))
	(values total seconds minutes hours)))

    (defparameter +two-minutes+ (* 1000 60 2))
    (defparameter +five-minutes+ (* 1000 60 5))
    
    (defun initialize-clock (id endtime)
      (let* ((clock ((ps:@ document get-element-by-id) id))
	     (hour-span ((ps:@ clock query-selector) ".hours"))
	     (minute-span ((ps:@ clock query-selector) ".minutes"))
	     (second-span ((ps:@ clock query-selector) ".seconds")))
	(flet ((update-clock ()
		 (if *deadline*
		     (multiple-value-bind (total seconds minutes hours)
			 (get-time-remaining)
		       (if (< total +two-minutes+)
			   (setf (ps:@ document body class-name) "WithinTwoMinutes")
			   (if (< total +five-minutes+)
			       (setf (ps:@ document body class-name) "WithinFiveMinutes")
			       (setf (ps:@ document body class-name) "")))
		       (setf (ps:inner-html hour-span) ((ps:@ (+ "0" hours) slice) -2))
		       (setf (ps:inner-html minute-span) ((ps:@ (+ "0" minutes) slice) -2))
		       (setf (ps:inner-html second-span) ((ps:@ (+ "0" seconds) slice) -2)))
		     (dolist (span (list hour-span minute-span second-span))
		       (setf (ps:inner-html span) "--")))))
	  (update-clock)
	  (set-interval update-clock 1000))))
	
    (defvar *deadline* nil)
    
    (initialize-clock "clockdiv" *deadline*)))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)

  (hunchentoot:define-easy-handler (clock :uri "/") (name)
    (spinneret:with-html-string
	(:doctype)
      (:html
       (:head
	(:link :rel "stylesheet" :href "css/gdash-countdown-clock.css")
	(:raw (generate-prologue *ajax-pusher*)))
       (:body
	:onload (ps-inline (chain smackpusher (start-poll)))
	(:h1 "Next Meeting")
	(:div :id "clockdiv"
	      (:div (:span :class "hours")
		    (:div :class "smalltext" "Hours"))
	      (:div (:span :class "minutes")
		    (:div :class "smalltext" "Minutes"))
	      (:div (:span :class "seconds")
		    (:div :class "smalltext" "Seconds"))))
       	(:script (:raw (countdown-js))))))
  )

