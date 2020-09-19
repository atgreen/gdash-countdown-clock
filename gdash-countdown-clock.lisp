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

;; Hostname for our ActiveMQ broker.  We normally run in kubernetes,
;; so this is just the service name.
(defparameter +amq-host+ "amq-broker")

;; The ActiveMQ topic for our messages.  Messages are sent from a
;; container that is able to pull the Google calendar with gcalcli and
;; send the results with stomp.py.  See
;; https://github.com/atgreen/gdash-gcal-poll
(defparameter +gcal-agenda+ "/topic/gcal-agenda")

;; The SmackJack package enables AJAX-like calls from this server into
;; parenscript code running in the browser.  SmackJack polls our
;; server at this URI.
(defparameter +ajax-pusher+
  (make-instance 'smackjack:ajax-pusher :server-uri "/ajax-push"))

;; Remember the last matching event.  This is used by new connections.
(defvar *timestring* 0)

(defun getenv (var)
  "Getenv with an error if the variable is not defined."
  (let ((val (uiop:getenv var)))
    (when (null val)
      (error "Environment variable ~A is not set." var))
    val))

;; (defun root-dir ()
;;   "Where are we installed?  Use this to serve up our CSS content."
;;   (fad:pathname-as-directory
;;    (make-pathname :name nil
;;                   :type nil
;;                   :defaults #.(or *compile-file-truename* *load-truename*))))

(defun-push push-next-meeting (datestring) (+ajax-pusher+)
  "Parenscript code we call from the server when we have an agenda
   update.  This is injected into the browser as javascript."
  (setf *deadline* (if (equal 0 datestring)
		       nil
		       (ps:chain -Date (parse datestring)))))

(defun gcal-agenda-callback (hunchentoot-server frame)
  "Callback on receiving ActiveMQ messages on our topic.  Parse the
   message, which is tab separated agenda output from gcalcli, to find
   the next meeting based on the current time."
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
		      (let ((hunchentoot:*acceptor* hunchentoot-server))
			(push-next-meeting (setf *timestring* timestring))
			t)
		      nil))
	  finally (unless line
		    ;; There's no next meeting.
		    (let ((hunchentoot:*acceptor* hunchentoot-server))
		      (push-next-meeting (setf *timestring* 0)))))))

;; Start the web app.
(defun start-gdash-countdown-clock ()
  "Start the web application and start the AMQ connection."

  ;; Set the default timezone based on ${TZ} (eg. America/Toronto)
  (local-time:reread-timezone-repository)

  (setf local-time:*default-timezone* (local-time:find-timezone-by-location-name (getenv "TZ"))
	hunchentoot:*show-lisp-errors-p* t
	hunchentoot:*show-lisp-backtraces-p* t)

  (log:info "** Starting hunchentoot on 8080")

  (let ((stomp (stomp:make-connection +amq-host+ 61613))
	(hunchentoot-server (hunchentoot:start
			     (make-instance 'hunchentoot:easy-acceptor
					    :port 8080))))
    (reset-session-secret)
    (push (create-ajax-dispatcher +ajax-pusher+) *dispatch-table*)
    (push (hunchentoot:create-folder-dispatcher-and-handler
	   "/css/"
           (merge-pathnames "css/"
                            (asdf:system-source-directory :gdash-countdown-clock)))
	  *dispatch-table*)

    (stomp:register stomp (lambda (frame)
			    (gcal-agenda-callback hunchentoot-server frame))
		    +gcal-agenda+)

    (stomp:start stomp)))

(defun countdown-js ()
  "Parenscript code that is injected into the HTML page as javascript.
  This implements the countdown timer.  CSS changes based on 5min and
  2min warnings are implemented by changing the document body class,
  and providing CSS content keyed on that."
  (ps
    (defparameter +two-minutes+ (* 1000 60 2))
    (defparameter +five-minutes+ (* 1000 60 5))
    (defvar *deadline* (ps:chain -Date (parse (ps:lisp *timestring*))))

    (defun get-time-remaining ()
      (let* ((total (- *deadline* (ps:chain -Date (parse (ps:new (-Date))))))
	     (seconds (floor (mod (/ total 1000) 60)))
	     (minutes (floor (mod (/ (/ total 1000) 60) 60)))
	     (hours (floor (/ total (* 1000 60 60)))))
	(values total seconds minutes hours)))

    (defun initialize-clock (id endtime)
      (let* ((clock ((@ document get-element-by-id) id))
	     (hour-span ((@ clock query-selector) ".hours"))
	     (minute-span ((@ clock query-selector) ".minutes"))
	     (second-span ((@ clock query-selector) ".seconds")))
	(flet ((update-clock ()
		 (if *deadline*
		     (multiple-value-bind (total seconds minutes hours)
			 (get-time-remaining)
		       (if (< total +two-minutes+)
			   (setf (@ document body class-name) "WithinTwoMinutes")
			   (if (< total +five-minutes+)
			       (setf (@ document body class-name) "WithinFiveMinutes")
			       (setf (@ document body class-name) "")))
		       (setf (inner-html hour-span) ((@ (+ "0" hours) slice) -2))
		       (setf (inner-html minute-span) ((@ (+ "0" minutes) slice) -2))
		       (setf (inner-html second-span) ((@ (+ "0" seconds) slice) -2)))
		     (dolist (span (list hour-span minute-span second-span))
		       (setf (inner-html span) "--")))))
	  (update-clock)
	  (set-interval update-clock 1000))))

    (initialize-clock "clockdiv" *deadline*)))

(hunchentoot:define-easy-handler (clock :uri "/") ()
  "Generate our simple HTML page with parenscript code translated to
  javascript.  We initialize the AJAX magic 'onload', which triggers
  calls to PUSH-NEXT-MEETING when we have an agenda update."
  (with-html-string
    (:doctype)
    (:html
     (:head
      (:link :rel "stylesheet" :href "css/gdash-countdown-clock.css")
      (:raw (generate-prologue +ajax-pusher+)))
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
