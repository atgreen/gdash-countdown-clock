;;; -*- mode: lisp; indent-tabs-mode: nil; -*-
;;;
;;; Available under MIT-style License. see COPYING.
;;;

(defpackage :cl-stomp

  (:nicknames :stomp)

  (:use :cl
        :cl-user)

  (:export :make-connection
           :start
           :stop
           :frame
           :frame-body
           :frame-name
           :frame-headers
           :get-header
           :set-header
           :get-destination
           :set-destination
           :pass-through-key-mapping-strategy
           :amq-key-mapping-strategy
           :register
           :unregister-id
           :register-error
           :subscribe
           :unsubscribe
           :unsubscribe-id
           :post
           :ack
           :begin
           :commit
           :rollback))

(in-package :cl-stomp)


;;;-------------------------------------------------------------------------
;;; Convenience utils

#-nil
(defun log-debug (fmt &rest args)
  (fresh-line *standard-output*)
  (apply #'format *standard-output* fmt args)
  (fresh-line *standard-output*)
  (force-output *standard-output*))

#+nil
(defun log-debug (fmt &rest args)
  (declare (ignore fmt args)))

(defun string-strip (string)
  "Remove spaces, tabs and line enders from both ends of a string."
  (check-type string string)
  (string-trim '(#\Space #\NewLine #\Return #\Tab #\Nul #\Linefeed) string))

(defun string-left-strip (string)
  "Remove spaces, tabs and line enders from the beginning of a string."
  (check-type string string)
  (string-left-trim '(#\Space #\NewLine #\Return #\Tab #\Nul #\Linefeed) string))

(defun string-split (string delim)
  "Splits STRING at the first occurrence of DELIM and returns the substrings before and after it.
   If DELIM is not found in STRING, returns STRING and NIL."
  (when string
    (let ((start (search delim string :test 'string=)))
      (if (null start)
        (values string nil)
        (let ((start2 (min (length string) (+ (length delim) start))))
          (values (subseq string 0 start)
                  (subseq string start2)))))))

;;;-------------------------------------------------------------------------
;;; The CL-STOMP API

(defgeneric get-header (frame key)
  (:documentation "Return the value of the header named KEY, or NIL if it doesn't exist."))

(defgeneric set-header (frame key value)
  (:documentation "Add a header named KEY to FRAME with VALUE.
                   If the header already exists, VALUE replaces the existing value."))

(defgeneric get-destination (frame)
  (:documentation "Return the destination header for FRAME."))

(defgeneric set-destination (frame destination)
  (:documentation "Set the destination header for FRAME."))

(defgeneric start (connection &key username passcode)
  (:documentation "Start listening for messages from STOMP."))

(defgeneric stop (connection)
  (:documentation "Stop the connection with STOMP."))

(defgeneric register (connection callback destination &key selector id client-ack?)
  (:documentation "Register a listener for messages to a destination.
                   CALLBACK should be a function which accepts a frame argument.
                   SELECTOR can be used to provide an SQL 92 selector for filtering
                   messages. An ID may be given for later use with UNREGISTER-ID
                   to support overlapping subscriptions using selectors with the
                   same destination. If CLIENT-ACK? is T, the client is responsible
                   for sending ACK."))

(defgeneric unregister-id (connection id)
  (:documentation "Unregister a callback by id."))

(defgeneric register-error (connection callback)
  (:documentation "Register a listener for STOMP error frames."))

(defgeneric subscribe (connection destination &key selector id client-ack?)
  (:documentation "Subscribe to a topic or queue.
                   SELECTOR can be used to provide an SQL 92 selector for filtering
                   messages. An ID may be given for later use with UNSUBSCRIBE-ID
                   to support overlapping subscriptions using selectors with the
                   same destination."))

(defgeneric unsubscribe (connection destination)
  (:documentation "Unsubscribe from a topic or queue by name.
                   Unsubscribing does not unregister any callbacks."))

(defgeneric unsubscribe-id (connection id)
  (:documentation "Unsubscribe from a topic or queue by id.
                   Unsubscribing does not unregister any callbacks."))

(defgeneric post (connection message destination &optional headers)
  (:documentation "Post a message to a destination.
                   HEADERS is an alist of header name and value pairs."))

(defgeneric ack (connection frame &optional transaction)
  (:documentation "Send the client an ACK for frame."))

(defgeneric begin (connection transaction)
  (:documentation "Start a transaction with the given name."))

(defgeneric commit (connection transaction)
  (:documentation "Commit a transaction with the given name."))

(defgeneric rollback (connection transaction)
  (:documentation "Abort a transaction with the given name."))


;;;-------------------------------------------------------------------------
;;; Frames

(defclass frame ()
  ((name :type string
         :initform "MESSAGE"
         :initarg :name
         :accessor frame-name)
   (headers :type list
            :initform ()
            :initarg :headers
            :accessor frame-headers)
   (body :type string
         :initform ""
         :initarg :body
         :accessor frame-body)))

(defun make-frame-from-string (string connection)
  "Construct a frame by parsing STRING according to the STOMP protocol."
  ;; Declare some useful local functions
  (labels ((get-line (stream)
             (let ((line (read-line stream nil 'eof)))
               (unless (eql line 'eof)
                 line)))
           (make-header (line)
             (with-slots (key-mapping-strategy) connection
               (multiple-value-bind (before after) 
                   (string-split line ":")
                 (list (demangle-key key-mapping-strategy (string-strip (string before))) 
                       (string-strip (string after))))))
           ;; Frame name is first line
           (get-name (stream)
             (get-line stream))
           ;; Frame headers are second lines through to empty line
           (get-headers (stream)
             (loop for line = (get-line stream)
                   while (> (length line) 0)
                   collect (make-header line)))
           ;; Frame body is all the lines after the empty line
           (get-body (stream)
             (coerce (loop for c = (read-char stream nil 'eof)
                           while (not (eql c 'eof))
                           collect c)
                     'string)))
    (with-input-from-string (stream string)
      (let ((name (get-name stream))
            (headers (get-headers stream))
            (body (get-body stream)))
        (make-instance 'frame
          :name name
          :headers headers
          :body body)))))

;; Makes a frame with the given name and headers,
;; evaluates the body,
;; and then sends the frame over the connection
(defmacro sending-frame ((connection vframe name &rest headers) &body body)
  `(let ((,vframe (make-instance 'frame :name ,name)))
     ,@(loop for (key val) on headers by #'cddr
             collect (list 'set-header vframe key val))
     (progn ,@body)
     (send ,connection ,vframe)))

(defmethod print-object ((frame frame) stream) 
  (with-slots (name headers body) frame
    (format stream "~A~%~A~%~A~%" name headers body)))

(defmethod render-frame ((frame frame) connection)
  (with-output-to-string (stream)
    (with-slots (name headers body) frame
      (format stream "~A~%" name)
      (with-slots (key-mapping-strategy) connection
        (dolist (header headers)
          (format stream 
                  "~A:~A~%" 
                  (first header)
                  ;;(mangle-key key-mapping-strategy (first header))
                  (second header)))
        (format stream "~%~A~A" body (code-char 0))))))
  
(defun header= (header1 header2)
  "Case insensitive comparison function for headers."
  (string-equal (string header1) (string header2)))

(defmethod get-header ((frame frame) (key string))
  "Return the value of the header named KEY, or NIL if it doesn't exist."
  (with-slots (headers) frame
    (second (assoc key headers :test #'header=))))

(defmethod set-header ((frame frame) (key string) value)
  "Add a header named KEY to FRAME with VALUE, which can be of any type.
   If the header already exists, VALUE replaces the existing value."
  (when value    
    (with-slots (headers) frame
      (if (not (assoc key headers :test #'header=))
        (setf headers (append (list (list key value)) headers))
        (let ((result ()))
          (dolist (header headers)
            (if (header= (first header) key)
              (push (list key value) result)
              (push header result)))
          (setf headers result))))))
  
(defmethod get-destination ((frame frame))
  "Return the destination header for FRAME."
  (get-header frame "destination"))

(defmethod set-destination ((frame frame) (destination string))
  "Set the destination header for FRAME."
  (set-header frame "destination" destination))

(defmethod get-subscription ((frame frame))
  "Get the subscription header for FRAME, if one exists."
  (get-header frame "subscription"))

(defmethod set-selector ((frame frame) (selector string))
  "Specify a 'selector' header for FRAME."
  (set-header frame "selector" selector))

(defmethod set-id ((frame frame) (id string))
  "Specify an 'id' header for FRAME."
  (set-header frame "id" id))

(defmethod set-client-ack ((frame frame))
  "Specify a 'client' ack header for FRAME."
  (set-header frame "ack" "client"))

(defmethod error-frame-p ((frame frame))
  (string-equal (frame-name frame) "error"))


;;;-------------------------------------------------------------------------
;;; Registrations

(defclass registration ()
  ((callback    :type (or null function) ;the callback function
                :initform nil
                :initarg :callback)
   (destination :type string             ;the topic/queue name
                :initarg :destination)
   (selector    :type (or null string)   ;an SQL 92 selector, if provided
                :initarg :selector)
   (id          :type (or null string)   ;a subscription id, if provided
                :initarg :id)
   (client-ack? :initarg :client-ack?))) ;use client (or auto) ack?


;;;-------------------------------------------------------------------------
;;; Key-mapping strategies

(defgeneric mangle-key (strategy key))
(defgeneric demangle-key (strategy key))


(defclass pass-through-key-mapping-strategy () ())

(defmethod mangle-key ((strategy pass-through-key-mapping-strategy) key)
  key)

(defmethod demangle-key ((strategy pass-through-key-mapping-strategy) key)
  key)


(defclass amq-key-mapping-strategy () ())

(defvar *replacement-pairs* 
  '(("-" . "_HYPHEN_")
    ("." . "_DOT_")))

(defmethod mangle-key ((strategy amq-key-mapping-strategy) key)
  (declare (ignore strategy))
  (let ((str key))
    (loop for pair in *replacement-pairs*          
          do (setf str (string-replace (car pair) (cdr pair) str)))    
    str))

(defmethod demangle-key ((strategy amq-key-mapping-strategy) key)
  (declare (ignore strategy))
  (let ((str key))
    (loop for pair in *replacement-pairs*          
          do (setf str (string-replace (cdr pair) (car pair) str)))    
    str))

(defun string-replace (search replace string)
  (loop for start = (search search (or result string)
                            :start2 (if start (1+ start) 0))
        while start
        as result = (concatenate 'string
                      (subseq (or result string) 0 start)
                      replace
                      (subseq (or result string) (+ start (length search))))
        finally (return-from string-replace (or result string))))


;;;-------------------------------------------------------------------------
;;; Connections

(defclass connection ()
  ((host :type string
         :initform "localhost"
         :initarg :host)
   (port :type integer
         :initform 61613
         :initarg :port)
   (stream :initform nil
           :initarg :stream)
   (stream-write-lock :initform (bt:make-lock))
   (encoding :initform :utf-8)           ;only utf-8 is currently supported
   (registrations :type list
                  :initform ()
                  :initarg :registrations)
   (key-mapping-strategy :initarg :key-mapping-strategy
                         :initform (make-instance 'pass-through-key-mapping-strategy))
   (error-callback :type (or null function)
                   :initform nil)
   (terminate :initform nil)))

(defun make-connection (host port
                        &key (key-mapping-strategy 'pass-through-key-mapping-strategy))
  (check-type host string)
  (check-type port integer)
  (make-instance 'connection
                 :host host
                 :port port
                 :key-mapping-strategy (make-instance key-mapping-strategy)))

;;;-------------------------------------------------------------------------
;;; Implementation of the API

(defmethod start ((conn connection) &key username passcode)
  "Connects to the message broker, sends subscriptions for any existing registrations,
   and enters a receive loop."
  (handler-bind
      ((t (lambda (e)
            (disconnect conn)
            (log-debug "Error: ~A" e))))
    (with-slots (host port stream stream-write-lock registrations terminate) conn
      (usocket:with-client-socket (socket strm host port
                                          :protocol :stream
                                          :element-type '(unsigned-byte 8))
        (setf stream strm)
        ;; Send CONNECT frame
        (connect conn username passcode)
        ;; Send SUBSCRIBE frames
        (loop for reg in registrations
              do (with-slots (destination selector id client-ack?) reg
                   (subscribe conn destination
                              :selector selector :id id :client-ack? client-ack?)))
        ;; The receive loop
        (let ((recvbuf '()))
          (loop until terminate
                do (let ((sock (car (usocket:wait-for-input socket :timeout 1 :ready-only t))))
                     (when sock
                       (let ((newbuf (append recvbuf (receive conn))))
                         (setf recvbuf (process-receive-buffer conn newbuf)))))))
        (disconnect conn)
        (log-debug "Terminated")
        (setf terminate nil)))))
        
(defmethod stop ((conn connection))
  "Gracefully terminates the current receive loop and closes the connection to the message broker."
  (with-slots (terminate) conn
    (setf terminate t)))

(defmethod connect ((conn connection) &optional username passcode)
  (check-type username (or null string))
  (check-type passcode (or null string))
  (with-slots (host) conn
    (sending-frame (conn frame "CONNECT"
                         "accept-version" "1.2"
                         "host" host
                         "heart-beat" "0,0"
                         "login" username
                         "passcode" passcode))))

(defmethod disconnect ((conn connection))
  (with-slots (stream) conn
    (when stream 
      (when (open-stream-p stream)
        (sending-frame (conn frame "DISCONNECT"))
        (close stream))
      (setf stream nil))))

(defmethod send ((conn connection) (frame frame))
  (send conn (with-output-to-string (stream)
               (write-string (render-frame frame conn) stream))))

(defmethod send ((conn connection) (string string))
  (with-slots (stream stream-write-lock encoding) conn
    (bt:with-lock-held (stream-write-lock)
      (log-debug "sending frame: ~A~%" string)
      (write-sequence (babel:string-to-octets string :encoding encoding) stream)
      (finish-output stream))))

(defmethod receive ((conn connection))
  "Called whenever there's activity on the connection stream.
   Reads from the stream and returns the received buffer as a list of bytes,
    or NIL if the connection has been closed by the broker."
  (with-slots (stream) conn
    (let ((buffer (loop while (listen stream)
                        as b = (read-byte stream nil 'eof)
                        unless (eql b 'eof)
                          collect b)))
      (if (> (length buffer) 0)
        ;; Return the buffer
        buffer
        ;; Otherwise, it means the other end has terminated,
        ;; so close things down
        (progn
          (log-debug "Nothing to read from socket, closing.")
          (stop conn)
          nil)))))

(defmethod process-receive-buffer ((conn connection) buffer)
  "Try to extract and process frame(s) from buffer.  Returns unprocessed buffer."
  (labels ((process-frame (frame)
             (log-debug "Frame: ~A" frame)
             (with-slots (name headers) frame
               (when (string= name "CONNECTED")
                 ;; Initiate heart-beat thread when required.
                 (let ((heart-beat (assoc :heart-beat headers :test #'header=)))
                   (when heart-beat
                     (multiple-value-bind (sx sy) 
                         (string-split (cadr heart-beat) ",")
                       (let ((period-ms (parse-integer sy)))
                         (when (> period-ms 0)
                           (with-slots (stream stream-write-lock terminate) conn
                             (bt:make-thread
                              (lambda ()
                                (loop until terminate
                                      do (progn
                                           (sleep (/ period-ms 1000))
                                           (log-debug "sending heartbeat")
                                           (bt:with-lock-held (stream-write-lock)
                                             (write-byte 10 stream)
                                             (finish-output stream))))))))))))))
             (apply-callbacks conn frame))
           (extract-frame ()
             ;; Identify frames by looking for NULLs
             ;; This is safe with UTF-8 because a 0 will never appear within multibyte characters
             ;;--- TODO: Use content-length header when provided instead of relying on NULL delimiter
             (let ((pos (position 0 buffer)))
               (when pos
                 (with-slots (encoding) conn 
                   (let* ((framevector (coerce buffer '(vector (unsigned-byte 8))))
                          (framestring (babel:octets-to-string framevector 
                                         :start 0 :end pos :encoding encoding)))
                     (process-frame (make-frame-from-string (string-left-strip framestring) conn))
                     (setf buffer (subseq buffer (1+ pos)))))))))
    (loop while (extract-frame))
    buffer))

(defun destination= (actual registered)
  "Returns T if the REGISTERED destination matches the ACTUAL destination."
  ;;--- TODO: Implement wildcard matching? (NOTE: not all message brokers support wildcard matching)
  (string-equal actual registered))

(defmethod apply-callbacks ((conn connection) (frame frame))
  "Send FRAME to any matching registered callbacks."
  (with-slots (registrations error-callback) conn
    (if (error-frame-p frame)
      (when error-callback
        (funcall error-callback frame))
      (let ((dest (get-destination frame))
            (subscription (get-subscription frame)))
        (loop for reg in registrations
              do (with-slots (callback destination id) reg
                   (when (and callback
                              ;; one or both could be nil
                              (string-equal subscription id)
                              ;; destination= will not return T for registrations using wildcards
                              ;; or temporary destinations, so allow a matching non-nil id to be
                              ;; sufficient for applying the callback
                              (or id (destination= dest destination)))
                     (funcall callback frame))))))))

(defmethod register ((conn connection) callback (destination string) &key selector id client-ack?)
  "Register a callback for a destination.  A subscription to the destination using the
   optional client-ack? is issued for all callbacks as part of connecting to the MQ server."
  (check-type callback (or null function))
  (with-slots (stream registrations) conn
    (when stream
      (subscribe conn destination :selector selector :id id :client-ack? client-ack?))
    (setf registrations (append registrations (list (make-instance 'registration 
                                                      :callback callback
                                                      :destination destination
                                                      :selector selector
                                                      :id id
                                                      :client-ack? client-ack?))))))

(defmethod unregister-id ((conn connection) id)
  (with-slots (stream registrations) conn
    (when stream
      (unsubscribe-id conn id))
    (setf registrations (remove-if #'(lambda (reg) 
                                       (string-equal id (slot-value reg 'id))) registrations))))

(defmethod register-error ((conn connection) callback)
  "Register an error callback for STOMP error frames."
  (check-type callback (or null function))
  (with-slots (error-callback) conn
    (setf error-callback callback)))

(defmethod subscribe ((conn connection) (destination string) &key selector id client-ack?)
  (sending-frame (conn frame "SUBSCRIBE"
                       "destination" destination)
    (when selector
      (set-selector frame selector))
    (when id
      (set-id frame id))
    (when client-ack?
      (set-client-ack frame))))

(defmethod unsubscribe ((conn connection) (destination string))
  (sending-frame (conn frame "UNSUBSCRIBE"
                       "destination" destination)))

(defmethod unsubscribe-id ((conn connection) (id string))
  (sending-frame (conn frame "UNSUBSCRIBE"
                       "id" id)))

(defmethod post ((conn connection) (message string) (destination string) &optional headers)
  (sending-frame (conn frame "SEND"
                       "destination" destination)
    (loop for (key value) in headers
          unless (header= key "destination")    ;don't overwrite the destination set above
            do (set-header frame key value))
    (setf (frame-body frame) message)))

(defmethod ack ((conn connection) (for-frame frame) &optional transaction)
  "Send the client ack for FRAME and optional TRANSACTION"
  (check-type transaction (or null string))
  (sending-frame (conn frame "ACK"
                       "message-id" (get-header for-frame "message-id")
                       "transaction" transaction)))

(defmethod begin ((conn connection) (transaction string))
  "Begin a transaction with name TRANSACTION"
  (sending-frame (conn frame "BEGIN"
                       "transaction" transaction)))

(defmethod commit ((conn connection) (transaction string))
  "Commit a transaction with name TRANSACTION"
  (sending-frame (conn frame "COMMIT"
                       "transaction" transaction)))

;; Naming this method 'abort' is not a good idea, so calling it 'rollback' instead
(defmethod rollback ((conn connection) (transaction string))
  "Abort a transaction with name TRANSACTION."
  (sending-frame (conn frame "ABORT"
                       "transaction" transaction)))
