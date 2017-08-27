(defpackage reconnecting-websocket
  (:use :cl)
  (:nicknames :rws)
  (:export #:reconnecting-websocket
           #:url
           #:debug-p
           #:auto-open-p
           #:reconnect-interval
           #:max-reconnect-interval
           #:reconnect-decay
           #:max-reconnect-attempts
           #:reconnect-attempts
           #:client
           #:event-listeners
           #:ready-state
           #:start-connection
           #:close-connection
           #:on
           #:send
           #:send-text
           #:send-binary
           #:send-ping))

(in-package :reconnecting-websocket)

(defclass reconnecting-websocket ()
  ((url
    :initarg :url
    :reader url
    :initform (error "url is required"))
   (debug-p
    :initarg :debug-p
    :accessor debug-p
    :initform nil)
   (auto-open-p
    :initarg :auto-open-p
    :accessor auto-open-p
    :initform t)
   (reconnect-interval
    :initarg :reconnect-interval
    :accessor reconnect-interval
    :initform 1)
   (max-reconnect-interval
    :initarg :max-reconnect-interval
    :accessor max-reconnect-interval
    :initform 30)
   (reconnect-decay
    :initarg :reconnect-decay
    :accessor reconnect-decay
    :initform 1.5)
   (max-reconnect-attempts
    :initarg :max-reconnect-attempts
    :accessor max-reconnect-attempts
    :initform nil)
   (reconnect-attempts
    :accessor reconnect-attempts
    :initform 0)
   (client
    :accessor client
    :initform nil)
   (forced-close-p
    :accessor forced-close-p
    :initform nil)
   (event-listeners
    :initarg :event-listeners
    :accessor event-listeners
    :initform nil)
   (ready-state
    :accessor ready-state
    :initform :connecting)))

(defmethod initialize-instance :after ((client reconnecting-websocket) &key)
  (when (auto-open-p client)
    (start-connection client)))

(defmethod close-connection ((client reconnecting-websocket) &optional reason code)
  (unless code
    (setf code 1000))
  (setf (forced-close-p client) t)
  (when (client client)
    (if (eq :open (ready-state client))
        (wsd:close-connection (client client) reason code)
        (setf (ready-state client):closed))))

(defmethod start-connection ((client reconnecting-websocket) &optional reconnect-attempt-p)
  (when (client client)
    (ignore-errors
      (wsd:close-connection (client client))))
  (setf (client client)
        (wsd:make-client (url client)))
  (if reconnect-attempt-p
      (when (and (max-reconnect-attempts client)
                 (> (reconnect-attempts client)
                    (max-reconnect-attempts client)))
        (let ((reason (format nil
                              "ReconnectingWebSocket attempt-connect too many times: ~A~%"
                              (reconnect-attempts client))))
          (when (debug-p client)
            (format *debug-io* "~A~%" reason))
          (close-connection client reason)
          (dolist (listener (getf (event-listeners client) :error))
            (funcall listener reason)))
        (return-from start-connection))
      (progn
        ;; eventTarget.dispatchEvent(generateEvent('connecting'));
        (setf (reconnect-attempts client) 0)))
  (when (debug-p client)
    (format *debug-io* "ReconnectingWebSocket attempt-connect ~S ~A times~%"
            (url client)
            (reconnect-attempts client)))
  (wsd:on :open (client client) 
          #'(lambda ()
              (when (debug-p client)
                (format *debug-io* "ReconnectingWebSocket onopen ~S~%" (url client)))
              (setf (ready-state client) :open)
              (setf (reconnect-attempts client) 0)
              (setf reconnect-attempt-p nil)
              ;; dispatch :open event
              (dolist (listener (getf (event-listeners client) :open))
                (funcall listener))))
  (wsd:on :close (client client) 
          #'(lambda (&key code reason)
              (setf (client client) nil)
              (if (forced-close-p client)
                  (progn
                    (setf (ready-state client) :closed)
                    ;; dispatch :closed event
                    (dolist (listener (getf (event-listeners client) :close))
                      (funcall listener :code code :reason reason)))
                  (progn
                    (setf (ready-state client) :connecting)
                    (when (not reconnect-attempt-p)
                      (when (debug-p client)
                        (format *debug-io*
                                "ReconnectingWebSocket onclose ~S, code: ~S, reason: ~S~%"
                                (url client)
                                code
                                reason))
                      ;; dispatch :close event
                      (dolist (listener (getf (event-listeners client) :close))
                        (funcall listener :code code :reason reason)))
                    (let* ((interval (* (reconnect-interval client)
                                        (expt (reconnect-decay client)
                                              (reconnect-attempts client))))
                           (interval (if (> interval (max-reconnect-interval client))
                                         (max-reconnect-interval client)
                                         interval)))
                      (when (debug-p client)
                        (format *debug-io*
                                "ReconnectingWebSocket ~S will wait ~As~%"
                                (url client)
                                interval))
                      (sleep interval)
                      (incf (reconnect-attempts client))
                      (start-connection client t))))))
  (wsd:on :message (client client)
          #'(lambda (message)
              (when (debug-p client)
                (format *debug-io*
                        "ReconnectingWebSocket onmessage ~S, ~S~%"
                        (url client)
                        message))
              ;; dispatch :message event
              (dolist (listener (getf (event-listeners client) :message))
                (funcall listener message))))
  (wsd:on :error (client client)
          #'(lambda (error)
              (when (debug-p client)
                (format *debug-io* "ReconnectingWebSocket onerror ~S, ~S~%"
                        (url client)
                        error))
              ;; dispatch :error event
              (dolist (listener (getf (event-listeners client) :error))
                (funcall listener error))))
  (handler-case
      (wsd:start-connection (client client))
    (error (condition)
      (when (debug-p client)
        (format *debug-io* "ReconnectingWebSocket start-connection error ~S~%"
                condition))
      (event-emitter:emit :close (client client)))))

(defmethod on ((client reconnecting-websocket) event handler)
  (pushnew handler (getf (event-listeners client) event)))

(defmethod remove-listener ((client reconnecting-websocket) event handler)
  (wsd:remove-listener (client client) event handler)
  (setf (getf (event-listeners client) event)
        (remove handler (getf (event-listeners client) event))))

(defmethod remove-all-listeners ((client reconnecting-websocket) &optional event)
  (if event
      (progn
        (wsd:remove-all-listeners (client client) event)
        (setf (event-listeners client)
              (remf (event-listeners client) event)))
      (progn
        (wsd:remove-all-listeners (client client))
        (setf (event-listeners client) nil))))

(defmethod send ((client reconnecting-websocket) data &key start end type code callback)
  (if (and (client client)
           (equal :open (ready-state client)))
      (progn
        (when (debug-p client)
          (format *debug-io* "ReconnectingWebSocket send ~S ~S~%"
                  (url client)
                  data))
        (wsd:send (client client)
                  data
                  :start start
                  :end end
                  :type type
                  :code code
                  :callback callback))
      (error "INVALID_STATE_ERR : Pausing to reconnect websocket")))

(defmethod send-text ((client reconnecting-websocket) message &key start end callback)
  (if (and (client client)
           (equal :open (ready-state client)))
      (progn
        (when (debug-p client)
          (format *debug-io* "ReconnectingWebSocket send-text ~S ~S~%"
                  (url client)
                  message))
        (wsd:send-text (client client)
                       message
                       :start start
                       :end end
                       :callback callback))
      (error "INVALID_STATE_ERR : Pausing to reconnect websocket")))

(defmethod send-binary ((client reconnecting-websocket) usb8-vector &key start end callback)
  (if (and (client client)
           (equal :open (ready-state client)))
      (progn
        (when (debug-p client)
          (format *debug-io* "ReconnectingWebSocket send-binary ~S ~S~%"
                  (url client)
                  usb8-vector))
        (wsd:send-binary (client client)
                         :start start
                         :end end
                         :callback callback))
      (error "INVALID_STATE_ERR : Pausing to reconnect websocket")))

(defmethod send-ping ((client reconnecting-websocket) &optional message callback)
  (if (and (client client)
           (equal :open (ready-state client)))
      (progn
        (when (debug-p client)
          (format *debug-io* "ReconnectingWebSocket send-ping ~S ~S~%"
                  (url client)
                  message))
        (wsd:send-ping (client client)
                       :message message
                       :callback callback))
      (error "INVALID_STATE_ERR : Pausing to reconnect websocket")))
