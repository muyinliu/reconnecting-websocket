(in-package :cl-user)

(defpackage reconnecting-websocket-test
  (:use :cl :prove)
  (:nicknames :rwst))

(in-package :reconnecting-websocket-test)

(defvar *websocket-echo-server*
  #'(lambda (env)
      (let ((ws (wsd:make-server env)))
        (wsd:on :message ws
                #'(lambda (message)
                    (wsd:send ws message)))
        #'(lambda (responder)
            (declare (ignore responder))
            (wsd:start-connection ws)))))

(defvar *wookie-server* nil)

(defun start-websocket-server ()
  (unless *wookie-server*
    (setf *wookie-server*
          (clack:clackup *websocket-echo-server* :server :wookie :port 5000 :silent t))))

(defun stop-websocket-server ()
  (when *wookie-server*
    (clack:stop *wookie-server*)
    (setf *wookie-server* nil)))

(defmacro with-websocket-server (&body body)
  `(progn
     (start-websocket-server)
     (unwind-protect
          (progn ,@body)
       (stop-websocket-server))))

(defmacro with-websocket-client ((var &rest args) &body body)
  `(let ((,var (make-instance 'rws:reconnecting-websocket
                              ,@args)))
     (unwind-protect
          (progn ,@body)
       (rws:close-connection client "test close connection" 2000))))


(plan nil)

(subtest "Testing connection"
  (with-websocket-server
    (with-websocket-client (client :url "ws://localhost:5000/ws")
      (is (rws:ready-state client) :open))))

(subtest "Testing auto-reconnect"
  (with-websocket-server
    (with-websocket-client (client :url "ws://localhost:5000/ws")
      (is (rws:ready-state client) :open)
      (stop-websocket-server)
      (is (rws:ready-state client) :connecting)
      (start-websocket-server)
      (sleep 3)
      (is (rws:ready-state client) :open))))

(subtest "Testing max-reconnect-attempts"
  (with-websocket-server
    (let* ((reconnect-too-many-times-p nil)
           (event-listeners (list :error
                                  (list #'(lambda (client error)
                                            (declare (ignorable client))
                                            (when (equal error "ReconnectingWebSocket attempt-connect too many times: 2
")
                                             (setf reconnect-too-many-times-p t)))))))
      (with-websocket-client (client :url "ws://localhost:5000/ws"
                                     :max-reconnect-attempts 1
                                     :event-listeners event-listeners)
        (is (rws:ready-state client) :open)
        (stop-websocket-server)
        (is (rws:ready-state client) :connecting)
        (sleep 4)
        (is (rws:ready-state client) :closed)
        (is (rws:reconnect-attempts client) 2)
        (is reconnect-too-many-times-p t)))))

(subtest "Testing event handler"
  (with-websocket-server
    (let* ((random-message (write-to-string (random 100000000))))
      (is (with-output-to-string (stream)
            (let ((event-listeners
                   (list :open
                         (list #'(lambda (client)
                                   (declare (ignorable client))
                                   (format stream "onopen~%")))
                         :message
                         (list #'(lambda (client message)
                                   (declare (ignorable client))
                                   (format stream "onmessage: ~A~%"
                                           (babel:octets-to-string message :encoding :utf-8))))
                         :close
                         (list #'(lambda (client &key code reason)
                                   (declare (ignorable client))
                                   (format stream "onclose: ~A ~A~%"
                                           code reason))))))
              (with-websocket-client (client :url "ws://localhost:5000/ws"
                                             :event-listeners event-listeners)
                (is (rws:ready-state client) :open)
                (rws:send client (babel:string-to-octets random-message :encoding :utf-8))
                (sleep .1)
                (rws:close-connection client "reason to close" 5000))))
          (format nil "onopen~%onmessage: ~A~%onclose: 5000 reason to close~%"
                  random-message)))))

(subtest "Testing event handler after reconnect"
  (with-websocket-server
    (let* ((random-message (write-to-string (random 100000000)))
           (random-message2 (write-to-string (random 100000000))))
      (is (with-output-to-string (stream)
            (let ((event-listeners
                   (list :open
                         (list #'(lambda (client)
                                   (declare (ignorable client))
                                   (format stream "onopen~%")))
                         :message
                         (list #'(lambda (client message)
                                   (declare (ignorable client))
                                   (format stream "onmessage: ~A~%"
                                           (babel:octets-to-string message :encoding :utf-8))))
                         :close
                         (list #'(lambda (client &key code reason)
                                   (declare (ignorable client))
                                   (format stream "onclose: ~A ~A~%"
                                           code reason))))))
              (with-websocket-client (client :url "ws://localhost:5000/ws"
                                             :event-listeners event-listeners)
                (is (rws:ready-state client) :open)
                (rws:send client (babel:string-to-octets random-message :encoding :utf-8))
                (sleep .1)
                (stop-websocket-server)
                (is (rws:ready-state client) :connecting)
                (start-websocket-server)
                (sleep 3)
                (is (rws:ready-state client) :open)
                (rws:send client (babel:string-to-octets random-message2 :encoding :utf-8))
                (sleep .1)
                (rws:close-connection client "reason to close" 5000))))
          (format nil "onopen~%onmessage: ~A~%onclose: NIL NIL~%onopen~%onmessage: ~A~%onclose: 5000 reason to close~%"
                  random-message
                  random-message2)))))
