(defsystem "reconnecting-websocket-test"
  :name "reconnecting-websocket-test"
  :description "test case for reconnecting-websocket"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :depends-on ("reconnecting-websocket"
               "websocket-driver-server"
               "clack"
               "babel"
               "prove")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module "test"
                        :serial t
                        :components ((:file "reconnecting-websocket-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
