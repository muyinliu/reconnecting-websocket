(defsystem "reconnecting-websocket"
  :name "reconnecting-websocket"
  :description "Auto Reconnect WebSocket Client for Common Lisp."
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("websocket-driver-client")
  :in-order-to ((test-op (test-op "reconnecting-websocket-test")))
  :serial t
  :components ((:file "reconnecting-websocket")))
