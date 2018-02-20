# reconnecting-websocket - An auto-reconnect WebSocket client for Common Lisp

Note: reconnecting-websocket is a Common Lisp fork of [reconnecting-websocket](https://github.com/joewalnes/reconnecting-websocket) in JavaScript, base on [websocket-driver](https://github.com/fukamachi/websocket-driver).

## Install

In Shell:

```shell
git clone https://github.com/muyinliu/reconnecting-websocket.git
cp -r reconnecting-websocket ~/quicklisp/local-projects/reconnecting-websocket
```

Then in Common Lisp REPL(require QuickLisp):

```lisp
(ql:quickload 'reconnecting-websocket)
```
=>
```=>
To load "reconnecting-websocket":
  Load 1 ASDF system:
    reconnecting-websocket
; Loading "reconnecting-websocket"
.....
(RECONNECTING-WEBSOCKET)
```

Note: the nickname of package `reconnecting-websocket` is `rws`

## Dependencies

* [websocket-driver-client](https://github.com/fukamachi/websocket-driver) A WebSocket Client
* [babel](https://travis-ci.org/cl-babel/babel) Encode/Decode String/Bytes

## Dependencies for test

* [websocket-driver](https://github.com/fukamachi/websocket-driver) A WebSocket Server
* [prove](https://github.com/fukamachi/prove) A test system

## Usage

```lisp
(defvar *rws-event-listeners*
  (list :open (list #'(lambda (client)
                        (declare (ignorable client))
                        (format t "onopen~%")))
        :message (list #'(lambda (client message)
                           (declare (ignorable client))
                           (format t "onmessage: ~S~%"
                                   (babel:octets-to-string message :encoding :utf-8))))
        :close (list #'(lambda (client &key code reason)
                         (declare (ignorable client))
                         (format t "onclose: code: ~S, reason: ~S~%"
                                 code reason)))
        :error (list #'(lambda (client error)
                         (declare (ignorable client))
                         (format t "onerror: ~S~%" error)))))

(defvar *rws-client* (make-instance 'rws:reconnecting-websocket
                                    :url "ws://localhost/"
                                    :event-listeners *rws-event-listeners*))
                                    
(rws:send *rws-client* (babel:string-to-octets "message send to server" :encoding :utf-8))

;; bind more listener
(rws:on *rws-client*
        :open #'(lambda (client)
                  (declare (ignorable client))
                  (format t "onopen event~%")))
```

Note: `rws` stands for **Reconnecting WebSocket**


## APIs

### [Class] `rws:reconnecting-websocket`

The base class for auto-reconnect WebSocket client, a wrapper of [websocket-driver-client](https://github.com/fukamachi/websocket-driver)

### [Function] `(rws:make-reconnecting-websocket url &key debug-p auto-open-p reconnect-interval max-reconnect-interval reconnect-decay max-reconnect-attempts event-listeners)`

Create an auto-reconnect WebSocket client.

### [Event] `:open` `:message` `:error` `:close`

The same event as websocket-driver-client's

#### [Method] `(rws:on rws-client event handler)`

Bind event handler to auto-reconnect WebSocket client

```lisp
(rws:on rws-client event handler)
```

for example:
```lisp
(rws:on rws-client
        :open
        #'(lambda (client)
            (format t "onopen: ~S~%" client)))
(rws:on rws-client
        :message
        #'(lambda (client message)
            (format t "onmessage: ~S, ~S~%"
                    client message)))
(rws:on rws-client
        :error
        #'(lambda (client error) 
            (format t "onerror:~S, ~S~%"
                    client error)))
(rws:on rws-client
        :close
        #'(lambda (client &key code reason)
            (format t "onclose: ~S, code: ~S, reason: ~S~%"
                    client code reason)))
```

#### [Method] `(rws:remove-listener rws-client event handler)`

Remove event listener from auto-reconnect WebSocket client

#### [Method] `(rws:remove-all-listeners rws-client &optional event)`

Remove all event listeners from auto-reconnect WebSocket client

### [Method] `(rws:start-connection rws-client)`

Manually start a WebSocket connection. 

Note: by default reconnecting-websocket auto start-connection without option `:auto-open-p nil`

### [Method] `(rws:send rws-client &key start end type code callback)`

Send message to WebSocket Server

### [Method] `(rws:send-text rws-client message &key start end callback)`

Send text(String) message to WebSocket Server

### [Method] `(rws:send-binary rws-client usb8-vector &key start end callback)`

Send binary(bytes) message to WebSocket Server

### [Method] `(rws:send-ping rws-client &optional message callback)`

Send ping message to WebSocket Server

### [Method] `(rws:close-connection rws-client)`

Manually close a WebSocket connection and will NOT auto-reconnect again.

### [Method] `(rws:url rws-client)`

Get URL of auto-reconnect WebSocket client

Note: Readonly

### [Method] `(rws:debug-p rws-client)`

Trun on/off debug message output to `*debug-io*`

Trun on debug message output:

```lisp
(setf (rws:debug-p rws-client) t)
```

### [Method] `(rws:auto-open-p rws-client)`

Whether to auto-open connection while auto-reconnect WebSocket client is creating

Note: Default `t`

### [Method] `(rws:reconnect-interval rws-client)`

Delay before reconnecting WebSocket client

Note: Default `1`(second)

### [Method] `(rws:max-reconnect-interval rws-client)`

Max delay before reconnecting WebSocket client

Default: 30(s)

### [Method] `(rws:reconnect-decay rws-client)`

Decay rate between one reconnection and next reconnection, for example: current delay before reconnecting is 1 second, then next delay before reconnecting is 1.5 second, the next next delay before reconnecting is 2.25 second...

Default: 1.5

### [Method] `(rws:max-reconnect-attempts rws-client)`

Default: nil, reconnect forever

### [Method] `(rws:client rws-client)`

Return origin websocket-driver-client

### [Method] `(rws:event-listeners rws-client)`

Return a property list like this:

```
'(:open (#'(lambda () xxx))
  :message (#'(lambda () yyy))
  :close (#'(lambda () zzz))
  :error (#'(lambda () aaa)))
```

### [Method] `(rws:ready-state rws-client)`

Return WebSocket client's current ready-state, one of `:connecting` `:open` `:closed`


## Debug

```lisp
(setf (rws:debug-p *rws-client*) t)
```

Note: All debug message will output to `*debug-io*`


## Run test case

In Common Lisp REPL:

```lisp
(asdf:test-system :reconnecting-websocket)
```

Or in Shell:

```shell
sbcl --eval "(asdf:test-system :reconnecting-websocket)" --eval "(quit)"
```


## License

MIT (See LICENSE file for details).
