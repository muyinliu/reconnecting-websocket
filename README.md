# reconnecting-websocket - An auto-reconnect WebSocket client for Common Lisp

Note: reconnecting-websocket is a Common Lisp fork of [reconnecting-websocket](https://github.com/joewalnes/reconnecting-websocket) in JavaScript, base on [websocket-driver](https://github.com/fukamachi/websocket-driver).

## Install
In Shell:
```shell
git clone https://github.com/muyinliu/reconnecting-websocket.git
cp -r reconnecting-websocket ~/quicklisp/local-projects/reconnecting-websocket
```

In Common Lisp REPL(require QuickLisp):
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
* [websocket-driver-client](https://github.com/fukamachi/websocket-driver)
* [babel](https://travis-ci.org/cl-babel/babel)

## Dependencies for test
* [websocket-driver](https://github.com/fukamachi/websocket-driver)
* [prove](https://github.com/fukamachi/prove)

## Usage

```lisp
(defvar *rws-event-listeners*
  (list :open (list #'(lambda ()
                        (format t "onopen~%")))
        :message (list #'(lambda (message)
                           (format t "onmessage: ~S~%"
                                   (babel:octets-to-string message :encoding :utf-8))))
        :close (list #'(lambda (&key code reason)
                         (format t "onclose: code: ~S, reason: ~S~%"
                                 code reason)))
        :error (list #'(lambda (error)
                         (format t "onerror: ~S~%" error)))))

(defvar *rws-client* (make-instance 'rws:reconnecting-websocket
                                    :url "ws://localhost/"
                                    :event-listeners *rws-event-listeners*))
                                    
(rws:send *rws-client* (babel:string-to-octets "message send to server" :encoding :utf-8))

;; bind more listener
(rws:on *rws-client*
        :open #'(lambda ()
                  (format t "onopen event~%")))                                   
```


## APIs
### [Class] `rws:reconnecting-websocket`
The base class for auto-reconnect WebSocket client, a wrapper of [websocket-driver-client](https://github.com/fukamachi/websocket-driver)

### [Event] `:open` `:message` `:error` `:close`
The same event as websocket-driver-client's

#### [Method] `(rws:on rws-client event handler)`
```lisp
(rws:on rws-client event handler)
```

for example:
```lisp
(rws:on rws-client :open #'(lambda () (format t "onopen~%")))
```

#### [Method] `(rws:remove-listener rws-client event handler)`

#### [Method] `(rws:remove-all-listeners rws-client &optional event)`


### [Method] `(rws:start-connection rws-client)`
Manually start a WebSocket connection. 
Note: by default reconnecting-websocket auto start-connection without option `:auto-open-p nil`

### [Method] `(rws:send rws-client &key start end type code callback)`

### [Method] `(rws:send-text rws-client message &key start end callback)`

### [Method] `(rws:send-binary rws-client usb8-vector &key start end callback)`

### [Method] `(rws:send-ping rws-client &optional message callback)`

### [Method] `(rws:close-connection rws-client)`
Manually close a WebSocket connection and will NOT auto-reconnect again.

### [Method] `(rws:url rws-client)`
Readonly

### [Method] `(rws:debug-p rws-client)`
Trun on/off debug message output to `*debug-io*`

### [Method] `(rws:auto-open-p rws-client)`
Default: `t`

### [Method] `(rws:reconnect-interval rws-client)`
Default: 1(s)

### [Method] `(rws:max-reconnect-interval rws-client)`
Default: 30(s)

### [Method] `(rws:reconnect-decay rws-client)`
Default: 1.5

### [Method] `(rws:max-reconnect-attempts rws-client)`
Default: nil, reconnect forever

### [Method] `(rws:client rws-client)`
Return websocket-driver-client

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
