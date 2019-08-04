;; proxy-mode.lisp --- Proxy mode (e.g. Tor et al.)

(in-package :next)

(define-mode proxy-mode ()
    "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."
    ;: TODO: for the PyQt side, we now want the protocol, the IP and the
    ;; port on different slots.
    ((server-address :accessor server-address :initarg :server-address
                     :initform "socks5://127.0.0.1:9050"
                     :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example:
  http://192.168.1.254:8080")
     (whitelist :accessor whitelist :initarg :whitelist
                :initform '("localhost" "localhost:8080")
                :documentation "A list of URI not to forward to the proxy.
It must be a list of strings.")
     (proxied-downloads-p :accessor proxied-downloads-p :initarg :proxied-downloads-p
                          :initform t
                          :documentation "Non-nil if downloads should also use
the proxy.")
     (destructor
      :initform
      (lambda (mode)
        (rpc-set-proxy *interface* (buffer mode) "" nil))))
  (rpc-set-proxy *interface* (buffer %mode) (server-address %mode) (whitelist %mode))
  (echo "Proxy set to ~a (whitelisting ~a)." (server-address %mode) (whitelist %mode)))
