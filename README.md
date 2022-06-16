socks
------

socks proxy protocol implements by common lisp, server support socks 4/4a/5, client only support socks5

usage
-----

## server

start socks server

	(asdf:load-system :socks)
	(socks:listen-for-connection "127.0.0.1" 3458)

test with curl

     curl -v --socks5-hostname 127.0.0.1:3458  https://httpbin.org/ip
     curl -v --socks5 127.0.0.1:3458  https://httpbin.org/ip
     curl -v --socks4a 127.0.0.1:3458  https://httpbin.org/ip
     curl -v --socks4 127.0.0.1:3458  https://httpbin.org/ip

## client

      (asdf:load-system :socks)
      (let ((conn (socks:connect "127.0.0.1" 3458 "192.168.0.1" 80)))
        ;; now conn is a tcp connection to 192.168.0.1:80
        ;; do something with conn
        ;; ex, send http raw request
      (usocket:socket-close conn))
