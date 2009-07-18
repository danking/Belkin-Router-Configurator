#lang scheme
(require net/url)
(provide (all-defined-out))

(define path-to-settings-file "settings")

;; Virtual-Servers is a [ListOf (list NAME ENABLED PROTOCOL OCTET
;;                                    (list PORT PORT) (list PORT PORT))]
;; Where..
;;  - NAME     is a human-readable String name of the server
;;  - ENABLED  is either true or false
;;  - PROTOCOL is either 'TCP or 'UDP
;;  - OCTET    is a number between 1 and 255, reprsenting the server's last octet
;;  - PORT     is a number between 1 and 65535
;; The first pair of ports is the inbound (from the internet) ports and the
;; second pair of ports is the serverbound (from the router) ports.

;; [ElementOf [Virtual-Servers]] -> Boolean
(define (valid-syntax? x)
  (let ([port? (lambda (x) (and (number? x) (<= 1 x 65535)))])
    (and (string? (first x))
         (boolean? (second x))
         (and (symbol? (third x)) (or (symbol=? 'TCP (third x))
                                      (symbol=? 'UDP (third x))))
         (and (number? (fourth x)) (<= 1 (fourth x) 255))
         (and (cons? (fifth x)) (port? (first (fifth x)))
              (port? (second (fifth x))))
         (and (cons? (sixth x)) (port? (first (sixth x)))
              (port? (second (sixth x)))))))

(define (build-fwi vs)
  (foldr
   (lambda (x xs)
     (if (valid-syntax? x)
         (let ([enabled  (if (second x) "1" "0")]
               [iport1   (number->string (first  (fifth x)))]
               [iport2   (number->string (second (fifth x)))]
               [protocol (cond [(symbol=? 'TCP (third x)) "1"]
                               [(symbol=? 'UDP (third x)) "2"]
                               [else (error 'build-fwi "Unknown protocol: ~a"
                                            (third x))])]
               [octet    (number->string (fourth x))]
               [oport1   (number->string (first  (sixth x)))]
               [oport2   (number->string (second (sixth x)))])
           (string-append enabled "-x-" iport1 "-" iport2 "-" protocol "-"
                          octet "-" oport1 "-" oport2 "%20"
                          xs))
         (error 'build-fwi "Bad syntax for entry: ~a" x)))
   "" vs))

(define (build-descriptions vs)
  (foldr (lambda (x xs) (let ([ls (string->list (first x))])
                          (string-append (list->string
                                          (foldr (lambda (c cs)
                                                   (if (char=? #\  c)
                                                       (list* #\_ #\_ cs)
                                                       (cons c cs)))
                                                 empty ls))
                                         "%20" xs)))
         "" vs))

;; some obscure format for sepcifying ports
;; ENABLED-x-IPORT1-IPORT2-PROTOCOL-LASTOCTET-OPORT1-OPORT2<SPACE>
;; Where..
;;  - ENABLED is either 1 (enabled) or 2 (disabled)
;;  - x is just x, I think it means there is a description to follow
;;  - IPORT1 and IPORT2 specify the inbound (from-internet) port range
;;    as [IPORT1, IPORT2]
;;  - PROTOCOL is either 1 (TCP) or 2 (UDP)
;;  - OPORT1 and OPORT2 specify the "private" (to-server) port range
;;    as [OPORT1, OPORT2]
;;  - <SPACE> is a space, written as %20
(define fwi (string-append "1-x-80-80-1-101-80-80%201-x-21-21-1-101-21-21"
                           "%201-x-27015-27015-2-11-27015-27015%201-x-3784-3784"
                           "-1-11-3784-3784%201-x-6112-6112-1-6-6112-6112%201-x"
                           "-22-22-1-101-22-22%201-x-3389-3389-1-102-3389-"
                           "3389%201-x-1000-1010-1-101-1000-1010%201-x-"
                           "54236-54236-1-11-54236-54236%20"))
;; Another weird format for specifying descriptions which is basically a
;; string of space delimited names.  A space in a name is represented as
;; __ (two underscores).  The space delimeter is written as %20.
(define descriptions (string-append "HTTP__Server%20FTP__Server%20Half__Life"
                                    "__Server%20Ventrilo%20Sins%20freeNX%20"
                                    "remotedesktop%20freeNX%20__%20"))


;; The settings datum
(define settings (read (open-input-file path-to-settings-file)))
;; the Yj... stuff is a base64 encoded version of my password
(define login-post (string->bytes/utf-8
                    "action=submit&logout=&page=&pws=YjFuNTNqcTQ%3D"))
;; The virtual servers url
(define virt-srvs/url (string->url "http://192.168.2.1/fw_virt.cgi"))
;; Build the post request
(define virt-srvs/post-req (string->bytes/utf-8
                            (string-append "action=Apply"
                                           "&fwi=" (build-fwi settings)
                                           "&fwi_des=" (build-descriptions settings))))

;; ============================================================================
;; Send the requests

;; this actually sends the request to login.cgi to get authorized
(define router/post (post-pure-port (string->url "http://192.168.2.1/login.cgi")
                                    login-post))
;; Send the post request
(define virt-srvs/input-port (post-pure-port virt-srvs/url
                                             virt-srvs/post-req))


;; Print out the response
(define (get-response)
  (letrec ([loop (lambda ()
                 (let ([v (read-line virt-srvs/input-port)])
                   (if (eof-object? v)
                       (void)
                       (begin (display (string-append v "\n"))
                              (loop)))))])
    (loop)))

