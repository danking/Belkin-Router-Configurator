#lang scheme
(require test-engine/scheme-gui)
(require (only-in "../main.ss" valid-syntax? build-fwi build-descriptions))

(check-expect (valid-syntax? '("Ventrilo" #t TCP 101 (80 80) (80 80))) true)
(check-expect (valid-syntax? '("HTTP Server" #t TCP 101 (80 80) (80 80))) true)
(check-expect (valid-syntax? '(not-a-string #t TCP 101 (80 80) (80 80))) false)
(check-expect (valid-syntax? '("Ventrilo" 1 TCP 101 (80 80) (80 80))) false)
(check-expect (valid-syntax? '("Ventrilo" #t 1 101 (80 80) (80 80))) false)
(check-expect (valid-syntax? '("Ventrilo" #t TCP 256 (80 80) (80 80))) false)
(check-expect (valid-syntax? '("Ventrilo" #t TCP 255 (0 80) (80 80))) false)

(check-expect (build-fwi '(("HTTP Server" #t TCP 101 (80 80) (80 80))
                           ("FTP Server"  #t TCP 101 (21 21) (21 21))))
              "1-x-80-80-1-101-80-80%201-x-21-21-1-101-21-21%20")
(check-expect (build-descriptions '(("HTTP Server" #t TCP 101 (80 80) (80 80))
                                    ("FTP Server"  #t TCP 101 (21 21) (21 21))))
              "HTTP__Server%20FTP__Server%20")
(test)