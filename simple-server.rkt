#lang racket

;;; template whence to quickly hack together communication between a local machine
;;; and remote ones.
;;; to that simple end, this is a simple wrapper around (-> req resp) fns.

;; this template displays an input text box; entering text then submitting the form
;; writes the box's content to current-output-port.

;; remember to check your iptables! use a port accepts connections!

(require web-server/servlet-dispatch
         web-server/http/response-structs
         web-server/http/request-structs
         net/url-structs
         net/url-string
         (only-in xml xexpr->string))

#| quick ref
(struct request (method        ; bytes?
                 uri           ; url?
                 headers/raw   ; (listof header)
                 post-data/raw ; (or/c #f bytes?) ; populated for urlencoded forms
                 bindings/raw  ; populated for multipart form data
                 host-ip       ; string?
                 host-port     ; number?
                 client-ip))   ; string?
(struct header (field value)) ; both bytes?
(headers-assq* id heads) ; (-> bytes? (listof header?) (or/c #f header?))
(struct url (scheme user host port path-absolute? path query fragment))
response/output kwargs: #:code #:message bytes? #:mime-type bytes? #:headers
|#

(define (page req)
  (xexpr->string `(html ()
                    (head () (title () ""))
                    (body ()
                      (form ([action ""] [enctype "multipart/form-data"] [method "post"])
                            (input ([name "a"] [type "text"]))
                            (input ([style "display:none"] [type "submit"])))))))

(serve/launch/wait
 #:port 2000
 #:listen-ip #f ; accept connexions from any remote machine
 (λ (_sem) ; fsemaphore-post to stop server
  (dispatch/servlet
   (λ (req)
    (flush-output (current-output-port)) ;; somewhy needed for outputting to stdout before returning http response
    (case (request-method req)
          [(#"post" #"POST") (displayln (binding:form-value (bindings-assq #"a" (request-bindings/raw req))))
                             (response/output (λ (o) (display (page req) o)))]
          [else (response/output (λ (o) (display (page req) o)))])))))
