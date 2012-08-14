(require-extension irc posix regex) ; http-client html-parser)

;; The configuration file should contain a definiton of the format:
;; (define *config* '(CHANNEL SERVER BOT-NICK OWNER-NICK))
(load "config.scm")

(define +channel+ (list-ref *config* 0))
(define +server+ (list-ref *config* 1))
(define +nick+ (list-ref *config* 2))
(define +vote-string+ ":!v")
(define +enact-string+ ":!e")
(define +propose-string+ ":!p")
(define *connection* (irc:connection server: +server+ nick: +nick+))
(define *user-table* (list (list-ref *config* 3)))

;;;;;;;;;;;;;;;;;;;;  http://schemecookbook.org/Cookbook/StringSplit
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
            ((char=? ch (string-ref str b)) (if (= a b)
                                              (split (+ 1 a) (+ 1 b))
                                              (cons (substring str a b) (split b b))))
            (else (split a (+ 1 b)))))))
      (split 0 0))))

;;;;;;;;;;;;;;;;;;;

(define (add-user! nick) (set! *user-table* (cons nick *user-table*)))
(define (remove-user! nick)
  (set! *user-table*
	(filter (lambda (x) (not (equal? x nick))) *user-table*)))

(define *proposal-table* '())
(define *last-proposal-number* -1)
(define (add-proposal! prop) (begin
  (set! *last-proposal-number* (+ 1 *last-proposal-number*))
  (set! *proposal-table* (cons (list *last-proposal-number* prop '())
			       *proposal-table*))
  *last-proposal-number*))

(define (remove-proposal! prop-number)
  (set! *proposal-table*
	(filter (lambda (x) (not (equal? prop-number (car x)))) *proposal-table*)))

;; this is find-tail
(define (lookup-proposal-tail* prop-number table)
  (if (null? table) #f
      (if (= (caar table) prop-number) table
	  (lookup-proposal* prop-number (cdr table)))))
(define (lookup-proposal-tail prop-number)
  (lookup-proposal-tail* prop-number *proposal-table*))

(define (update-alist alist key value)
  (if (null? alist) (list (cons key value))
      (if (equal? (caar alist) key)
	  (cons (cons key value) (cdr alist))
	  (cons (car alist) (update-alist (cdr alist) key value)))))

(define (add-vote-to-vtable vtable nick vote)
  (update-alist vtable nick vote))

(define (record-user-vote! nick vote prop-number)
  (let ((this-prop (lookup-proposal-tail prop-number)))
    (if this-prop
	(let ((vote-table (cddar this-prop)))
	  (set! (car vote-table)
	      (add-vote-to-vtable (car vote-table) nick vote))))))

(define (quorum) (/ (+ (length *user-table*) 1) 2))

(define (vote-count prop)
  (let* ((vtable (caddr prop))
	 (neg-votes (apply + (map (lambda (v) (if (cdr v) 0 1)) vtable)))
	 (pos-votes (apply + (map (lambda (v) (if (cdr v) 0 1)) vtable))))
    (cons pos-votes neg-votes)))

(define (enactable? prop)
  (if prop
      (let* ((vtable (caddr prop))
	     (pv (apply + (map (lambda (v) (if (cdr v) 1 0)) vtable))))
	(>= pv (quorum)))
      #f))
;;;;;;;;;;;;;;;;
(define (pong m)
  (irc:command *connection*
	       (string-append "PONG :" (first (irc:message-parameters m)))))

(define (kick user reason)
  (irc:command *connection*
	       (string-append "KICK " +channel+ " " user " :" reason)))

(define (kick-sender-for-reason reason)
  (lambda (m) (kick (irc:message-sender m) reason)))

(define (kick-sender-on-regex-for-reason! regex reason)
  (irc:add-message-handler! *connection* (kick-sender-for-reason reason)
			    body: (string-append ":.*" regex)
			    receiver: +channel+))

;;;;;;;;;;;;;;;
(define (eval-string s) (eval (with-input-from-string s read)))
(define (print-string s) (with-output-to-string (lambda () (display s))))

(define (eval-string-ignoring-errors s)
  (condition-case (eval-string s) (var () 'error)))

(define (eval-msg m) (begin (write (irc:message-parameters m)) (newline)
   (condition-case
    (let ((result (eval (with-input-from-string
			    (substring (cadr (irc:message-parameters m))
				       (string-length +eval-string+)) read))))
      (if (not (eq? result (void)))
	  (irc:say *connection* (print-string result))))
    (var () 'error))))

(define (propose-msg m) (begin
   (add-proposal! (substring (cadr (irc:message-parameters m))
			     (string-length +propose-string+)))
   (irc:say *connection*
	    (string-append "proposal added as #"
			   (number->string *last-proposal-number*) "."))))
(define (vote-msg m) (begin
   (let* ((msg-body (substring (cadr (irc:message-parameters m))
			       (string-length +vote-string+)))
	  (msg-words (str-split msg-body #\space))
	  (n-words (length msg-words))
	  (valid (>= n-words 2))
	  (user (car (irc:message-prefix m))))
     (if valid (let ((nprop (string->number (cadr msg-words)))
		     (vote (string-ref (car msg-words) 0)))
		 (if nprop
		     (cond
		      ((equal? vote #\n) (record-user-vote! user #f nprop))
		      ((equal? vote #\y) (record-user-vote! user #t nprop)))))))))

(define (enact-msg m) (begin
   (let* ((msg-body (substring (cadr (irc:message-parameters m))
			       (string-length +enact-string+)))
	  (nprop (string->number msg-body)))
     (if nprop
	 (let ((prop-tail (lookup-proposal-tail nprop)))
	   (if prop-tail
	       (if (enactable? (car prop-tail))
		   (begin
		     (let ((result (eval-string-ignoring-errors (cadar prop-tail))))
		       (remove-proposal! nprop)
		       (if (equal? result 'error)
			   (irc:say *connection*
				    (string-append "proposal #" msg-body
						   " was enacted, but encountered an error."))
			   (irc:say *connection* (string-append "enacted proposal #"
								msg-body ".")))))
		   (irc:say *connection* (string-append "proposal #" msg-body
							" has not reached a quorum.")))
	       (irc:say *connection* (string-append "what is proposal #" msg-body "?"))))))))

;;;;;;;;;;;;;;;

(define (start-bot) (begin
   (irc:add-message-handler! *connection* pong
			     command: "PING")
   (irc:add-message-handler! *connection* enact-msg
			     command: "PRIVMSG"
			     body: +enact-string+ receiver: +channel+)
   (irc:add-message-handler! *connection* propose-msg
			     command: "PRIVMSG"
			     body: +propose-string+ receiver: +channel+)
   (irc:add-message-handler! *connection* vote-msg
			     command: "PRIVMSG"
			     body: +vote-string+ receiver: +channel+)
   
   (irc:connect *connection*)
   (sleep 10)
   (irc:join *connection* +channel+)
   (irc:run-message-loop *connection*)))
