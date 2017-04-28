#lang racket

(require "monitor.rkt")
(require racket/tcp)

(provide HOST PORT)

;; a User is a String, representing the username

(define chatroom%
  (class (monitor-mixin object%)
    (super-new)
    ;; (Hashof User (String -> Void))
    ;; given that hashes use a semaphore internally, it might make sense to use
    ;; a more 'unsafe' datastructure to motivate the monitor (but, I don't think
    ;; the internal semaphore guards against all/most concurrency issues)
    (define members (make-hash))

    ;; User (String -> Void) -> Void
    (define/synchronized (connect user callback)
      (for ([existing-user (in-hash-keys members)])
        (callback (format "~a arrived" existing-user)))
      (hash-set! members user callback)
      (announce (format "~a arrived" user)))

    ;; User String -> Void
    (define/synchronized (speak user text)
      (announce (format "~a: ~a" user text)))

    ;; User -> Void
    (define/synchronized (disconnect user)
      (when (hash-has-key? members user)
        (hash-remove! members user)
        (announce (format "~a left" user))))

    ;; String -> Void
    (define (announce what)
      (for ([(user callback) (in-hash (hash-copy members))])
        (with-handlers ([(const #t) (lambda (e) (disconnect user))])
          (callback what))))))

(define HOST "localhost")
(define PORT 5998)

(define (chat-server)
  (define chatroom (new chatroom%))
  ;; can't explain 128 and #t, copied from syndicate driver
  (define listener (tcp-listen PORT 128 #t HOST))
  (let loop ()
    (printf "Listening for new connections...\n")
    (define-values (in-p out-p) (tcp-accept listener))
    (file-stream-buffer-mode out-p 'line)
    (printf "Accepted new connection\n")
    (thread (thunk (user-agent chatroom in-p out-p)))
    (loop)))

(define (user-agent chatroom in-p out-p)
  (define username (symbol->string (gensym 'user)))
  (define (shutdown!)
    (close-input-port in-p)
    (close-output-port out-p))
  (define (from-server what)
    (with-handlers ([(const #t) (lambda (e) (shutdown!))])
      (displayln what out-p)))
  (send chatroom connect username from-server)
  (let loop ()
    (define in (read-line in-p))
    (cond
      [(eof-object? in)
       ;; need to shutdown/cleanup
       (send chatroom disconnect username)
       (shutdown!)]
      [else
       (send chatroom speak username in)
       (loop)])))

(module+ main
  (chat-server))