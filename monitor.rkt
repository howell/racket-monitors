#lang racket

(require (for-syntax syntax/parse))

(module+ test
  (require rackunit))

(define-syntax (define/synchronized stx)
  (syntax-parse stx
    [(_ (method-name:id formals:id ...) body:expr ...)
     #'(define/public (method-name formals ...)
         (send this enter-synchronized-method)
         (define result
           (begin body ...))
         (send this exit-synchronized-method)
         result)]))

(define-syntax (define/synchronized/override stx)
  (syntax-parse stx
    [(_ (method-name:id formals:id ...) body:expr ...)
     #'(define/override (method-name formals ...)
         (send this enter-synchronized-method)
         (define result
           (begin body ...))
         (send this exit-synchronized-method)
         result)]))

(define monitor-mixin
  (mixin () ()
    (super-new)
    ;; lock guarding mutable bits of monitor state, initially available
    (define the-lock (make-semaphore 1))
    ;; (U #f Thread)
    (define the-owner #f)
    ;; How many synchronized methods does the owner need to return from before
    ;; relinquishing the monitor
    (define stack-depth 0)
    ;; (Listof (Cons Semaphore Thread))
    (define waiters '())

    (define/public (enter-synchronized-method)
      (define current (current-thread))
      (cond
        [(equal? the-owner current)
         ;; we are already in the monitor, no need to lock
         (set! stack-depth (add1 stack-depth))]
        [else
         ;; someone else may be in the montior, obtain the lock
         (semaphore-wait the-lock)
         (set! the-owner current)
         (set! stack-depth 1)]))

    ;; need to keep track of depth, I believe
    (define/public (exit-synchronized-method)
      (define current (current-thread))
      (unless (equal? the-owner current)
        (error 'exit-synchronized-method
               "exit-synchronized-method called by non-owner"))
      (set! stack-depth (sub1 stack-depth))
      (when (zero? stack-depth)
        (exit-monitor)))

    (define (exit-monitor)
      (set! the-owner #f)
      (semaphore-post the-lock))

    ;; wake up the first
    (define (notify)
      (unless (equal? the-owner (current-thread))
        (error 'notify "notify called by non-owner"))
      (unless (empty? waiters)
        (match-define (cons sema waiter) (first waiters))
        (set! waiters (rest waiters))
        (semaphore-post sema)))

    ;; wake up all
    (define (notify-all)
      (unless (equal? the-owner (current-thread))
        (error 'notify "notify called by non-owner"))
      (define ws waiters)
      (set! waiters empty)
      (for ([pr (in-list ws)])
        (semaphore-post (car pr))))

    (define (wait)
      (define current (current-thread))
      (unless (equal? the-owner current)
        (error 'wait "wait called by non-owner"))
      (define sema (make-semaphore))
      (set! waiters (append waiters (list (cons sema current))))
      (semaphore-post the-lock)
      (semaphore-wait sema))))

(module+ test

  (define counter%
    (class object%
      (super-new)
      (field [count 0])
      (define/public (incr!)
        (define val count)
        (sleep 0)
        (set! count (add1 val)))))

  (define synchronized-counter%
    (class (monitor-mixin counter%)
      (super-new)
      (define/synchronized/override (incr!)
        (super incr!))))

  (define (test cntr threads incrs)
    (define handles
      (for/list ([i (in-range threads)])
        (thread
         (thunk
          (for ([_ (in-range incrs)])
            (send cntr incr!))))))
    (for ([thd (in-list handles)])
      (thread-wait thd)))
  (define c1 (new counter%))
  (define THREADS 50)
  (define INCRS 1000)
  (test c1 THREADS INCRS)
  (check-not-equal? (* THREADS INCRS)
                    (get-field count c1))
  (define c2 (new synchronized-counter%))
  (test c2 THREADS INCRS)
  (check-equal? (* THREADS INCRS)
                (get-field count c2)))