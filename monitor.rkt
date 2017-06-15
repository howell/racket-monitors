#lang racket

(provide monitor-mixin
         monitor%
         define/synchronized
         define/synchronized/override)

(require data/queue)
(require (for-syntax syntax/parse))

(module+ test
  (require rackunit))

(define-member-name enter-synchronized-method (generate-member-key))
(define-member-name exit-synchronized-method (generate-member-key))

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
    ;; (Queueof Semaphore)
    ;; Would an alternative design with more sharing (possibly just a single
    ;; semaphore) be better?
    (define waiters (make-queue))

    (define (acquire!)
      (define current (current-thread))
      (cond
        ;; we are already in the monitor, no need to lock
        [(equal? the-owner current)
         #f]
        [else
         (semaphore-wait the-lock)
         (set! the-owner current)]))

    (define (release!)
      (set! the-owner #f)
      (semaphore-post the-lock))

    (define/public (enter-synchronized-method)
      (acquire!)
      (set! stack-depth (add1 stack-depth)))

    ;; need to keep track of depth, I believe
    (define/public (exit-synchronized-method)
      (define current (current-thread))
      (unless (equal? the-owner current)
        (error 'exit-synchronized-method
               "exit-synchronized-method called by non-owner"))
      (set! stack-depth (sub1 stack-depth))
      (when (zero? stack-depth)
        (release!)))

    ;; wake up the first
    (define/public (notify)
      (unless (equal? the-owner (current-thread))
        (error 'notify "notify called by non-owner"))
      (unless (queue-empty? waiters)
        (define sema (dequeue! waiters))
        (semaphore-post sema)))

    ;; wake up all
    (define/public (notify-all)
      (unless (equal? the-owner (current-thread))
        (error 'notify "notify called by non-owner"))
      (let loop ()
        (unless (queue-empty? waiters)
          (semaphore-post (dequeue! waiters))
          (loop))))

    (define/public (wait)
      (define current (current-thread))
      (unless (equal? the-owner current)
        (error 'wait "wait called by non-owner"))
      (define sema (make-semaphore))
      ;; hold my beer
      (enqueue! waiters sema)
      (define my-stack-depth stack-depth)
      (set! stack-depth 0)
      (release!)
      (semaphore-wait sema)
      (acquire!)
      (set! stack-depth my-stack-depth))))

;; convenience form for creating monitors that are direct subclasses of object%
(define monitor%
  (monitor-mixin object%))

;; how many of these are needed? What should the general form be?

(define-syntax (define/synchronized stx)
  (syntax-parse stx
    [(_ (method-name:id formals:id ...) body:expr ...)
     (with-syntax ([method-body (synchronized-method-body #'(let () body ...))])
     #'(define/public (method-name formals ...)
         method-body))]))

(define-syntax (define/synchronized/override stx)
  (syntax-parse stx
    [(_ (method-name:id formals:id ...) body:expr ...)
     (with-syntax ([method-body (synchronized-method-body #'(let () body ...))])
     #'(define/override (method-name formals ...)
         method-body))]))

(begin-for-syntax
  ;; SyntaxList -> Syntax
  (define (synchronized-method-body body)
    #`(let ()
        (send this enter-synchronized-method)
        (with-handlers ([(const #t) (lambda (e)
                                      (send this exit-synchronized-method)
                                      (raise e))])
          (define result
            #,body)
          (send this exit-synchronized-method)
          result))))

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
    (class (monitor-mixin object%)
      (super-new)
      (field [count 0])
      ;; for the purpose of exercising nesting in synchronized methods
      (define/synchronized (calculate v)
        (add1 v))
      (define/synchronized (incr!)
        (define val count)
        (sleep 0)
        (set! count (calculate val)))))

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

(module+ test
  ;; dining philosophers test; uses the wait queue
  (define dining-table%
    (class (monitor-mixin object%)
      (super-new)
      ;; Positive Integer
      (init places)
      ;; The number of philosophers holding the fork; should always be 0 or 1
      (define forks (make-vector places 0))

      (define (take-fork! i)
        (check-invariants!)
        (vector-set! forks i (add1 (vector-ref forks i)))
        (check-invariants!))

      (define (release-fork! i)
        (check-invariants!)
        (vector-set! forks i (sub1 (vector-ref forks i)))
        (check-invariants!))

      (define (fork-available? i)
        (zero? (vector-ref forks i)))

      (define/synchronized (get-forks i j)
        (let loop ()
          (cond
            [(and (fork-available? i) (fork-available? j))
             (sleep 0)
             (take-fork! i)
             (sleep 0)
             (take-fork! j)
             ]
            [else
             (send this wait)
             (loop)])))

      (define/public (get-forks/unsafe i j)
        (let loop ()
          (cond
            [(and (fork-available? i) (fork-available? j))
             (sleep 0)
             (take-fork! i)
             (sleep 0)
             (take-fork! j)
             ]
            [else
             (sleep .1)
             (loop)])))

      (define/synchronized (release-forks i j)
        (release-fork! i)
        (release-fork! j)
        (send this notify-all))

      (define/public (view)
        (printf "~v\n" forks))

      (field [invariants-violated? #f])
      (define (check-invariants!)
        (for ([f (in-vector forks)])
          (unless (or (zero? f) (= f 1))
            (set! invariants-violated? #t))))))

  (define SNOOZE .1)
  (define DINERS 10)
  (define HUNGER 5)
  (define print-thread
    (thread (lambda ()
            (let loop ()
              (define v (thread-receive))
              (unless (equal? v 'stop)
                (displayln v)
                (loop))))))
  (define (serial-printf . items)
  (thread-send print-thread
               (apply format items)))
  (define (philosopher i table hunger [unsafe? #f])
    (define left-fork i)
    (define right-fork (modulo (add1 i) DINERS))
    (let loop ([hunger hunger])
      (cond
        [(zero? hunger)
         (serial-printf "Philosopher ~v is finished" i)]
        [else
         (serial-printf "Philosopher ~v reaches for forks ~v and ~v" i left-fork right-fork)
         (cond
           [unsafe? (send table get-forks/unsafe left-fork right-fork)]
           [else (send table get-forks left-fork right-fork)])
         (serial-printf "Philosopher ~v obtains forks ~v and ~v" i left-fork right-fork)
         (sleep SNOOZE)
         (serial-printf "Philosopher ~v releases forks ~v and ~v" i left-fork right-fork)
         (send table release-forks left-fork right-fork)
         (serial-printf "Philosopher ~v dozes off" i)
         (sleep SNOOZE)
         (loop (sub1 hunger))])))
  (define (dining-philosophers-test unsafe?)
    (define t (new dining-table% [places DINERS]))
    (define handles
      (for/list ([i (in-range DINERS)])
        (thread (thunk (philosopher i t HUNGER unsafe?)))))
    (for ([thd (in-list handles)])
      (thread-wait thd))
    (check-equal? (get-field invariants-violated? t)
                  unsafe?))
  (dining-philosophers-test #f)
  (dining-philosophers-test #t))

;; test exception handling
(module+ test
  (define exn-test%
    (class (monitor-mixin object%)
      (super-new)
      (define (would-block?)
        (cond
          [(semaphore-try-wait? (get-field the-lock this))
           (semaphore-post (get-field the-lock this))
           #f]
          [else #t]))
      ;; recur to test greater stack depths
      (define/synchronized (fail n)
        (cond
          [(zero? n) (error 'fail)]
          [else (fail (sub1 n))]))
      (define/synchronized (succeed)
        #t)))
  (define et (new exn-test%))
  ;; the exception occurs and propagates
  (check-exn exn:fail? (thunk (send et fail 10)))
  ;; but we can still access the monitor
  (define t (thread (thunk (send et succeed))))
  (sleep 0)
  (define r (sync/timeout .5 (handle-evt t (const #t))))
  (check-true r))
