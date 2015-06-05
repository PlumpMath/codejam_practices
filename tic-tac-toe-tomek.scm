;;; https://code.google.com/codejam/contest/2270488/dashboard
(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-11))

(define X #\X)
(define O #\O)
(define T #\T)
(define DOT #\.)

(define (line-won? who line)
  (= 4 (length (filter (lambda (x) (or (equal? x who)
                                  (equal? x T))) line)))) ; T is neutral

(define (vertical-won? who tb)
  (cond ((null? tb) #f)
        ((line-won? who (car tb)) #t)
        (else (vertical-won? who (cdr tb)))))

(define (horizontal-tb tb)
  (let*-values (((l1 l2 l3 l4) (apply values tb)))
    (map (lambda (w x y z) (list w x y z)) l1 l2 l3 l4)))

(define (horizontal-won? who tb)
  (let loop ((tb (horizontal-tb tb)))
    (cond ((null? tb) #f)
          ((line-won? who (car tb)) #t)
          (else (loop (cdr tb))))))

(define (diagonal-tb1 tb)
  (let loop ((i 0) (result '()) (tb tb))
    (cond ((> i 3) result)
          (else (loop (+ i 1) (cons (list-ref (car tb) i) result) (cdr tb))))))

(define (diagonal-tb2 tb)
  (let loop ((i 3) (result '()) (tb tb))
    (cond ((< i 0) result)
          (else
           (loop (- i 1) (cons (list-ref (car tb) i) result) (cdr tb))))))

(define (diagonal-won? who tb)
  (cond ((line-won? who (diagonal-tb1 tb)) #t)
        ((line-won? who (diagonal-tb2 tb)) #t)
        (else #f)))

(define (draw? tb)
  (cond ((null? tb) #t)
        ((< 0 (length (filter (lambda (x) (equal? x DOT)) (car tb)))) #f)
        (else (draw? (cdr tb)))))

;;; It can be more fast
(define (solve-tomek tb)
  (cond ((or (vertical-won? X tb) (horizontal-won? X tb)
             (diagonal-won? X tb)) "X won")
        ((or (vertical-won? O tb) (horizontal-won? O tb)
             (diagonal-won? O tb)) "O won")
        ;; Check empty or draw
        ((draw? tb) "Draw")
        (else "Game has not completed")))

(define read-tomek
  (case-lambda
    ((in) (read-tomek in 0 '()))
    ((in i l) (cond ((< i 4)
                     (read-tomek in (+ i 1)
                                 (cons (string->list (read-line in)) l)))
                    (else (reverse l))))))

(define (solve file-in file-out)
  (let* ((in (open-input-file file-in))
         (out (open-output-file file-out)))
    (let loop ((i 1))
      (if (not (eof-object? (read-line in)))
          (begin
            (display (format #f "Case #~a: ~a\n" i
                             (solve-tomek (read-tomek in))) out)
            (loop (+ i 1)))))))

(unless (> (length (command-line)) 1) (exit 1))

(let ((path (cadr (command-line))))
  (if (string-match ".in$" path)
      (solve path (regexp-substitute/global #f ".in$" path 'pre ".out" 'post))))
