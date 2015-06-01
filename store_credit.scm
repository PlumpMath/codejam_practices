;;; https://code.google.com/codejam/contest/351101/dashboard#s=p0
;;; Store Credit
(use-modules (ice-9 rdelim)
             (ice-9 regex))

(define (price money len alt)
  (let loop ((i 1) (j 2) (val (car alt)) (l (cdr alt)))
    (cond ((> j len) '())
          ((= money (+ val (car l))) (cons i j))
          (else
           (let ((result (loop i (+ j 1) val (cdr l))))
             (if (and (null? result) (= i (- j 1)))
                 (loop (+ i 1) (+ j 1) (car l) (cdr l))
                 result))))))

(define (solve file-in file-out)
  (let* ((in (open-input-file file-in))
         (out (open-output-file file-out))
         (x (string->number (read-line in))))

    (let loop ((i 0))
      (if (< i x)
          (let ((result (price (string->number (read-line in))
                               (string->number (read-line in))
                               (map string->number (string-split (read-line in) #\space)))))
            (display (format #f "Case #~a: ~a ~a\n" (+ i 1) (car result) (cdr result)) out)
            (loop (+ i 1)))))))

(unless (> (length (command-line)) 1) (exit 1))

(let ((path (cadr (command-line))))
  (if (string-match ".in$" path)
      (solve path (regexp-substitute/global #f ".in$" path 'pre ".out" 'post))))
