;;; https://code.google.com/codejam/contest/2270488/dashboard
(use-modules (ice-9 rdelim)
             (ice-9 regex))

(define (solve-tomek tb)
  "Draw")

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
