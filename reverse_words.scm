;;; https://code.google.com/codejam/contest/351101/dashboard#s=p1
(use-modules (ice-9 rdelim)
             (ice-9 regex))

(define (solve file-in file-out)
  (let* ((in (open-input-file file-in))
         (out (open-output-file file-out))
         (T (string->number (read-line in))))
    (let loop ((i 1))
      (cond ((<= i T)
             (display
              (format #f "Case #~a: ~a\n" i
                      (string-join
                       (reverse (string-split
                                 (read-line in) #\space)))) out)
             (loop (+ i 1)))))))

(unless (> (length (command-line)) 1) (exit 1))

(let ((path (cadr (command-line))))
  (if (string-match ".in$" path)
      (solve path (regexp-substitute/global #f ".in$" path 'pre ".out" 'post))))
