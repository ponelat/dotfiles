;;; fun.el -- It's just for fun
;;; Commentary:

;;; Code:
;; (defun pop-off-prime (coll)
  

(defun seive (max)
  "This gets you a list of primes under MAX."
  (interactive)
  (setq-local x 2)
  (setq-local ar (number-sequence max))
  (while (<= x (sqrt max))
    (progn
      (
       (setq-local x (+ x 2)))))

(seive 15)

(provide 'fun)
;;; fun.el ends here
