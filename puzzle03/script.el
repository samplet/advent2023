;;; script.el --- Solve Advent of Code 2023 3
;;; Commentary:
;;;
;;; I forget to save the solutions to the parts separately, so this
;;; provides solutions to both part one and part two.
;;;
;;; To use this code, open the input file in Emacs and evaluate
;;; '(sum-part-numbers)'.
;;;
;;; Code:

(defun move-line (&optional count)
  "Move lines by COUNT."
  (let* ((col (- (point) (line-beginning-position)))
         (delta (forward-line count)))
    (and (zerop delta)
         (not (eobp))
         (forward-char col))
    delta))

(defun digitp (char)
  "Check if CHAR is a digit."
  (<= ?0 char ?9))

(defun engine-symbolp (char)
  "Check if CHAR is an engine symbol character."
  (not (or (digitp char) (char-equal char ?.))))

(defun gear-symbolp (char)
  "Check if CHAR is a gear symbol character."
  (char-equal char ?*))

(defun above-or-belowp (pred)
  "Check if PRED is true above, below, or at the point."
  (or (save-excursion
        (and (zerop (move-line -1))
             (funcall pred (char-after))
             (point)))
      (and (funcall pred (char-after))
           (point))
      (save-excursion
        (and (zerop (move-line 1))
             (not (eobp))
             (funcall pred (char-after))
             (point)))))

(defun part-numberp (&optional symbol-pred)
  "Check if the point at the beginning of a part number.

Use SYMBOL-PRED to detect engine symbol characters."
  (setq symbol-pred (or symbol-pred #'engine-symbolp))
  (save-excursion
    (or (and (not (bolp))
             (save-excursion
               (forward-char -1)
               (above-or-belowp symbol-pred)))
        (let ((acc nil))
          (while (and (not acc) (digitp (char-after)))
            (setq acc (above-or-belowp symbol-pred))
            (forward-char 1))
          acc)
        (and (not (eolp))
             (above-or-belowp symbol-pred)))))

(defun sum-part-numbers ()
  "Output the sum of all part numbers in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((acc 0)
          (gear-parts (make-hash-table))
          (gacc 0))
      (while (progn (skip-chars-forward "^0-9") (not (eobp)))
        (save-excursion
          (when (part-numberp)
            (re-search-forward "[0-9]+")
            (setq acc (+ acc (string-to-number (match-string 0))))))
        (save-excursion
          (let ((p (part-numberp #'gear-symbolp)))
            (when p
              (re-search-forward "[0-9]+")
              (let ((n (string-to-number (match-string 0))))
                (puthash p (cons n (gethash p gear-parts)) gear-parts)))))
        (skip-chars-forward "0-9"))
      (maphash (lambda (p ns)
                 (when (= (length ns) 2)
                   (setq gacc (+ gacc (apply #'* ns)))))
               gear-parts)
      (cons acc gacc))))

(provide 'script)
;;; script.el ends here
