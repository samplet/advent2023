;;; script-01.scm: Solve Advent of Code 2023 2.1
;;; Copyright 2023 Timothy Sample <samplet@ngyro.com>
;;; SPDX-License-Identifier: CC0-1.0

(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

(define-record-type <handful>
  (make-handful red green blue)
  handful?
  (red handful-red)
  (green handful-green)
  (blue handful-blue))

(define handful-char-set (string->char-set "bdeglnru0123456789"))

(define (string->handful str)
  (let loop ((parts (string-tokenize str handful-char-set))
             (red 0) (green 0) (blue 0))
    (match parts
      ((count "red" . rest)
       (loop rest (string->number count) green blue))
      ((count "green" . rest)
       (loop rest red (string->number count) blue))
      ((count "blue" . rest)
       (loop rest red green (string->number count)))
      (() (and red green blue (make-handful red green blue)))
      (_ (error "Invalid handful string" str)))))

(define (handful<= h1 h2)
  (and (<= (handful-red h1) (handful-red h2))
       (<= (handful-green h1) (handful-green h2))
       (<= (handful-blue h1) (handful-blue h2))))

(define-record-type <game>
  (make-game id handfuls)
  game?
  (id game-id)
  (handfuls game-handfuls))

(define (string->game str)
  (unless (string-prefix? "Game " str)
    (error "Invalid game string" str))
  (let* ((idx (string-index str #\:))
         (id-str (substring str 5 idx))
         (id (string->number id-str)))
    (unless id
      (error "Invalid game string" str))
    (let* ((handfuls-str (substring str (1+ idx)))
           (handful-strs (string-split handfuls-str #\;))
           (handfuls (map string->handful handful-strs)))
      (make-game id handfuls))))

(define (sum-possible port)
  (define limits (make-handful 12 13 14))
  (let loop ((acc 0))
    (define line (get-line port))
    (if (eof-object? line)
        acc
        (let ((game (string->game line)))
          (if (every (lambda (hf)
                       (handful<= hf limits))
                     (game-handfuls game))
              (loop (+ acc (game-id game)))
              (loop acc))))))

(define (sum-possible-from-file filename)
  (call-with-input-file filename sum-possible))

(match (command-line)
  ((_ input)
   (display (sum-possible-from-file input))
   (newline))
  (_ (error "Invalid args")))
