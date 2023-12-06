;;; script-02.scm: Solve Advent of Code 2023 2.2
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

(define (handful-power hf)
  (* (handful-red hf)
     (handful-green hf)
     (handful-blue hf)))

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

(define (minimum-handful game)
  (match (game-handfuls game)
    ((hf0 . rest)
     (let loop ((hfs rest)
                (red (handful-red hf0))
                (green (handful-green hf0))
                (blue (handful-blue hf0)))
       (match hfs
         (() (make-handful red green blue))
         ((hf . rest)
          (loop rest
                (max red (handful-red hf))
                (max green (handful-green hf))
                (max blue (handful-blue hf)))))))))

(define (sum-power-of-minimum port)
  (let loop ((acc 0))
    (define line (get-line port))
    (if (eof-object? line)
        acc
        (let* ((game (string->game line))
               (mhf (minimum-handful game))
               (power (handful-power mhf)))
          (loop (+ acc power))))))

(define (sum-power-of-minimum-from-file filename)
  (call-with-input-file filename sum-power-of-minimum))

(match (command-line)
  ((_ input)
   (display (sum-power-of-minimum-from-file input))
   (newline))
  (_ (error "Invalid args")))
