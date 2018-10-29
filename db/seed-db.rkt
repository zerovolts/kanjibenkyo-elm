#!/usr/local/bin/racket
#lang racket

(define file (open-input-file "kana.csv"))

(for ([i 50])
    (let ([line (read-line file)])
        (if (eof-object? line)
            (displayln "none")
            (displayln (string-split line)))))

(close-input-port file)