#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-R0.rkt")
(require "compiler.rkt")

;; (debug-level 4)

(interp-tests "integers and arithmetic" #f r0-passes interp-R0 "r0" (range 1 4))
(display "tests passed!") (newline)
