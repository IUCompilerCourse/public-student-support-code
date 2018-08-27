#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-R0.rkt")
(require "interp-R1.rkt")
(require "compiler.rkt")
(debug-level 4)

(interp-tests "r0" #f r0-passes interp-R0 "r0" (tests-for "r0"))
(interp-tests "r1" #f r1-passes (interp-R1 '()) "r1" (tests-for "r1"))
(display "tests passed!") (newline)
