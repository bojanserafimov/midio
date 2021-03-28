;;; scales.el --- Plays scales smoothly -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bojan Serafimov
;;
;; Author: Bojan Serafimov <https://github.com/bojanserafimov>
;; Maintainer: Bojan Serafimov
;; Version: 0.0.1
;; Keywords: midi music sound soundfont fluidsynth composition
;; Homepage: https://github.com/bojanserafimov/midio
;; Package-Requires: ((emacs "24.3") (hydra "0.15.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Just playing scales and trying to make it sound human.
;;
;;; Code:

(require 'midio)
(require 'midio-lang-base)

(defun midio-tanh (x)
  "Return hyperbolic tangent of `X'."
  (/ (- (exp x) (exp (- x)))
     (+ (exp x) (exp (- x)))))

(defun midio-e-gentle (time)
  "Return value of gentle envelope for `TIME' in (0, 1].

Fast fade in and slow taper. The goal is to have no notes
sticking out (not even the first one) when playing a scale
under this envelope. The weight of the hand is balanced at
a little bit after the first note. The formula using tanh
and sqrt is just an arbitrary interpolation of the series I
arrived at by hand-tuning each note individually.
"
  (let ((x (* 2.4 time)))
    (/ (midio-tanh x) (sqrt x))))

(midio-interpret
 (midio-batch
  (midio-voice
   (list
    (midio-on 60 60) (midio-sit 0.21)
    (midio-on 62 89) (midio-sit 0.2)
    (midio-on 64 92) (midio-sit 0.2)
    (midio-on 65 92) (midio-sit 0.19)
    (midio-on 67 85) (midio-sit 0.21)
    (midio-on 69 79) (midio-sit 0.2)
    (midio-on 71 74) (midio-sit 0.19)
    (midio-on 72 71) (midio-sit 0.6)))))

; TODO
; - implement compiler pass that abstracts this
(midio-interpret
 (midio-batch
  (midio-voice
   (list
    (midio-on 60 (* 120.0 (midio-e-gentle (/ 1.0 8.0)))) (midio-sit 0.21)
    (midio-on 62 (* 120.0 (midio-e-gentle (/ 2.0 8.0)))) (midio-sit 0.2)
    (midio-on 64 (* 120.0 (midio-e-gentle (/ 3.0 8.0)))) (midio-sit 0.2)
    (midio-on 65 (* 120.0 (midio-e-gentle (/ 4.0 8.0)))) (midio-sit 0.19)
    (midio-on 67 (* 120.0 (midio-e-gentle (/ 5.0 8.0)))) (midio-sit 0.21)
    (midio-on 69 (* 120.0 (midio-e-gentle (/ 6.0 8.0)))) (midio-sit 0.2)
    (midio-on 71 (* 120.0 (midio-e-gentle (/ 7.0 8.0)))) (midio-sit 0.19)
    (midio-on 72 (* 120.0 (midio-e-gentle (/ 8.0 8.0)))) (midio-sit 0.6)))))


(provide 'scales)
;;; scales.el ends here
