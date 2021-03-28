;;; midio-lang-base.el --- Basic instruction structs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bojan Serafimov
;;
;; Author: Bojan Serafimov <https://github.com/bojanserafimov>
;; Maintainer: Bojan Serafimov
;; Version: 0.1.0
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
;;  This package provides low-level music instruction structs.
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct
    (midio-i-on
     (:constructor midio-on
      (pitch &optional (velocity 90))))
  pitch velocity)

(cl-defstruct
    (midio-i-off
     (:constructor midio-off
      (pitch)))
  pitch)

(cl-defstruct
    (midio-i-sit
     (:constructor midio-sit
      (duration)))
  duration)

(cl-defstruct
    (midio-i-play-and-sit
     (:constructor midio-play-and-sit
      (batch duration)))
  batch duration)

(defun midio-batch (instructions)
  "Group `INSTRUCTIONS' between sits into lists."
  (let (buffer result)
    (dolist (i instructions (append result (list (midio-play-and-sit buffer 0))))
      (if (midio-i-sit-p i)
          (progn (setq result
                       (append result
                               (list (midio-play-and-sit
                                buffer
                                (midio-i-sit-duration i)))))
                 (setq buffer nil))
        (setq buffer (append buffer (list i)))))))

(defun midio-voice (instructions)
  "Sprinkle some midio-off into `INSTRUCTIONS' to make sure there's one voice."
  (let (stack active)
    (dolist (i instructions (reverse (if active (cons (midio-off active) stack) stack)))
      (when (midio-i-on-p i)
        (when active (setq stack (cons (midio-off active) stack)))
        (setq active (midio-i-on-pitch i)))
      (when (and (midio-i-off-p i) (equal stack (midio-i-off-pitch i)))
        (setq active nil))
      (setq stack (cons i stack)))))

(provide 'midio-lang-base)
;;; midio-lang-base.el ends here
