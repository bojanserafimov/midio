;;; midio-hydra.el --- Keyboard interface for controlling playback -*- lexical-binding: t; -*-
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
;;  This package provides `midio-hydra/body' which spawns the
;;  hydra keyboard interface, and `midio-hydra-process-event'
;;  which passes keypress events to the hydra.
;;
;;; Code:

(require 'hydra)
(require 'midio-play)
(require 'midio-scheduler)

(defun midio-kill ()
  "Stop playback."
  (interactive)
  (setq midio-upcoming-events nil)
  (midio-all-notes-off)
  (message "Done."))

(defun midio--off-only-batch-p (batch)
  "Test whether `BATCH' has only off events."
  (or (not batch)
      (and (midio-i-off-p (car batch))
           (midio--off-only-batch-p (cdr batch)))))

(defun midio-hold-step ()
  "Advance to the next batch."
  (interactive)
  (midio-schedule-next-event 0)
  (while (and (cadr midio-upcoming-events)
              (midio--off-only-batch-p (midio-i-play-and-sit-batch (car midio-upcoming-events))))
    (let ((popped (pop midio-upcoming-events))
          (first (car midio-upcoming-events)))
      (setf (midio-i-play-and-sit-batch first)
            (append (midio-i-play-and-sit-batch popped)
                    (midio-i-play-and-sit-batch first)))))
  (let ((first (car midio-upcoming-events)))
    (if first (setf (midio-i-play-and-sit-duration first) 10000))))

(defun midio-hold-release ()
  "Release hold."
  (interactive)
  (midio-schedule-next-event 0)
  (midio-hydra/body))

(defun midio-not-implemented ()
  "Apply some transformations."
  (interactive)
  (message "Unimplemented midio-hydra function called."))

(defhydra midio-hydra-hold (:pre (progn
                                  (midio-schedule-next-event 100000)
                                  (message "Holding notes ..."))
                            :hint nil
                            :color pink)
  "
_s_: step
_r_: release
_k_: kill"
  ("s" midio-hold-step)
  ("k" midio-kill :exit t)
  ("r" midio-hold-release :exit t))

(defhydra midio-hydra (:pre (message "Playing midi ...")
                       :hint nil
                       :color pink)
  "
_h_: hold
_k_: kill
"
  ("k" midio-kill :exit t)
  ("h" midio-hydra-hold/body :exit t))

(defun midio-hydra-process-event (event)
  "Process `EVENT'."
  (let ((key (cdr event)))
    ; (funcall (lookup-key midio-hydra/keymap key))))
    (let ((func (cdr (assoc key (cdr hydra-curr-map)))))
      (if func (funcall func)))))

(provide 'midio-hydra)
;;; midio-hydra.el ends here
