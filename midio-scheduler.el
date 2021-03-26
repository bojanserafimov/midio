;;; midio-scheduler.el --- Schedule score events -*- lexical-binding: t; -*-
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
;;  This package provides mutable state and utils for interacting
;;  with it.
;;
;;; Code:

(defvar midio-current-score nil
  "The score currently playing.")

(defvar midio-upcoming-events nil
  "The upcoming midi events scheduled.")

(defvar midio-next-event-time nil
  "The time the next event is scheduled for.")

(defun monotonic-time ()
  "Return time in float seconds that is meant to be subtracted."
  ; TODO actually implement monotonic time. This won't fly.
  (float-time))

(defcustom midio-seconds-to-wait-before-playing 0.3
  "Number of seconds the scheduler should wait before playing music with."
  :type 'float
  :group 'midio)

(defun midio-sit-until-next-event ()
  "Wait until next event and return t. On user input, stop and return f."
  (sit-for (- midio-next-event-time (monotonic-time))))

(defun midio-schedule-next-event (time)
  "Schedule next event in `midio-upcoming-events' to happen in `TIME' seconds."
  (setq midio-next-event-time (+ (monotonic-time) time)))


(provide 'midio-scheduler)
;;; midio-scheduler.el ends here
