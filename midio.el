;;; midio.el --- Live midi player -*- lexical-binding: t; -*-
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
;; This package provides `midio-interpret' which plays a score
;; live with an interactive keyboard interface for controlling
;; playback and operating on the music.
;;
;; Run the tests in the `tests' directory to get started.
;;
;;; Code:

(require 'midio-play)
(require 'midio-scheduler)
(require 'midio-hydra)

(defun midio-interpret (score)
  "Play `SCORE' and interpret user input all in a single thread."
  (setq midio-current-score score)
  (setq midio-upcoming-events score)
  (midio-schedule-next-event midio-seconds-to-wait-before-playing)
  (funcall (midio-fluidsynth-interface-prepare midio-fluidsynth-method))
  (midio-hydra/body)
  (while midio-upcoming-events
    (while (and midio-upcoming-events (midio-sit-until-next-event))
      (let ((event (pop midio-upcoming-events)))
        (midio-play-batch (midio-instruction-play-and-sit-batch event))
        (midio-schedule-next-event (midio-instruction-play-and-sit-duration event))))
    (let ((events unread-command-events))
      (discard-input)
      (mapc 'midio-hydra-process-event events)))
  (midio-hydra/midio-kill-and-exit)
  nil)

(defun midio--eval (sexp)
  "Evaluate `SEXP' and interpret the resulting score."
  (midio-interpret (eval (macroexpand-all sexp) lexical-binding)))

(require 'eros)
(defun midio-eval-sexp ()
  "Eval music s-expression at point and show the return value."
  (interactive)
  (let ((result (midio--eval (elisp--preceding-sexp))))
    (eros--eval-overlay result (point))))

(provide 'midio)
;;; midio.el ends here
