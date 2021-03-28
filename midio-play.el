;;; midio.el --- Midi playback utils -*- lexical-binding: t; -*-
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
;;  This package provides a server-agnostic interface for playing midi
;;  via functions `midio-play-batch' and `midio-all-notes-off'.
;;
;;; Code:

(require 'cl-lib)
(require 'midio-fluidsynth-managed)
(require 'midio-fluidsynth-fifo)
(require 'midio-lang-base)

(defcustom midio-fluidsynth-method midio-fluidsynth-managed
  "Method to interface with fluidsynth for midi playback."
  :type '(choice
          (const :tag "Managed fluidsynth server as subprocess" midio-fluidsynth-managed)
          (const :tag "Standalone fluidsynth server listening to fifo" midio-fluidsynth-fifo))
  :group 'midio)

(defvar midio--pressed-keys nil
  "Pitches of midi notes currently pressed. May contain duplicates.")

(defun midio-play-instruction (i)
  "Update internal state and represent `I' as fluidsynth command."
  (cond ((midio-i-on-p i)
         (setq midio--pressed-keys (cons (midio-i-on-pitch i)
                                         midio--pressed-keys))
         (format "noteon 0 %d %d"
                 (midio-i-on-pitch i)
                 (midio-i-on-velocity i)))
        ((midio-i-off-p i)
         (format "noteoff 0 %d"
                 (midio-i-off-pitch i)))))

(defun midio-play-batch (batch)
  "Play `BATCH' of instructions."
  (let ((msg (mapconcat 'identity (mapcar 'midio-play-instruction batch) "\n")))
    (funcall (midio-fluidsynth-interface-send-message midio-fluidsynth-method) msg)))

(defun midio-all-notes-off ()
  "Depress all notes."
  (midio-play-batch (mapcar 'midio-off midio--pressed-keys))
  (setq midio--pressed-keys nil))

(provide 'midio-play)
;;; midio-play.el ends here
