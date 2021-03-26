;;; midio-fluidsynth-fifo.el --- Fifo interface to standalone fluidsynth server -*- lexical-binding: t; -*-
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
;;  This file provides `midio-fluidsynth-fifo', which implements
;;  the `midio-fluidsynth-interface'. It lets you start a standalone
;;  fluidsynth server from outside Emacs and lets you communicate
;;  with it via fifo.
;;
;;  The following initialization instructions would work on most systems:
;;  $ mkdir -p ~/var/midi/emacs
;;  $ mkfifo ~/var/midi/emacs/out.midi
;;  $ cat > ~var/midi/emacs/out.midi  # Keep the fifo alive and send commands if necessary
;;
;;  Then in a separate shell:
;;  $ fluidsynth -a portaudio ~/path/to/soundfont.sf2 < ~/var/midi/emacs/out.midi
;;
;;; Code:

(require 'midio-fluidsynth-interface)

(defcustom midio-fluidsynth-fifo-file "~/var/midi/emacs/out.midi"
  "Output file for midi notes played by Emacs."
  :type '(file :must-match t)
  :group 'midio)

(defun append-object-to-file (filename object)
  "Append `OBJECT' to file `FILENAME', ending with newline."
  (setq write-region-inhibit-fsync nil)
  (with-temp-buffer
    (princ object (current-buffer))
    (terpri (current-buffer) t)
    (let ((inhibit-message t))  ; TODO is this surpressing errors too?
      (append-to-file (point-min) (point-max) filename))))

(defun midio-fluidsynth-fifo-send-message (msg)
  "Send `MSG' to the fluidsynth server at `midio-fluidsynth-fifo-file'."
  (append-object-to-file midio-fluidsynth-fifo-file msg))

(defun midio-fluidsynth-fifo-prepare ()
  "Do nothing.")

(defconst midio-fluidsynth-fifo
  (make-midio-fluidsynth-interface
   :prepare 'midio-fluidsynth-fifo-prepare
   :send-message 'midio-fluidsynth-fifo-send-message))

(provide 'midio-fluidsynth-fifo)
;;; midio-fluidsynth-fifo.el ends here
