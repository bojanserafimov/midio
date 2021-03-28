;;; midio-fluidsynth-managed.el --- Fluidsynth server as emacs subprocess -*- lexical-binding: t; -*-
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
;;  This file provides `midio-fluidsynth-managed', which implements
;;  the `midio-fluidsynth-interface'. It prepares by starting a
;;  fluidsynth server as an Emacs subprocess and communicates with
;;  it by sending stdin messages to its buffer.
;;
;;; Code:

(require 'midio-fluidsynth-interface)

(defcustom midio-fluidsynth-audio-driver "portaudio"
  "Audio driver to use with fluidsynth."
  :group 'midio
  :type '(choice
          (const :tag "JACK Audio Connection Kit" "jack")
          (const :tag "Advanced Linux Sound Architecture" "alsa")
          (const :tag "Open Sound System" "oss")
          (const :tag "PulseAudio" "pulseaudio")
          (const :tag "Apple CoreAudio" "coreaudio")
          (const :tag "Microsoft DirectSound" "dsound")
          (const :tag "PortAudio Library" "portaudio")
          (const :tag "Apple SoundManager" "sndman")
          (const :tag "DART sound driver" "dart")
          (const :tag "OpenSL ES" "opensles")
          (const :tag "Oboe" "oboe")
          (const :tag "Microsoft WaveOut, alternative to DirectSound" "waveout")
          (const :tag "Simple DirectMedia Layer" "sdl2*")))

(defcustom midio-soundfont-library nil
  "Starting search directory for `midio-fluidsynth-select-soundfont'."
  :group 'midio
  :type '(choice (directory :tag "Custom directory")
                 (const :tag "Buffer-local default directory" nil)))

(defvar midio-fluidsynth-soundfont nil
  "Soundfont to use with fluidsynth.")

(defun midio-fluidsynth-select-soundfont ()
  "Set `midio-fluidsynth-soundfont'."
  (interactive)
  (setq midio-fluidsynth-soundfont
        (read-file-name
         "Choose soundfont: "
         midio-soundfont-library))
  (midio-fluidsynth-restart))

(defun midio-fluidsynth-get-soundfont ()
  "Return the soundfont to use."
  (or midio-fluidsynth-soundfont
      (midio-fluidsynth-select-soundfont)))

(defvar midio-fluidsynth-server-buffer nil
  "Buffer running the fluidsynth server.")

(defun midio-fluidsynth-server-running ()
  "Return the server buffer if it's running. Return nil otherwise."
  (interactive)
  (ignore-errors
    (midio-fluidsynth-send-message "help")
    midio-fluidsynth-server-buffer))

(defun midio-fluidsynth-start ()
  "Start a fluidsynth server inside Emacs."
  (interactive)
  (setq midio-fluidsynth-server-buffer
        (make-process
         :name "emacs-fluidsynth-server"
         :buffer "fluidsynth-server"
         :command (list
                   "fluidsynth"
                   "--audio-driver" midio-fluidsynth-audio-driver
                   (midio-fluidsynth-get-soundfont)))))

(defun midio-fluidsynth-start-if-not-running ()
  "Start fluidsynth server if not already running."
  (interactive)
  (or
   (midio-fluidsynth-server-running)
   (midio-fluidsynth-start)))

(defun midio-fluidsynth-stop-if-running ()
  "Stop the internal fluidsynth server if it's running."
  (interactive)
  (ignore-errors (midio-fluidsynth-send-message "quit")))

(defun midio-fluidsynth-restart ()
  "Restart fluidsynth server."
  (interactive)
  (midio-fluidsynth-stop-if-running)
  (midio-fluidsynth-start))

(defun midio-fluidsynth-send-message (msg)
  "Send message `MSG' to `midio-fluidsynth-server-buffer'."
  (interactive)
  (process-send-string midio-fluidsynth-server-buffer (concat msg "\n")))

(defconst midio-fluidsynth-managed
  (make-midio-fluidsynth-interface
   :prepare 'midio-fluidsynth-start-if-not-running
   :send-message 'midio-fluidsynth-send-message))

(provide 'midio-fluidsynth-managed)
;;; midio-fluidsynth-managed.el ends here
