;;; midio-fluidsynth-interface.el --- Fluidsynth server interface -*- lexical-binding: t; -*-
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
;;  This file provides `midio-fluidsynth-interface' struct.
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct midio-fluidsynth-interface
  (prepare :read-only t :documentation "Function to call before playback.")
  (send-message :read-only t :documentation "Send message to server."))

(provide 'midio-fluidsynth-interface)
;;; midio-fluidsynth-interface.el ends here
