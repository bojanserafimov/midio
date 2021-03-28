;;; the-lick.el --- Plays the famous lick -*- lexical-binding: t; -*-
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
;; Plays the lick via the midi scheduler.
;;
;;; Code:

(require 'midio)
(require 'midio-lang-base)

(midio-interpret
 (midio-batch
  (midio-voice
   (list
    (midio-on 64  98) (midio-sit 0.2)
    (midio-on 66 105) (midio-sit 0.2)
    (midio-on 67 100) (midio-sit 0.2)
    (midio-on 69  95) (midio-sit 0.2)
    (midio-on 66  95) (midio-sit 0.3) (midio-off 66) (midio-sit 0.1)
    (midio-on 62  92) (midio-sit 0.2)
    (midio-on 64  93) (midio-sit 0.6)))))

(provide 'the-lick)
;;; the-lick.el ends here
