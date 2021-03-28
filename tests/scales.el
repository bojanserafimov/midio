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
    (midio-on 71 78) (midio-sit 0.19)
    (midio-on 72 71) (midio-sit 0.6)))))

(provide 'scales)
;;; scales.el ends here
