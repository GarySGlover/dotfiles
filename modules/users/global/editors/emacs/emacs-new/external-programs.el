;;; external-programs.el --- Paths to external programs required by my emacs configuration and custom functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defvar cnit-gsettings-executable (executable-find "gsettings")
  "Path to the gsettings executable.")

(defvar cnit-gdbus-executable (executable-find "gdbus")
  "Path to the gdbus executable.")

(provide 'external-programs)
;;; external-programs.el ends here
