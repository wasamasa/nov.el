;;; org-nov.el --- Support for links to nov.el buffers within org-mode.

;; Copyright (C) 2017-2019 Vasilij Schneidermann <mail@vasilij.de>

;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; URL: https://github.com/wasamasa/nov.el
;; Version: 0.2.9
;; Package-Requires: ((dash "2.12.0") (esxml "0.3.3") (emacs "24.4"))
;; Keywords: hypermedia, multimedia, epub

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org)


;;; org interop

(defun nov-org-link-follow (path)
  (require 'nov)
  (unless (string-match "^\\(.*\\)::\\([0-9]+\\):\\([0-9]+\\)$" path)
    (error "Invalid nov.el link"))
  (let ((file (match-string 1 path))
        (index (string-to-number (match-string 2 path)))
        (point (string-to-number (match-string 3 path))))
    (find-file file)
    (unless (eq major-mode 'nov-mode)
      (error "Major mode not nov.el"))
    (when (and (boundp 'nov-documents-index)
               (fboundp 'nov-render-document))
      (setq nov-documents-index index)
      (nov-render-document)
      (goto-char point))))

(defun nov-org-link-store ()
  (when (and (eq major-mode 'nov-mode)
             (boundp 'nov-file-name)
             (boundp 'nov-documents-index))
    (org-store-link-props
     :type "nov"
     :link (format "nov:%s::%d:%d" nov-file-name nov-documents-index (point))
     :description (format "EPUB file at %s" nov-file-name))))

(org-link-set-parameters
 "nov"
 :follow 'nov-org-link-follow
 :store 'nov-org-link-store)

(provide 'org-nov)
;;; org-nov.el ends here
