;;; yasnippet-org.el --- Generate yasnippet templates from org document  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Takeo Obara

;; Author: Takeo Obara <bararararatty@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.5"))
;; URL: https://github.com/takeokunn/yasnippet-org

;; This program is free software: you can redistribute it and/or modify
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

;; Generate yasnippet templates from org document.

;;; Code:

(require 'subr-x)
(require 'org)
(require 'org-element)

(defgroup yasnippet-org nil
  "Generate yasnippet templates from org document."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/takeokunn/yasnippet-org"))

(defcustom yasnippet-org-file (locate-user-emacs-file "yasnippets.org")
  "File template definition path."
  :group 'yasnippet-org
  :type 'string)

(defcustom yasnippet-org-generate-root (locate-user-emacs-file "snippets")
  "File template definition path."
  :group 'yasnippet-org
  :type 'string)

(defcustom yasnippet-org-target "snippet"
  "Set yasnippet.org h1 tag"
  :group 'yasnippet-org
  :type 'string)

(defvar yasnippet-org--file-buffer nil)

(defun yasnippet-org-file-buffer ()
  "Return yasnippet-org file buffer."
  (setq yasnippet-org--file-buffer (find-file-noselect yasnippet-org-file)))

(defun yasnippet-org-get-heading ()
  "Get `org' heading."
  (with-current-buffer yasnippet-org--file-buffer
    (letrec ((fn (lambda (outer)
                   (mapcar (lambda (inner)
                             (when (eq (car inner) 'headline)
                               (cons (nth 1 inner)
                                     (funcall fn (cddr inner)))))
                           outer))))
      (funcall fn (org-element-contents
                   (org-element-parse-buffer 'headline))))))

(defun yasnippet-org-search-heading (queue)
  "Search QUEUE from `yasnippet-org-get-heading'."
  (cl-find-if (lambda (outer)
                (cl-find-if (lambda (inner)
                              (string= queue (plist-get inner :raw-value)))
                            outer))
              (yasnippet-org-get-heading)))

(defun yasnippet-org-1 (heading parent-directory)
  "Generate file from HEADING."
  (when-let* ((heading* (car-safe heading))
              (title (plist-get heading* :title)))
    (when (and (not (string-suffix-p "/" title))
               (cdr heading))
      (error "Heading %s is not suffixed \"/\", but it have childlen" title))
    (when (string-empty-p title)
      (error "Heading %s will be empty string.  We could not create file with empty name" title))
    (if (string-suffix-p "/" title)
        (progn
          (mkdir (expand-file-name title yasnippet-org-generate-root) 'parent)
          (dolist (elm (cdr heading))
            (yasnippet-org-1 elm title)))
      (let ((src (save-excursion
                   (save-restriction
                     (narrow-to-region (plist-get heading* :begin)
                                       (plist-get heading* :end))
                     (goto-char (point-min))
                     (let ((case-fold-search t))
                       (when (search-forward "#+begin_src" nil 'noerror)
                         (goto-char (match-beginning 0))))
                     (org-element-src-block-parser (point-max) nil)))))
        (unless src
          (error "Node %s/%s has no src block" parent-directory title))
        (let* ((file (expand-file-name title (expand-file-name parent-directory yasnippet-org-generate-root)))
               (srcbody (org-remove-indentation (plist-get (cadr src) :value))))
          (with-temp-file file
            (insert srcbody)))))))

;;;###autoload
(defun yasnippet-org ()
  "Generate yasnippet templates from org document."
  (interactive)
  (with-current-buffer (yasnippet-org-file-buffer)
    (let ((heading (yasnippet-org-search-heading yasnippet-org-target)))
      (unless heading
        (error "%s is not defined at %s" yasnippet-org-target yasnippet-org-file))
      (dolist (elm (cdr heading))
        (yasnippet-org-1 elm nil)))))

(provide 'yasnippet-org)

;;; yasnippet-org.el ends here
