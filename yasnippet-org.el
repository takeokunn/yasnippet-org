;;; yasnippet-org.el --- Generate yasnippet templates from org document  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Takeo Obara

;; Author: Takeo Obara <bararararatty@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.3") (mustache "0.23"))
;; URL: https://github.com/takeokunn/yasnippet-org.el

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
(require 'mustache)

(defgroup yasnippet-org nil
  "Generate yasnippet templates from org document."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/takeokunn/yasnippet-org"))

(defcustom yasnippet-org-file (locate-user-emacs-file "yasnippet.org")
  "File template definition path."
  :group 'yasnippet-org
  :type 'string)

(defcustom yasnippet-org-edit-recursive-edit nil
  "If non-nil, use `recursive-edit' for `yasnippet-org-edit'."
  :group 'yasnippet-org
  :type 'boolean)

(defcustom yasnippet-org-with-export-as-org t
  "If non-nil, the target's definition is exported as org beforehand.
By exporting as org before generating it is possible to use some additional org
features like including files, macros replacements and the noweb reference
syntax."
  :group 'yasnippet-org
  :type 'boolean)

(defcustom yasnippet-org-target "snippet"
  "yasnippet.org h1 tag"
  :group 'yasnippet-org
  :type 'boolean)

(defcustom yasnippet-org-show-save-message t
  "If non-nil, show message after save files."
  :group 'yasnippet-org
  :type 'boolean)

(defvar yasnippet-org-root nil)
(defvar yasnippet-org-mustache-info nil)
(defvar yasnippet-org--file-buffer nil)

(defun yasnippet-org-file-buffer ()
  "Return yasnippet-org file buffer."
  (or (and (buffer-live-p yasnippet-org--file-buffer)
           yasnippet-org--file-buffer)
      (setq yasnippet-org--file-buffer
            (find-file-noselect yasnippet-org-file))))

(defun yasnippet-org--hash-table-from-alist (alist)
  "Create hash table from ALIST."
  (let ((h (make-hash-table :test 'equal)))
    ;; the first key-value pair in an alist gets precedence, so we
    ;; start from the end of the list:
    (dolist (pair (reverse alist) h)
      (let ((key (car pair))
            (value (cdr pair)))
        (puthash key value h)))))

(defun yasnippet-org-get-heading ()
  "Get `org' heading."
  (with-current-buffer (yasnippet-org-file-buffer)
    (letrec ((fn (lambda (elm)
                   (mapcar
                    (lambda (elm)
                      (when (eq (car elm) 'headline)
                        (cons
                         (nth 1 elm)
                         (funcall fn (cddr elm)))))
                    elm))))
      (funcall
       fn
       (org-element-contents
        (org-element-parse-buffer 'headline))))))

(defun yasnippet-org-candidate ()
  "Get `org' candidate heading for `current-buffer'."
  (let (res)
    (dolist (h1 (yasnippet-org-get-heading))
      (dolist (h2 (cdr h1))
        (push (format "%s/%s"
                      (plist-get (car h1) :raw-value)
                      (plist-get (car h2) :raw-value))
              res)))
    (nreverse res)))

(defun yasnippet-org-search-heading (queue)
  "Search QUEUE from `yasnippet-org-get-heading'."
  (let* ((fn (lambda (h seq)
               (cl-find-if
                (lambda (elm)
                  (cl-find-if
                   (lambda (e)
                     (string= h (plist-get e :raw-value)))
                   elm))
                seq)))
         (lst (split-string queue "/"))
         (h1 (nth 0 lst))
         (h2 (nth 1 lst)))
    (when-let* ((tmp (funcall fn h1 (yasnippet-org-get-heading)))
                (tmp (funcall fn h2 tmp)))
      tmp)))

(defun yasnippet-org--create-string-for-export (heading)
  "Return the string to use for export to org for HEADING in the current buffer.
The string returned consists of the target's heading and its subtree, its parent
heading including the content before the first child , and the content before
the first heading.  This is needed to avoid macro replacments in parts that are
not relevant."
  (let* ((start (plist-get (car heading) :begin))
         regions)
    (save-excursion
      (save-match-data
        ;; Target heading and its subtree.
        (push (cons start (plist-get (car heading) :end)) regions)
        ;; Parent's heading and content.
        (goto-char start)
        (org-up-heading-safe)
        (push (cons (point) (outline-next-heading)) regions)
        ;; Content before first heading.
        (goto-char (point-min))
        (unless (org-at-heading-p)
          (push (cons (point-min) (outline-next-heading)) regions))))
    ;; Create the string for export.
    (apply #'concat
           (mapcar
            (lambda (e) (buffer-substring-no-properties (car e) (cdr e)))
            regions))))

(defvar org-export-with-properties)
(defun yasnippet-org--export-string-as-org (string)
  "Export the STRING as org and return the exported string.
Properties are exported as well."
  (require 'ox-org)
  (let ((org-export-with-properties t))
    (org-export-string-as string 'org t)))

(defun yasnippet-org--with-export (heading)
  "Return a new buffer with the definition for HEADING exported as org."
  (let* ((string-to-export (yasnippet-org--create-string-for-export heading))
         (exported-string (yasnippet-org--export-string-as-org string-to-export))
         (export-buffer (generate-new-buffer "*yasnippet-org-temp*")))
    (with-current-buffer export-buffer
      (insert exported-string)
      (let ((org-inhibit-startup t))
        (org-mode)))
    export-buffer))

;;;###autoload
(defun yasnippet-org-edit ()
  "Open `yasnippet-org-file'."
  (interactive)
  (if yasnippet-org-edit-recursive-edit
      (save-window-excursion
        (save-excursion
          (pop-to-buffer-same-window (yasnippet-org-file-buffer))
          (recursive-edit)))
    (pop-to-buffer-same-window (yasnippet-org-file-buffer))))

(defun yasnippet-org-1 (root heading)
  "Generate file from HEADING.
If ROOT is non-nil, omit some conditions."
  (if root
      (dolist (elm heading)
        (yasnippet-org-1 nil elm))
    (when-let* ((heading* (car-safe heading))
                (title (plist-get heading* :title))
                (title* (mustache-render title yasnippet-org-mustache-info)))
      (when (and (not (string-suffix-p "/" title*)) (cdr heading))
        (error "Heading %s is not suffixed \"/\", but it have childlen" title*))
      (when (string-empty-p title*)
        (error "Heading %s will be empty string.  We could not create file with empty name" title))
      (if (string-suffix-p "/" title*)
          (mkdir (expand-file-name title* default-directory) 'parent)
        (let ((src
               (save-excursion
                 (save-restriction
                   (narrow-to-region
                    (plist-get heading* :begin) (plist-get heading* :end))
                   (goto-char (point-min))
                   (let ((case-fold-search t))
                     (when (search-forward "#+begin_src" nil 'noerror)
                       (goto-char (match-beginning 0))))
                   (org-element-src-block-parser (point-max) nil)))))
          (unless src
            (error "Node %s has no src block" title*))
          (let* ((file (expand-file-name title* default-directory))
                 (srcbody (org-remove-indentation (plist-get (cadr src) :value)))
                 (srcbody* (mustache-render srcbody yasnippet-org-mustache-info)))
            (with-temp-file file
              (insert srcbody*))
            (when yasnippet-org-show-save-message
              (message "[yasnippet-org] Saved: %s" file)))))
      (dolist (elm (cdr heading))
        (let ((default-directory
                (expand-file-name title* default-directory)))
          (yasnippet-org-1 nil elm))))))

;;;###autoload
(defun yasnippet-org ()
  "Generate yasnippet templates from org document."
  (interactive)
  (let ((dir default-directory)
        export-buffer)
    (with-current-buffer (yasnippet-org-file-buffer)
      (let ((heading (yasnippet-org-search-heading yasnippet-org-target)))
        (unless heading
          (error "%s is not defined at %s" yasnippet-org-target yasnippet-org-file))
        ;; Export as org to new buffer before if needed.
        (when yasnippet-org-with-export-as-org
          (setq export-buffer (yasnippet-org--with-export heading))
          (set-buffer export-buffer)
          ;; Update heading positions.
          (let ((yasnippet-org--file-buffer export-buffer))
            (setq heading (yasnippet-org-search-heading yasnippet-org-target))))
        (unwind-protect
            (let* ((fn (lambda (elm)
                         (org-entry-get-multivalued-property
                          (plist-get (car heading) :begin)
                          (symbol-name elm))))
                   (root (funcall fn 'yasnippet-org-root))
                   (vars (funcall fn 'yasnippet-org-variable))
                   (beforehooks (funcall fn 'yasnippet-org-before-hook))
                   (afterhooks  (funcall fn 'yasnippet-org-after-hook)))
              (setq root (expand-file-name
                          (or yasnippet-org-root
                              (car root)
                              (read-file-name "Generate root: " dir))))
              (unless (file-directory-p root)
                (error "%s is not directory" root))
              (let ((default-directory root)
                    (yasnippet-org-mustache-info
                     (or yasnippet-org-mustache-info
                         (yasnippet-org--hash-table-from-alist
                          (mapcar (lambda (elm)
                                    (cons elm (read-string (format "%s: " elm))))
                                  vars)))))
                (when beforehooks
                  (dolist (elm beforehooks)
                    (funcall (intern elm))))

                (yasnippet-org-1 t heading)

                (when afterhooks
                  (dolist (elm afterhooks)
                    (funcall (intern elm))))
                (when (called-interactively-p 'interactive)
                  (dired root))))
          (when export-buffer
            (kill-buffer export-buffer)))))))

(provide 'yasnippet-org)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; yasnippet-org.el ends here
