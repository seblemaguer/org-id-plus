;;; org-id+.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 30 December 2017
;;

;; Author:  <slemaguer@coli.uni-saarland.de>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; org-id+ is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; org-id+ is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with org-id+.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defvar org-id+-set '())

(defun org-id+-new  (&optional pom)
  (interactive)
  (org-with-point-at pom
    (let* ((cleaned-title (downcase (replace-regexp-in-string "[^[:alnum:] ]" "" (org-entry-get nil "ITEM"))))
           (elts (split-string cleaned-title))
           (base-id (string-join (subseq elts 0 (min 3 (safe-length elts))) "-"))
           (i 2)
           (id base-id))
      (while (member id org-id+-set)
        (setq id (format "%s%s%d" base-id "-" i))
        (setq i (1+ i)))
      (add-to-list 'org-id+-set id)
      id)))

(defun org-id+-get-id (id-property &optional pom create prefix)
  "Get the ID property identified by ID-PROPERTY of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an ID as identified by ID-PROPERTY, the function
   returns nil. However, when CREATE is non nil, create a a
   property ID-PROPERTY if none is present already. PREFIX will
   be passed through to `org-id-new'. In any case, the value of
   the ID-PROPERTY of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil id-property)))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (concat "sec:" (org-id+-new pom)))
        (org-entry-put pom id-property id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun org-id+-add-id-to-current-headline ()
  "Generate and add an ID for the headline at the current point."
  (interactive)
  (org-id+-get-custom-id "ID" (point) 'create))

(defun org-id+-add-custom-id-to-current-headline ()
  "Generate and add a CUSTOM_ID for the headline at the current point."
  (interactive)
  (org-id+-get-custom-id "CUSTOM_ID" (point) 'create))

(defun org-id+-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which do
   not already have one."
  (interactive)
  (org-map-entries (lambda () (org-id+-add-id-to-current-headline))))

(defun org-id+-add-custom-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (org-id+-add-custom-id-to-current-headline))))

(provide 'org-id+)

;;; org-id+.el ends here
