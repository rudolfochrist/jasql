;;; orgtexi.el --- Org texinfo customizations

;;; Commentary:
;; Stuff to make org and texinfo work better together.

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Code:

(require 'ox-texinfo)

;;; use strike-through to markup vars in texinfo
(add-to-list 'org-texinfo-text-markup-alist '(strike-through . "@var{%s}"))

(defun org-texinfo-ref-open (path)
  "Open the reference.

PATH is the reference headline."
  (let ((headline (org-find-exact-headline-in-buffer path (current-buffer) t)))
    (if headline
        (goto-char headline)
      ;; try to find anchor
      (let ((anchor (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (search-forward (format "@anchor{%s}" path) nil t)
                        (when (match-string 0)
                          (point))))))
        (when anchor
          (goto-char anchor))))))

(defun org-texinfo-reference-export (ref-type path description backend)
  "Format the texinfo reference."
  (when (eql backend 'texinfo)
    (format "@%s{%s}"
            ref-type
            (if description
                (format "%s,,%s" path description)
              path))))

(defun org-texinfo-xref-export (path desc backend)
  (org-texinfo-reference-export "xref" path desc backend))

(org-link-set-parameters "texixref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-xref-export)

(defun org-texinfo-ref-export (path desc backend)
  (org-texinfo-reference-export "ref" path desc backend))

(org-link-set-parameters "texiref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-ref-export)

(defun org-texinfo-pxref-export (path desc backend)
  (org-texinfo-reference-export "pxref" path desc backend))

(org-link-set-parameters "texipxref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-pxref-export)


(provide 'orgtexi)

;;; orgtexi.el ends here
