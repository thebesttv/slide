;; Download reveal.js 4.3.1 from:
;;   https://github.com/hakimel/reveal.js/archive/refs/tags/4.3.1.zip
;; and uncompress it under current directory as 'reveal.js'

(defcustom eserver-slide (expand-file-name "slide" eserver-root)
  "Root directory of slide."
  :group 'eserver
  :type 'directory)

(eserver-register-site "/slide"
  "Present a list of reveal.js slides.")

(defun eserver-slide-insert-directory (dir)
  "Insert to buffer three HTML slide links under DIR."
  (let ((dir (file-name-nondirectory dir)))
    (let* ((slide-url (format "/slide/%s/%s.html" dir dir))
           (print-url (format "%s?print-pdf" slide-url))
           (raw-org (format "/slide/%s/%s.org" dir dir)))
      (insert (format "  <a href=\"%s\">%s</a>" slide-url dir print-url)
              ", "
              (format "<a href=\"%s\">[printable pdf]</a>" print-url)
              ", "
              (format "<a href=\"%s\">[raw]</a>" raw-org)
              "\n"))))

(defun eserver-map-every (pred map)
  "Applies every element of MAP to PRED, evaluates to t only if all results are non-nil."
  (catch 'map--break
    (mapc (lambda (value)
                 (or (funcall pred value)
                     (throw 'map--break nil)))
          map)
    t))

(defun eserver-slide-directory-p (dir)
  "Determines if DIR contains a slide.

DIR should be
- an absolute path
- under `eserver-slide'
- is a directory
- is not 'reveal.js'
- contains '.org' and '.html' file with the same name as
  (file-name-nondir DIR).  For example, if DIR is '~/eserver/slide/nlp',
  then there sholud be two files 'nlp.org' and 'nlp.html' under DIR."
  (let ((dir-name (file-name-nondirectory dir)))
    (message (expand-file-name (format "%s.org" dir-name) dir))
    (and (eserver-file-safe-p dir t eserver-slide)
         (file-directory-p dir)
         (not (string-equal (expand-file-name "reveal.js"
                                              eserver-slide)
                            dir))
         (eserver-map-every
          (lambda (suffix)
            (eserver-file-safe-p
             (expand-file-name (string-join (list dir-name suffix))
                               dir)
             nil eserver-slide))
          (list ".org" ".html")))))

(defun httpd/slide (proc path &rest args)
  (if (or (string-equal "/slide" path)
          (string-equal "/slide/" path))
      ;; print index page
      (with-httpd-buffer proc "text/html; charset=utf-8"
        (insert "<pre>")
        (insert "Available slides:\n")
        (mapc (lambda (file)            ; `file' is absolute path
                (when (eserver-slide-directory-p file)
                  (eserver-slide-insert-directory file)))
              (directory-files eserver-slide t
                               ;; dircard ".", "..", hidden, and emacs-backup files
                               (rx line-start
                                   (not (or ?. ?#))
                                   (* anything)
                                   (not (or ?~ ?#))
                                   line-end)))
        (insert "</pre>"))
    (let ((file (expand-file-name
                 (string-join (list "."
                                    (string-remove-prefix "/slide" path)))
                 eserver-slide)))
      (if (not (eserver-file-safe-p file))
          (with-httpd-buffer proc "text/plain; charset=utf-8"
            (insert "ERROR: This file does not exist:\n  " file "\n"))
        (if (string-suffix-p ".org" file)
            (with-httpd-buffer proc "text/plain; charset=utf-8"
              (insert-file-contents-literally file))
          (httpd-send-file proc file))))))
