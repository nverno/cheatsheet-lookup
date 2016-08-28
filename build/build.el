(require 'json)

;; convert json output to an elisp data structure
;; @@FIXME: still not sure best way to store, currently as an alist

(defvar python (if (eq system-type 'windows-nt)
                   "C:/Program Files/Anaconda/python.exe"
                 "python"))

(defun batch-convert ()
  (defvar command-line-args-left)
  (let ((error nil))
    (while command-line-args-left
      (let* ((infile (car command-line-args-left))
             (outfile (concat (file-name-sans-extension infile) ".dat")))
        (message "%s -> %s" infile outfile)
       (build-el infile outfile))
      (setq command-line-args-left (cdr command-line-args-left)))))

(defun build-el (in out)
  (save-hash (build-hash in) (expand-file-name out)))

(defun build-hash (file)
  (let* ((json-key-type 'string)
         (lst (json-read-file file))
         (ht (make-hash-table :test 'equal :size (length lst))))
    (mapc (lambda (item) (puthash (car item) (cdr item) ht)) lst)
    ht))

(defun hash-to-alist (hash)
  (let (res)
    (maphash
     (lambda (key value)
       (setq res (cons (cons key value) res)))
     hash)
    res))

(defun save-hash (hash file)
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string (hash-to-alist hash)))
      (write-region (point-min) (point-max) file))))

(defun scrape-data ()
  (let ((proc
         (start-process "cheatsheet" "*cheatsheet output*"
                        python
                        "cheatsheet-lookup.py")))
    (set-process-sentinel proc
                          #'(lambda (p s)
                              (message "%s : %s" p s)
                              (build-el)))))
