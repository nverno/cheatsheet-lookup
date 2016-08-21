(require 'json)

(defun scrape-data ()
  (let ((proc
         (start-process "cheatsheet" "*cheatsheet output*"
                        (if (eq system-type 'windows-nt)
                            nvp-python-program
                          "python")
                        "cheatsheet-lookup.py")))
    (set-process-sentinel proc
                          #'(lambda (p s)
                              (message "%s : %s" p s)
                              (build-el)))))

(defun build-el ()
  (save-hash (build-hash "cheatsheet-lookup-data.json")
             (expand-file-name "cheatsheet-lookup-data.el" "..")))

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
