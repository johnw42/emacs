;;; scheme-internal.el --- Summary -*- lexical-binding: t -*-
;;; Commentary
;;; Code:

(defvar scheme-value-ref-tag (make-symbol "scheme-value-ref-tag"))
(defvar scheme-id-to-value-ref-table
  (make-hash-table :test 'eql :weakness 'value))
(defvar lisp-id-to-object-table (make-hash-table :test 'eql))
(defvar lisp-object-to-id-table (make-hash-table :test 'eq))
(defvar next-lisp-object-id 0)

(defun ensure-lisp-object-id (x)
  (or (gethash x lisp-object-to-id-table)
      (let ((id next-lisp-object-id))
        ;; (message "assinging lisp object %s = %S" id x)
        (setq next-lisp-object-id
              (1+ next-lisp-object-id))
        (puthash x id lisp-object-to-id-table)
        (puthash id x lisp-id-to-object-table)
        id)))

(defun scheme-value-ref-id (x)
  (and (vectorp x)
       (= 3 (length x))
       (eq scheme-value-ref-tag (aref x 0))
       (aref x 1)))

(defalias 'scheme-value-ref-p 'scheme-value-ref-id)

(defun ensure-scheme-value-ref (scheme-obj-id)
  (or (gethash scheme-obj-id scheme-id-to-value-ref-table)
      (let ((value-ref (vector scheme-value-ref-tag
                               scheme-obj-id
                               (make-finalizer
                                (lambda ()
                                  (forget-scheme-object
                                   scheme-obj-id))))))
        (puthash scheme-obj-id value-ref scheme-id-to-value-ref-table)
        value-ref)))

(defun forget-lisp-object (id)
  "Allow a Lisp object to be garbage collected."
  (let ((obj (gethash id lisp-id-to-object-table)))
    ;; (message "forgetting lisp object %s" id)
    (remhash id lisp-id-to-object-table)
    (remhash obj lisp-object-to-id-table)))

(defun scheme-internal-reset ()
  (clrhash lisp-id-to-object-table)
  (clrhash lisp-object-to-id-table)
  (clrhash scheme-id-to-value-ref-table))

(provide 'scheme-internal)
