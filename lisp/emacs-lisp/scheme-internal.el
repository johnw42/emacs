;;; scheme-internal.el --- Summary -*- lexical-binding: t -*-
;;; Commentary
;;; Code:

(message "**\n** In scheme-internal.el\n**")
(funcall
 #[0 "\300\301!\210\300\302\3031 \3040\202 \210\300\305!\"\210\300\306!\210\300\307\31011 \300\311!\210\312\313\314\"\210\300\305!\210\3040\2024 \211\262\"\210\300\315!\207" [message "Before cc" "Non-error cc returned %S" (error) 42 "Not reached!" "Between ccs" "Error cc returned %S" (error) "Generating signal" signal error foo "after ccs"] 5]
 )

'(lambda ()
  (message "Before cc")
  
  (message "Non-error cc returned %S"
           (condition-case nil
               (progn
                 (+ 41 1))
             (error (message "Not reached!"))))

  (message "Between ccs")

  (message "Error cc returned %S"
           (condition-case x
               (progn
                 (message "Generating signal")
                 (signal 'error 'foo)
                 (message "Not reached!")
                 (+ 1 41))
             (error x)))

  (message "after ccs"))

(message "End of scheme-internal.el")

(provide 'scheme-internal)
