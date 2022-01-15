;;; salo-mode.el --- Major mode for editing Salo code. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021, SEMC
;;
;;; Code:

(setq salo-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("break" "default" "do" "else" "for" "if" "return" "state" "while"))
            (x-types '("Int" "String"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode salo-mode idris-mode "Salo mode"
  "Major mode for editing Salo code."
  (setq font-lock-defaults '((salo-font-lock-keywords))))

(provide 'salo-mode)
;;; salo-mode.el ends here
