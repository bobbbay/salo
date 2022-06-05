;;; salo-mode.el --- Major mode for editing Salo code. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021, SEMC
;;
;;; Code:

(setq salo-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("module"))
            (x-types '("Int" "String"))
            (x-constants '())

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
          (,x-keywords-regexp . 'font-lock-keyword-face))))

;;;###autoload
(define-derived-mode salo-mode fundamental-mode "Salo mode"
  "Major mode for editing Salo code."
  (setq font-lock-defaults '((salo-font-lock-keywords))))

(provide 'salo-mode)
;;; salo-mode.el ends here
