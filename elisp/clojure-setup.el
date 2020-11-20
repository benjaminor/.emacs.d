;;; clojure-setup.el --- Setup for clojure development

;;; Commentary:
;;

;;; Code:


(use-package cider
  :defer)

(use-package helm-cider
  :after helm cider)

(provide 'clojure-setup)

;;; clojure-setup.el ends here
