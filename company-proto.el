;;; company-proto.el --- companybackend for protobuf -*- lexical-binding: t; -*-

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Usage:
;;
;; You need to add following config to your init file:
;;
;; (add-hook 'protobuf-mode-hook
;;           (lambda () (setq-local company-backends
;;                           (cl-adjoin '(company-proto) company-backends))))
;;

;;; Code:

(require 'cl-lib)
(require 'company)

(defconst company-proto-top-level-keyword
  '("syntax"
    "import"
    "package"
    "message"
    "enum"
    "service"))

(defconst company-proto-field-rule
  '("optional"
    "repeated"
    "map"))

(defconst company-proto-build-in-type
  '("double"
    "float"
    "int32"
    "int64"
    "uint32"
    "uint64"
    "sint32"
    "sint64"
    "fixed32"
    "fixed64"
    "sfixed32"
    "sfixed64"
    "bool"
    "string"
    "bytes"))

(defconst company-proto-rpc
  '("rpc"))

(defconst company-proto-returns
  '("resturns"))

(defun company-proto--types-from-imenu (type)
  "Get from imenu all match TYPE."
  (mapcar #'car (cdr (assoc type imenu--index-alist))))

(defun company-proto--message-types ()
  "Get proto `Message' type from imenu."
  (company-proto--types-from-imenu "Message"))

(defun company-proto--types ()
  "Get all type using imenu--index-alist."
  (let ((custom-types (company-proto--message-types))
	(custom-enum (company-proto--types-from-imenu "Enum")))
    (append custom-types custom-enum company-proto-build-in-type nil)))


(defun company-proto--current-token-type ()
  "Prase the token type in current position."
  (save-excursion
    (let* ((k (when (re-search-backward (rx (or "{" "}" "(" "<" ">")) nil t 1)
		(match-string 0)))
	   (scope (when (and (string-equal k "{")
			     (re-search-backward (rx word-start (or "message" "enum" "service") word-end) nil t 1))
		    (match-string 0))))
      (cond
       ((string-equal k "<") 'proto-type)
       ((or (not k) (string-equal k "}")) 'proto-top-level)
       ((string-equal k "(") 'proto-message)
       ((string-equal scope "service") 'proto-api-define)
       ((string-equal scope "message") 'proto-field-rule-or-type)))))

(defun company-proto--first-symbol? (prefix)
  "Retrun non-nil if PREFIX is the first word of line."
  (looking-back (rx-to-string `(: bol (* whitespace) ,prefix)) 1))


(defun company-proto--prefix ()
  "Check if is company prefix."
  (and (eq major-mode 'protobuf-mode)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-proto--api-candidates (prefix)
  "`rpc' or `returns' which match PREFIX."
  (cond
   ((company-proto--first-symbol? prefix) company-proto-rpc)
   ((looking-back (rx-to-string `(: ")" (* whitespace) ,prefix)) 1) company-proto-returns)))

(defun company-proto--candidates (prefix)
  "Get comapny condationtes start with PREFIX for protobuf."
  (let ((token-type (company-proto--current-token-type)))
    (cl-remove-if-not
     (lambda (c) (string-prefix-p prefix c))
     (cl-case token-type
       ('proto-return company-proto-returns)
       ('proto-type (company-proto--types))
       ('proto-top-level (when (company-proto--first-symbol? prefix) company-proto-top-level-keyword))
       ('proto-message (company-proto--message-types))
       ('proto-api-define (company-proto--api-candidates prefix))
       ('proto-field-rule-or-type (if (company-proto--first-symbol? prefix)
				      (append company-proto-field-rule (company-proto--types) nil)
				    (company-proto--types)))))))


;;;###autoload
(defun company-proto (command &optional arg &rest ignored)
  "Companybackend for protobuf."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-proto))
    (prefix (company-proto--prefix))
    (candidates (company-proto--candidates arg))))




(provide 'company-proto)

;;; company-proto.el ends here
