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

(defconst company-proto-option
  '("option"))

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

(defconst company-proto-file-options
  '("java_package"
    "java_outer_classname"
    "java_multiple_files"
    "java_generate_equals_and_hash"
    "java_string_check_utf8"
    "optimize_for"
    "go_package"
    "cc_generic_services"
    "java_generic_services"
    "py_generic_services"
    "php_generic_services"
    "deprecated"
    "cc_enable_arenas"
    "objc_class_prefix"
    "csharp_namespace"
    "swift_prefix"
    "php_class_prefix"
    "php_namespace"
    "php_metadata_namespace"
    "ruby_package"
    "uninterpreted_option"))

(defconst company-proto-message-options
  '("message_set_wire_format"
    "no_standard_descriptor_accessor"
    "deprecated"
    "map_entry"
    "uninterpreted_option"))

(defconst company-proto-field-options
  '("ctype"
    "packed"
    "jstype"
    "lazy"
    "deprecated"
    "weak"
    "uninterpreted_option"))

(defconst company-proto-enum-options
  '("allow_alias"
    "deprecated"
    "uninterpreted_option"))

(defconst company-proto-service-options
  '("deprecated"
    "uninterpreted_option"))


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


(defun company-proto--current-token-type (prefix)
  "Prase the token type in current position."
  (let* ((k (when (save-excursion (re-search-backward (rx (or "{" "}" "(" "<" ">" "[")) nil t 1))
	      (match-string 0)))
	 (scope (when (save-excursion (and (string-equal k "{")
					  (re-search-backward (rx word-start (or "message" "enum" "service") word-end) nil t 1)))
		  (match-string 0)))
	 (option? (looking-back (rx-to-string `(: bow "option" eow (* whitespace) ,prefix)) 1)))
    (cond
     ((string-equal k "<") 'proto-type)
     ((or (not k) (string-equal k "}")) (if option? 'proto-file-option 'proto-top-level))
     ((string-equal k "(") 'proto-message)
     ((string-equal k "[") (when (looking-back (rx-to-string `(: (or "[" ",") (* whitespace) ,prefix)) 1) 'proto-field-option))
     ((string-equal scope "service") (if option? 'proto-service-option 'proto-api-define))
     ((string-equal scope "message") (if option? 'proto-message-option 'proto-field-rule-or-type))
     ((string-equal scope "enum") (when option? 'proto-enum-option)))))

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
   ((company-proto--first-symbol? prefix) (append company-proto-option company-proto-rpc nil))
   ((looking-back (rx-to-string `(: ")" (* whitespace) ,prefix)) 1) company-proto-returns)))

(defun company-proto--candidates (prefix)
  "Get comapny condationtes start with PREFIX for protobuf."
  (let ((token-type (company-proto--current-token-type prefix)))
    (cl-remove-if-not
     (lambda (c) (string-prefix-p prefix c))
     (cl-case token-type
       ('proto-return company-proto-returns)
       ('proto-type (company-proto--types))
       ('proto-top-level (when (company-proto--first-symbol? prefix) (append company-proto-option company-proto-top-level-keyword nil)))
       ('proto-message (company-proto--message-types))
       ('proto-api-define (company-proto--api-candidates prefix))
       ('proto-field-rule-or-type (if (company-proto--first-symbol? prefix)
				      (append company-proto-option company-proto-field-rule (company-proto--types) nil)
				    (company-proto--types)))
       ('proto-file-option company-proto-file-options)
       ('proto-field-option company-proto-field-options)
       ('proto-message-option company-proto-message-options)
       ('proto-service-option company-proto-service-options)
       ('proto-enum-option company-proto-enum-options)))))


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
