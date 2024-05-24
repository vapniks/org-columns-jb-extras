;;; org-columns-jb-extras.el --- Extra functions & macros for working with org-columns

;; Filename: org-columns-jb-extras.el
;; Description: Extra functions & macros for working with org-columns
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2024, Joe Bloggs, all rites reversed.
;; Created: 2024-05-24 00:22:47
;; Version: 0.1
;; Last-Updated: 2024-05-24 00:22:47
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/org-columns-jb-extras
;; Keywords: tools
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ((org "9.4.6"))
;;
;; Features that might be required by this library:
;;
;; org 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; This library contains some extra functions for org-columns, and shadows some existing ones;
;; `org-columns--compute-spec' & `org-columns-check-computed'.
;; The new functions allow column views that treat values of leafs of org trees differently to
;; values at branches when summarizing in a column view. 
;; The functions `org-columns--summary-min-decision-tree' & `org-columns--summary-max-decision-tree'
;; can be used to create decision trees that calculate probabilities.
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-columns-leaf-types'
;;    Like `org-columns-summary-types' (which see), but for leaves of the tree only.
;;    default = nil

;;
;; All of the above can be customized by:
;;      M-x customize-group RET org-columns-jb-extras RET
;;

;;; Installation:
;;
;; Put org-columns-jb-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'org-columns-jb-extras)

;;; History:

;;; Require
(require 'org)

;;; Code:

;; REMEMBER TODO ;;;###autoload's 

;; org-columns stuff
(defcustom org-columns-leaf-types nil
  "Like `org-columns-summary-types' (which see), but for leaves of the tree only.
      Each element is of the form (LABEL . FUNCTION) where the LABEL is matched with the
      same label used to choose the summary function.
      The FUNCTION takes the same two arguments as the functions in `org-columns-summary-types',
      but the first argument will always be nil. This means you can use the same function for both
      summaries and leaves, and test the first argument to find out if it is being called at a leaf."
  :group 'org-properties
  :type '(alist :key-type (string :tag "           Label")
		:value-type (function :tag "Leaf function")))

(defmacro org-columns-with-prop-symbols (varlist pom inherit literal-nil &rest body)
  "Create `let' form with variables bound to values of properties at current point in tree.
      Elements of VARLIST can be strings indicating the names of variables and corresponding 
      properties, or lists of 2 elements (VAR PROP) where VAR is the name of the variable, 
      and PROP is the property to bind to it. If VARLIST is nil then all properties in the current 
      tree will be bound to variables with the same names. If any of the property names in VARLIST
      are prefixed by \"_\" then the `string-to-number' function will be applied to the extracted
      values.
      The POM, INHERIT and LITERAL-NIL arguments will be passed to `org-entry-get' to obtain the
      property values, and BODY forms will be placed inside the `let' form.
      E.g: (org-columns-with-prop-symbols (\"_foo\" \"baa\" (x \"_choo\") (y \"choo\")) (point) t nil 
					  (if (equal y baa) (+ x _foo)))"
  (declare (indent 4)
	   (debug ((&rest &or stringp (symbolp stringp)) sexp sexp sexp body)))
  `(let ,(mapcar (lambda (v)
		   (if (stringp v)
		       (let ((v2 (upcase v)))
			 (if (string-match "^_" v)
			     (list (intern v)
				   `(string-to-number
				     (or (org-columns--compute-leaf
					  (org-entry-get ,pom ,(substring v2 1)
							 ,inherit ,literal-nil) t) "0")))
			   (list (intern v)
				 `(org-columns--compute-leaf
				   (org-entry-get ,pom ,v2 ,inherit ,literal-nil) t))))
		     (if (and (listp v)
			      (symbolp (car v))
			      (stringp (second v)))
			 (let ((v2 (upcase (second v))))
			   (list (car v)
				 (if (string-match "^_" v2)
				     `(string-to-number
				       (or (org-columns--compute-leaf
					    (org-entry-get ,pom ,(substring v2 1) ,inherit ,literal-nil) t)
					   "0"))
				   `(org-columns--compute-leaf
				     (org-entry-get ,pom ,v2 ,inherit ,literal-nil) t))))
		       (error "Invalid argument: %s" v))))
		 (or varlist (mapcar 'car org-columns-current-fmt-compiled)))
     ,@body))

(defmacro org-columns-summary-function (name varlist doc symb summarybody leafbody)
  "Create a summary function for `org-columns-summary-types'.
      NAME is a string containing the name of the function without the org-columns--summary- prefix.
      DOC is a documentation string, and BODY is the body of the function. 
      The list of values in the current column for children of this branch will be bound 
      to SYMB in the BODY, and VARLIST is treated in the same way as `org-columns-with-prop-symbols', 
      i.e. it should be a list of strings or (VAR PROP) pairs indicating which properties
      should be let-bound to variables.
      For example: (org-columns-summary-function \"foo\" vals (\"_col1\" \"_col2\") \"A summary function.\" (apply '+ col1 col2 vals))"
  (org-with-gensyms (fmt values)
    `(defun ,(intern (concat "org-columns--summary-" name)) (,values ,fmt)
       ,(if (stringp doc) doc
	  (error "Invalid comment string"))
       (let ((,symb ,(if (string-match "^_" (symbol-name symb))
			 `(mapcar #'string-to-number ,values)
		       values)))
	 (format (or ,fmt "%s")
		 (org-columns-with-prop-symbols ,varlist (point) t nil
		   (if ,values ,summarybody ,leafbody)))))))

(org-columns-summary-function
 "min-decision-tree" ("_prob" "_cost" "todo")
 "Compute the sum of VALUES and multiply by the :PROB property of the current headline.
      Use PRINTF to format the output."
 _vals
 (cond ((equal todo "ACTION")
	(+ _cost (apply #'+ _vals)))
       ((equal todo "STATE")
	(* _prob (+ _cost (apply #'min _vals))))
       ((equal todo "DECISION")
	(apply #'min _vals))
       (t (error "Invalid TODO state")))
 (* _prob _cost))

(org-columns-summary-function
 "max-decision-tree" ("_prob" "_cost" "todo")
 "Compute the sum of VALUES and multiply by the :PROB property of the current headline.
      Use PRINTF to format the output."
 _vals
 (cond ((equal todo "ACTION")
	(+ _cost (apply #'+ _vals)))
       ((equal todo "STATE")
	(* _prob (+ _cost (apply #'max _vals))))
       ((equal todo "DECISION")
	(apply #'max _vals))
       (t (error "Invalid TODO state")))
 (* _prob _cost))

;; This function shadows another with the same name in org-colview.el
;; It adds extra functionality for computing values at leaves.
(defun org-columns--compute-spec (spec &optional update)
  "Update tree according to SPEC.
      SPEC is a column format specification.  When optional argument
      UPDATE is non-nil, summarized values can replace existing ones in
      properties drawers."
  (let* ((lmax (if (bound-and-true-p org-inlinetask-min-level)
		   org-inlinetask-min-level
		 29)) ;; Hard-code deepest level.
	 (lvals (make-vector (1+ lmax) nil))
	 (level 0)
	 (atstart t)
	 (inminlevel lmax)
	 (last-level lmax)
	 (property (car spec))
	 (printf (nth 4 spec))
	 (summarize (org-columns--summarize (nth 3 spec)))
	 (leaffn (cdr (assoc (nth 3 spec) org-columns-leaf-types))))
    (org-with-wide-buffer
     ;; Find the region to compute.
     (goto-char org-columns-top-level-marker)
     (goto-char (condition-case nil (org-end-of-subtree t) (error (point-max))))
     ;; Walk the tree from the back and do the computations.
     (while (re-search-backward org-outline-regexp-bol org-columns-top-level-marker t)
       (unless (or (= level 0) (eq level inminlevel))
	 (setq last-level level))
       (setq level (org-reduced-level (org-outline-level)))
       (let* ((pos (match-beginning 0))
	      (value (org-entry-get nil property))
	      (value-set (org-string-nw-p value))
	      (isleaf (or atstart (>= level last-level))))
	 (setq atstart nil)
	 ;; Collect values from lower levels and inline tasks here
	 ;; and summarize them using SUMMARIZE.  Store them in text
	 ;; property `org-summaries', in alist whose key is SPEC.
	 (let* ((summary
		 (if (not isleaf)
		     (and summarize
			  (let ((values (append (and (/= last-level inminlevel)
						     (aref lvals last-level))
						(aref lvals inminlevel))))
			    (and values (funcall summarize values printf))))
		   (if leaffn (funcall leaffn nil printf)
		     (org-columns--compute-leaf value)))))
	   ;; Mark the summary value
	   (when summary
	     (let* ((summaries-alist (get-text-property pos 'org-summaries))
		    (old (assoc spec summaries-alist)))
	       (if old (setcdr old summary)
		 (push (cons spec summary) summaries-alist)
		 (org-with-silent-modifications
		  (add-text-properties pos (1+ pos) (list 'org-summaries summaries-alist)))))
	     ;; When PROPERTY exists in current node, even if empty,
	     ;; but its value doesn't match the one computed, use
	     ;; the latter instead.
	     ;;
	     ;; Ignore leading or trailing white spaces that might
	     ;; have been introduced in summary, since those are not
	     ;; significant in properties value.
	     (let ((new-value (org-trim summary)))
	       (when (and update (not isleaf) value (not (equal value new-value)))
		 (org-entry-put (point) property new-value))))
	   ;; Add current to current level accumulator.
	   (when (or summary value-set)
	     (push (substring-no-properties (or summary value))
		   (aref lvals level)))
	   ;; Clear accumulators for deeper levels.
	   (cl-loop for l from (1+ level) to lmax do (aset lvals l nil))))))))

;; This function shadows one with the same name in org-colview.el
(defun org-columns-check-computed ()
  "Throw an error if current column value is computed."
  (let* ((spec (nth (current-column) org-columns-current-fmt-compiled))
	 (val (org-entry-get (point) (car spec))))
    (if (and val (string-match "^\\(remote\\|calc\\|prop\\)(.*)$" val))
	(overlay-put (car (overlays-at (point))) 'org-columns-value val)
      (and (nth 3 spec)
	   (assoc spec (get-text-property (line-beginning-position) 'org-summaries))
	   (error "This value is computed from the entry's children")))))

(defun org-columns--compute-leaf (value &optional ret)
  "Compute a leaf VALUE if it begins with \"remote\", \"calc\" or \"prop\".
      Otherwise return nil, or VALUE if RET is non-nil."
  (cond ((null value) nil)
	((string-match "^remote(\\([^,]*\\),\\([^,]*\\))$" value)
	 (let ((vals (org-table-get-remote-range (match-string 1 value)
						 (match-string 2 value))))
	   (propertize (if (listp vals) (mapconcat 'identity vals " ") vals)
		       'face 'org-table 'fontified t)))
	((string-match "^calc(\\(.*\\))$" value)
	 (let* ((sum (match-string 1 value))
		(val (calc-eval (replace-regexp-in-string
				 "%\\([a-zA-Z0-9_-]+\\)"
				 (lambda (str)
				   (save-match-data
				     (org-entry-get nil (upcase (match-string 1 str)))))
				 sum))))
	   (propertize (if (stringp val) val (error (cadr val)))
		       'face 'org-formula 'fontified t)))
	((string-match "^prop(\\([^,]*\\),\\([^,]*\\))$" value)
	 (let ((id (match-string 1 value))
	       (prop (match-string 2 value)))
	   (save-window-excursion
	     (save-excursion
	       (if (string-match "^#" id)
		   (org-link-search id)
		 (org-id-open id))
	       (propertize (org-entry-get nil prop t)
			   'face 'org-table 'fontified t)))))
	(ret value)))


(provide 'org-columns-jb-extras)

;; (org-readme-sync)
;; (magit-push)

;;; org-columns-jb-extras.el ends here
