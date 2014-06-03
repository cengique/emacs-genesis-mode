;;; genesis-mode.el -- For editing GENESIS neuron simulator script files
;;; Modified from: wpdl-mode-el -- Major mode for editing WPDL files
;;; and from DerivedMode example in Emacs Wiki.

;; Author: Cengiz Gunay <cengique@users.sf.net>
;; WPDL-Mode Author: Scott Andrew Borton <scott@pp.htv.fi>
;; Created: 30 May 2014
;; Keywords: Genesis major-mode

;; Copyright (C) 2014 Cengiz Gunay <cengique@users.sf.net>
;; Copyright (C) 2000, 2003 Scott Andrew Borton <scott@pp.htv.fi>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;; 
;; This mode uses an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial

;;; Code:
(defvar genesis-mode-hook nil)
(defvar genesis-mode-map
  (let ((genesis-mode-map (make-keymap)))
    (define-key genesis-mode-map "\C-j" 'newline-and-indent)
    genesis-mode-map)
  "Keymap for GENESIS major mode")

(add-to-list 'auto-mode-alist '("\\.g\\'" . genesis-mode))

(defvar genesis-keywords
    '("if" "foreach" "end" "function" "echo" "setfield" "call" "while"
      "addglobal" "getglobal" "listglobals" "setglobal")
    "Minimal highlighting expressions for GENESIS mode.")

(defvar genesis-types
    '("str" "int" "float"))

(defvar genesis-tab-width 2 "Width of a tab for GENESIS mode")

;; This is the main function I needed from WPDL-Mode
(defun genesis-indent-line ()
  "Indent current line as GENESIS code."
  (interactive)
  (beginning-of-line)
  (setq check-is-cont #'(lambda () (looking-at "^.*\\\\$"))) ; line ends with '\'?
  (if (bobp)
      (indent-line-to 0)	   ; First line is always non-indented
    (let ((not-indented t) cur-indent 
	  ;; TODO: make new variable that tracks a cont-block during reverse parsing
	  (is-cont (or (funcall check-is-cont) 	; either this or 
		       (save-excursion 
			 (forward-line -1) ; previous line
			 (funcall check-is-cont))))) ; is a continuation
      ;; decrease indentation if..
      (if (or (looking-at "^[ \t]*\\(end\\|pope\\)") ; If the line we are looking at is the end of a block
	      ;; or if a line-continuation block just ended
	      (and 
	       (not is-cont)		; We're not in continuation
	       (save-excursion 		; A continuation block just ended
		 (and
		  (forward-line -1)	; Previous line is not cont,
		  (not (funcall check-is-cont))
		       (forward-line -1)	; but one before that was.
		  (funcall check-is-cont)))))
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) genesis-tab-width)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    ;; indent at the same level if..
	    (if (or 
		 ;; This hint indicates that we need to align with the end token
		 (looking-at "^[ \t]*\\(end\\|pope\\)") 
		 ;; we're in an existing (last 2 lines) continuation block
		 )	; this line is also in block 
		;; (and is-cont		; already in block, look for start
		;; (funcall check-is-cont))
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      ;; This hint indicates that we need to indent an extra level
	      (if (or (looking-at "^[ \t]*\\(if\\|function\\|foreach\\|for\\|pushe\\)")
		      ;; a new line-continuation block started
		      (and is-cont	; We were in continuation
			   (funcall check-is-cont) ; and we are still in.
			   (save-excursion 
			     (forward-line -1)	; But previous line is not cont.
			     (not (funcall check-is-cont))))) ; So this is the first line of cont.
		  (progn
		    (setq cur-indent (+ (current-indentation) genesis-tab-width)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

;; Modified from DerivedMode example:
;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close
(defvar genesis-font-lock-defaults
  `((
     ;; stuff between "
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt genesis-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt genesis-types 'words) . font-lock-type-face) 
     ;; there is also font-lock-constant-face
     )))
  
(define-derived-mode genesis-mode fundamental-mode "GENESIS script"
    "GENESIS mode is a major mode for editing GENESIS  files"

    ;; from WPDL mode example
    (use-local-map genesis-mode-map)  
  
    ;; you again used quote when you had '((genesis-hilite))
    ;; I just updated the variable to have the proper nesting (as noted above)
    ;; and use the value directly here
    (setq font-lock-defaults genesis-font-lock-defaults)
  
    ;; when there's an override, use it
    ;; otherwise it gets the default value
    (when genesis-tab-width
      (setq tab-width genesis-tab-width))
  
    ;; for comments
    ;; overriding these vars gets you what (I think) you want
    ;; they're made buffer local when you set them
    (setq comment-start "/*")
    (setq comment-end "*/")

    ;; indentation -- from WPDL mode example
    (set (make-local-variable 'indent-line-function) 'genesis-indent-line)  

    ;; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" genesis-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" genesis-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" genesis-mode-syntax-table)
  
    ;;A gnu-correct program will have some sort of hook call here.
    (run-hooks 'genesis-mode-hook)
    )
  
(provide 'genesis-mode)

;;; genesis-mode.el ends here



