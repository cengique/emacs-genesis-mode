;;; genesis-mode.el -- Major mode for editing GENESIS neuron simulator script files.
;;; Modified from: wpdl-mode-el and from DerivedMode example in Emacs Wiki.

;; Authors: Cengiz Gunay <cengique@users.sf.net> and 
;; 	    Hugo Cornelis <hugo.cornelis@gmail.com>
;; WPDL-Mode Author: Scott Andrew Borton <scott@pp.htv.fi>
;; Created: 30 May 2014
;; Keywords: Genesis major-mode

;; Copyright (C) 2014 Cengiz Gunay <cengique@users.sf.net>
;; Copyright (C) 2005-2006 Hugo Cornelis
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

;; TODO:
;; - test in Xemacs
;; - add installation instructions, updated README.md

;; are these necessary?
(require 'custom)
(require 'cc-vars)
(require 'cc-mode)

;;; Code:
(defvar genesis-mode-hook nil)
(defvar genesis-mode-map
  (let ((genesis-mode-map (make-keymap)))
    (define-key genesis-mode-map "\C-j" 'newline-and-indent)
    genesis-mode-map)
  "Keymap for GENESIS major mode")

;; recognise .g files as genesis files
(add-to-list 'auto-mode-alist '("\\.g\\'" . genesis-mode))

;; recognise .p files as c++ files
(add-to-list 'auto-mode-alist '("\\.p$" . c++-mode))


(defvar genesis-keywords-functions
  '("if" "else" "elif" "for" "foreach" "end" "function" "call" "while"
    "return" "continue" "break" "addglobal" "getglobal" "listglobals" "setglobal"
    "abort" "abs" "acos" "addaction" "addalias" "addclass" "addescape"
    "addfield" "addforwmsg" "addglobal" "addmsg" "addmsgdef" "addobject"
    "addtask" "argc" "arglist" "argv" "asciidata" "asin" "atan" "call"
    "callfunc" "cd" "ce" "cellsheet" "check" "chr" "clearerrors"
    "closefile" "convert" "copy" "cos" "countchar" "countelementlist"
    "cpu" "create" "createmap" "debug" "delete" "deleteaction" "deleteall"
    "deleteclass" "deletefield" "deleteforwmsg" "deletemsg" "deletemsgdef"
    "deletetasks" "disable" "duplicatetable" "echo" "egg" "el" "enable"
    "enddump" "eof" "exists" "exit" "exp" "extern" "file2tab"
    "fileconnect" "findchar" "findsolvefield" "floatformat" "flushfile"
    "gaussian" "gen2spk" "genesis" "getarg" "getclock" "getdate"
    "getdefault" "getelementlist" "getenv" "getfield" "getfieldnames"
    "getglobal" "getmsg" "getparamGA" "getpath" "getsolvechildname"
    "getsolvecompname" "getstat" "getsyncount" "getsyndest" "getsynindex"
    "getsynsrc" "h" "help" "initdump" "initparamBF" "initparamCG"
    "initparamGA" "initparamSA" "initparamSS" "input" "isa" "le"
    "listcommands" "listescape" "listfiles" "listglobals" "listobjects"
    "loadtab" "log" "logfile" "max" "maxerrors" "maxwarnings" "min" "move"
    "msgsubstitute" "notes" "objsubstitute" "openfile" "planarconnect"
    "planardelay" "planardelay2" "planarweight" "planarweight2" "plane"
    "pope" "position" "pow" "printargs" "printenv" "pushe" "putevent"
    "pwe" "quit" "rand" "randcoord" "randseed" "readcell" "readfile"
    "reclaim" "relposition" "resched" "reset" "resetsynchanbuffers"
    "restore" "rotcoord" "round" "save" "scaletabchan" "setclock"
    "setdefault" "setenv" "setfield" "setfieldprot" "setglobal"
    "setmethod" "setparamGA" "setpostscript" "setprompt" "setrand"
    "setrandfield" "setsearch" "setupNaCa" "setupalpha" "setupgate"
    "setupghk" "setuptau" "sh" "shapematch" "showclocks" "showcommand"
    "showfield" "showmsg" "showobject" "showsched" "showstat" "silent"
    "simdump" "simobjdump" "simundump" "sin" "spkcmp" "sqrt" "stack"
    "step" "stop" "strcat" "strcmp" "strlen" "strncmp" "strsub"
    "substituteinfo" "substring" "swapdump" "syndelay" "tab2file" "tan"
    "trunc" "tweakalpha" "tweaktau" "useclock" "version" "volumeconnect"
    "volumedelay" "volumedelay2" "volumeweight" "volumeweight2" "where"
    "writecell" "writefile" "xcolorscale" "xgetstat" "xps" "xsimplot"
    "include" "pixflags" "xflushevents" "xhide" "xinit" "xlower" "xmap"
    "xpixflags" "xraise" "xshow" "xshowontop" "xtextload" "xupdate")
    "Keyword and function highlighting expressions for GENESIS mode.")

(defvar genesis-types
    '("str" "int" "float")
    "Data type highlighting list for GENESIS mode.")

(defvar genesis-objects
  '("Ca_concen" "Kpores" "Mg_block" "Napores" "PID" "RC" "asc_file"
    "autocorr" "calculator" "compartment" "concchan" "concpool"
    "crosscorr" "ddsyn" "dif2buffer" "difbuffer" "diffamp" "difshell"
    "disk_in" "disk_out" "diskio" "efield" "enz" "event_tofile"
    "facsynchan" "fixbuffer" "freq_monitor" "funcgen" "fura2" "ghk"
    "hebbsynchan" "hh_channel" "hillpump" "hsolve" "interspike" "leakage"
    "metadata" "mmpump" "nernst" "neutral" "paramtableBF" "paramtableCG"
    "paramtableGA" "paramtableSA" "paramtableSS" "peristim" "playback"
    "pool" "pulsegen" "randomspike" "reac" "receptor" "receptor2"
    "script_out" "sigmoid" "spikegen" "spikehistory" "symcompartment"
    "synchan" "synchan2" "tab2Dchannel" "tabchannel" "tabcurrent"
    "tabgate" "table" "table2D" "taupump" "timetable" "variable"
    "vdep_channel" "xbutton" "xcell" "xcoredraw" "xdialog" "xdraw"
    "xdumbdraw" "xfastplot" "xform" "xgif" "xgraph" "ximage" "xlabel"
    "xpix" "xplot" "xshape" "xsphere" "xtext" "xtoggle" "xtree" "xvar"
    "xview")
    "Object highlighting expressions for GENESIS mode.")

(defvar genesis-tab-width 2 "Width of a tab for GENESIS mode")

;; CG: This is the main function I needed from WPDL-Mode
(defun genesis-indent-line ()
  "Indent current line as GENESIS code."
  (interactive)
  (beginning-of-line)
  (setq check-is-cont #'(lambda () (looking-at "^.*\\\\[ \t]*\\($\\|//\\)"))) ; line ends with '\'?
  (if (bobp)
      (indent-line-to 0)	   ; First line is always non-indented
    (let ((not-indented t) cur-indent 
	  ;; TODO: make new variable that tracks a cont-block during reverse parsing
	  (is-cont (or (funcall check-is-cont) 	; either this or 
		       (save-excursion 
			 (forward-line -1) ; previous line
			 (funcall check-is-cont))))) ; is a continuation
      ;; decrease indentation if..
      (if (or (looking-at "^[ \t]*\\(end\\|pope\\|else\\)") ; If the line we are looking at is the end of a block
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
	    ;; This hint indicates that we need to align with the end token
	    (if (looking-at "^[ \t]*\\(end\\|pope\\)")
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      ;; This hint indicates that we need to indent an extra level
	      (if (or (looking-at "^[ \t]*\\(if\\|else\\|function\\|foreach\\|for\\|pushe\\)")
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
     ( ,(regexp-opt genesis-keywords-functions 'words) . font-lock-builtin-face)
     ( ,(regexp-opt genesis-types 'words) . font-lock-type-face) 
     ( ,(regexp-opt genesis-objects 'words) . font-lock-constant-face) 
     ;; there is also font-lock-constant-face
     )))

;;; configure function menu for genesis mode
;;; Hugo Cornelis <hugo.cornelis@gmail.com>
(if (featurep 'xemacs)
    (defvar fume-function-name-regexp-genesis ; For Xemacs, use func-menu
      "^\\(function\\|procedure\\)\\([ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\|[ \t]+\\)$"
      "Expression to get genesis function Names")
  (setq fume-function-name-regexp-genesis ; For GNU Emacs, use Imenu
    '(("Function" "^function\\([ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\|[ \t]+\\)" 1))
    ))


;;; Specialised routine to get the next genesis function in the buffer
;;; Hugo Cornelis <hugo@bbf.uia.ac.be>
(defun fume-find-next-genesis-function-name (buffer)
  "Searches for the next genesis function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp-genesis nil t)
      (let ((char (progn
                    (backward-up-list 1)
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (skip-chars-forward "[ \t\n]")
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
	;; should still be recoded for genesis function prototypes.
	;; then I have to test on '^[ \t]*extern' ?
        (if (eq char ?\;)
            (fume-find-next-c-function-name buffer)
          (let (beg
                name)
            ;; Get the function name and position
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (setq name (buffer-substring beg (point)))
            ;; ghastly crock for DEFUN declarations
            (cond ((string-match "^DEFUN\\s-*" name)
                   (forward-word 1)
                   (forward-word -1)
                   (setq beg (point))
                   (cond ((re-search-forward "\"," nil t)
                          (re-search-backward "\"," nil t)
                          (setq name
                                (format "%s %s"
                                        name
                                        (buffer-substring beg (point))))))))
            ;; kludge to avoid `void' etc in menu
            (if (string-match
                "\\`\\(void\\|if\\|else if\\|else\\|switch\\|for\\|while\\)\\'"
		name)
                (fume-find-next-c-function-name buffer)
              (cons name beg)))))))

(add-hook 'genesis-mode-hook
	  (lambda ()
	    (if (featurep 'xemacs)
		(progn
		  (require 'func-menu)
		  ;; add function parser for genesis mode
		  (setq fume-function-name-regexp-alist
			(append '((genesis-mode . fume-function-name-regexp-genesis))
				fume-function-name-regexp-alist))
		  (setq fume-find-function-name-method-alist
			(append '((genesis-mode . fume-find-next-genesis-function-name))
				fume-find-function-name-method-alist)))
	      (progn			; In GNU Emacs
		(setq imenu-generic-expression fume-function-name-regexp-genesis)
		(imenu-add-menubar-index)))))

(easy-menu-define c-genesis-menu genesis-mode-map "Genesis Mode Commands"
		  (c-mode-menu "Genesis"))

;; CG: Use DerivedMode
(define-derived-mode genesis-mode fundamental-mode "GENESIS script"
    "GENESIS major mode is for editing GENESIS neuron simulator script files.

The hook variable `genesis-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.

Key bindings:
\\{genesis-mode-map}"

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



