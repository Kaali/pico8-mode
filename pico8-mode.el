;;; pico8-mode.el --- a major-mode for editing Pico8 p8 files -*- lexical-binding: t -*-
;; Author: Väinö Järvelä <vaino@jarve.la>
;; URL: https://github.com/kaali/pico8-mode
;; Version: 20180215
;; Package-Requires: ((lua-mode "20180104"))
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(require 'seq)
(require 'lua-mode)
(require 'rx)
(require 'cl-lib)
(require 'xref)

;; TODO: Clean up and refactor
;; TODO: Highlight current argument with eldoc

(defgroup pico8 nil
  "pico8 major mode"
  :prefix "pico8-"
  :group 'languages)

;; TODO: Maybe rebuild documentation when setting this custom-var?
(defcustom pico8-documentation-file ""
  "Full path to pico8 manual.
Enables documentation annotations with eldoc and company"
  :type 'file
  :group 'pico8)

(defcustom pico8-dim-non-code-sections t
  "If enabled, then dim all sections that are not Lua code"
  :type 'boolean
  :group 'pico8)

(defface pico8--non-lua-overlay
  '((((background light)) :foreground "grey90")
    (((background dark)) :foreground "grey10"))
  "Face for non-Lua sections of the p8 file"
  :group 'pico8)

(defvar pico8--lua-block-start nil "")
(make-variable-buffer-local 'pico8--lua-block-start)

(defvar pico8--lua-block-end nil "")
(make-variable-buffer-local 'pico8--lua-block-end)

(defvar pico8--lua-block-end-tag nil "")
(make-variable-buffer-local 'pico8--lua-block-end-tag)

(cl-defstruct (pico8-symbol (:constructor pico8-symbol--create))
  "A Lua symbol on pico8 mode."
  symbol line column signature location doc doc-position arguments)

(defun pico8--make-builtin (symbol signature &optional doc)
  "Constructs a built-in plist with symbol, signature and documentation."
  (pico8-symbol--create
   :symbol symbol
   :signature (concat "function " symbol "(" signature ")")
   :arguments signature
   :doc doc))

(defconst pico8--builtins-list
  '(("clip" "[x y w h]")
    ("pget" "x y")
    ("pset" "x y c")
    ("sget" "x y")
    ("sset" "x y c")
    ("fget" "n [f]")
    ("fset" "n [f] v")
    ("print" "str [x y [col]]")
    ("cursor" "x y")
    ("color" "col")
    ("cls" "[col]")
    ("camera" "[x y]")
    ("circ" "x y r [col]")
    ("circfill" "x y r [col]")
    ("line" "x0 y0 x1 y1 [col]")
    ("rect" "x0 y0 x1 y1 [col]")
    ("rectfill" "x0 y0 x1 y1 [col]")
    ("pal" "c0 c1 [p]")
    ("palt" "c t")
    ("spr" "n x y [w h] [flip_x] [flip_y]")
    ("sspr" "sx sy sw sh dx dy [dw dh] [flip_x] [flip_y]")
    ("fillp" "p")
    ("add" "t v")
    ("del" "t v")
    ("all" "t")
    ("foreach" "t f")
    ("pairs" "t")
    ("btn" "[i [p]]")
    ("btnp" "[i [p]]")
    ("sfx" "n [channel [offset [length]]]")
    ("music" "[n [fade_len [channel_mask]]]")
    ("mget" "x y")
    ("mset" "x y v")
    ("map" "cel_x cel_y sx sy cel_w cel_h [layer]")
    ("peek" "addr")
    ("poke" "addr val")
    ("peek4" "addr")
    ("poke4" "addr val")
    ("memcpy" "dest_addr source_addr len")
    ("reload" "dest_addr source_addr len [filename]")
    ("cstore" "dest_addr source_addr len [filename]")
    ("memset" "dest_addr val len")
    ("max" "x y" "Returns maximum value of x and y")
    ("min" "x y" "Returns minimum value of x and y")
    ("mid" "x y z" "Returns middle value of x, y and z")
    ("flr" "x" "Floor x")
    ("ceil" "x" "Ceil x")
    ("cos" "x" "Cosine of x")
    ("sin" "x" "Sine of x")
    ("atan2" "dx dy")
    ("sqrt" "x")
    ("abs" "x")
    ("rnd" "x")
    ("srand" "x")
    ("band" "x y" "Boolean and")
    ("bor" "x y" "Boolean or")
    ("bxor" "x y" "Boolean xor")
    ("bnot" "x" "Boolean not")
    ("rotl" "x y" "Rotate right")
    ("rotr" "x y" "Ritate left")
    ("shl" "x n" "Shift left")
    ("shr" "x n" "Shift right")
    ("lshr" "x n" "Logical shift right")
    ("menuitem" "Index [label callback]")
    ("sub" "s a b")
    ("type" "val")
    ("tostr" "val [hex]")
    ("tonum" "val")
    ("cartdata" "id")
    ("dget" "index")
    ("dset" "index value")
    ("setmetatable" "t, m" "Get metatable")
    ("getmetatable" "t" "Set metatable")
    ("cocreate" "f")
    ("coresume" "c [p0 p1 ..]")
    ("costatus" "c")
    ("yield" "" "Yield coroutine execution")))

(defconst pico8--builtins
  (seq-map (lambda (x) (apply #'pico8--make-builtin x)) pico8--builtins-list))

(defconst pico8--builtins-symbols
  (seq-map (lambda (s) (plist-get s :symbol)) pico8--builtins))

;; based on lua-mode.el
(defconst pico8--builtins-regex
  (concat
   "\\(?:^\\|[^:. \t]\\|[.][.]\\)[ \t]*\\(?:"
   (mapconcat (lambda (x)
                (concat "\\(?1:\\_<" x "\\_>\\)"))
              pico8--builtins-symbols "\\|")
   "\\)"))

(defun pico8--has-documentation-p ()
  "Is pico8-documentation-file set and does the file exits?"
  (and (> (length pico8-documentation-file) 0)
       (file-exists-p pico8-documentation-file)))

(defun pico8--find-documentation (symbol arguments)
  "Find a part of documentation for `symbol' with `arguments'.
Does a dumb lookup which can break if the file format changes.

Returns string and location in the documentation file."
  (if (pico8--has-documentation-p)
      (with-temp-buffer
        (insert-file-contents pico8-documentation-file)
        (save-excursion
          (goto-char 1)
          (when (search-forward-regexp (concat "\t" (regexp-quote symbol) " +" (regexp-quote arguments)) nil t)
            (let ((fun-start (point)))
              (when (search-forward-regexp "\t\t[A-Za-z]" nil t)
                (let ((start (1- (point)))
                      (end (line-end-position)))
                  (cons (buffer-substring-no-properties start end) fun-start)))))))
    (error "Define pico8-documentation-file to use documentation features")))

;;;###autoload
(defun pico8-build-documentation ()
  "Rebuild pico8 function documentation.
Requires `pico8-documentation-file' to be set."
  (interactive)
  (seq-do (lambda (s)
            (if (pico8-symbol-doc s)
                s
              (let* ((symbol (pico8-symbol-symbol s))
                     (arguments (pico8-symbol-arguments s))
                     (doc (pico8--find-documentation symbol arguments)))
                (setf (pico8-symbol-doc s) (car doc))
                (setf (pico8-symbol-doc-position s) (cdr doc)))))
          pico8--builtins)
  nil)

(defun pico8--modified-lua-font-lock ()
  "Return a modified lua-font-lock.
Where lua built-ins are removed and replaced with pico8 builtins."
  (let ((without-builtins
        (seq-filter
         (lambda (x) (not (string-match ".*loadstring.*" (car x))))
         lua-font-lock-keywords)))
    (append `((,pico8--builtins-regex . font-lock-builtin-face))
            without-builtins)))

;; Adapted from lua-mode.el (lua-send-defun)
(defun pico8--lua-function-bounds ()
  "Return Lua function bounds, or nil if not in a function."
  (save-excursion
    (let ((pos (point))
          (start (if (save-match-data (looking-at "^function[ \t]"))
                     (point)
                   (lua-beginning-of-proc)
                   (point)))
          (end (progn (lua-end-of-proc) (point))))
      (if (and (>= pos start) (< pos end))
          (cons start end)
        nil))))

(defun pico8--match-column (SUBEXPR)
  "Return a position of column at start of text matched by last search."
  (- (match-beginning SUBEXPR) (line-beginning-position)))

;; this is copied from lua-mode.el to match its functionality
(defconst pico8--lua-function-regex
  (lua-rx (or bol ";") ws (opt (seq (symbol "local") ws)) lua-funcheader))

(defconst pico8--lua-variable-regex
  (lua-rx (or bol ";") ws (opt (seq (group-n 1 (symbol "local")) ws)) (group-n 2 lua-funcname) ws "="))

(defvar pico8--lua-argument-regex
  (lua-rx (seq (group-n 1 lua-name))))

(defun pico8--find-all-functions ()
  "Find and return all lua functions from the current buffer."
  (let ((symbols))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (or pico8--lua-block-start 1))
        (while (search-forward-regexp pico8--lua-function-regex
                                      pico8--lua-block-end t 1)
          (let ((symbol (pico8-symbol--create
                         :symbol (match-string-no-properties 1)
                         :line (line-number-at-pos)
                         :column (pico8--match-column 1)
                         :signature (thing-at-point 'line t)
                         :location (match-beginning 1))))
            ;; Augment with arguments
            (save-excursion
              (goto-char (match-beginning 0))
              (when (search-forward "(" (line-end-position) t 1)
                (let ((arguments '()))
                  (while (search-forward-regexp pico8--lua-argument-regex
                                                (line-end-position) t 1)
                    (push (match-string-no-properties 1) arguments))
                  (setf (pico8-symbol-arguments symbol) (string-join (reverse arguments) " ")))))
            (push symbol symbols)))))
    symbols))

(defun pico8--find-variables ()
  "Find and return lua variables.
Do some scoping with local variables."
  (let ((variables))
    (save-excursion
      (save-restriction
        (widen)
        (let ((fn-bounds (pico8--lua-function-bounds)))
          (goto-char (or pico8--lua-block-start 1))
          (while (search-forward-regexp pico8--lua-variable-regex
                                        pico8--lua-block-end t 1)
            (let ((is-local (match-string-no-properties 1))
                  (variable (match-string-no-properties 2))
                  (line-num (line-number-at-pos)))
              (when (or (not is-local)
                        (not fn-bounds)
                        (<= (car fn-bounds) (point) (cdr fn-bounds)))
                (push (pico8-symbol--create
                       :symbol variable
                       :line (line-number-at-pos)
                       :column (pico8--match-column 2)
                       :location (match-beginning 2))
                      variables)))))))
    variables))

;; TODO: Reduce code duplication in this pattern
(defun pico8--find-current-function-arguments ()
  "Find and return lua arguments of the current function."
  (let ((arguments))
    (save-excursion
      (save-restriction
        (widen)
        (when-let* ((fn-bounds (pico8--lua-function-bounds)))
          (goto-char (car fn-bounds))
          (when (search-forward "(" (cdr fn-bounds) t 1)
            (while (search-forward-regexp pico8--lua-argument-regex
                                          (line-end-position) t 1)
              (push (pico8-symbol--create
                     :symbol (match-string-no-properties 1)
                     :line (line-number-at-pos)
                     :column (pico8--match-column 1)
                     :location (match-beginning 1))
                    arguments))))))
    arguments))

(defun pico8--filter-symbol (symbol &optional symbols)
  "Find all symbols named symbol."
  (seq-filter (lambda (s) (string= symbol (pico8-symbol-symbol s)))
              (or symbols (pico8--completion-symbols-without-builtins))))

(defun pico8--find-symbol (symbol &optional symbols)
  "Find a single symbol named symbol.
Returns the first match in case of multiple matches."
  (seq-find (lambda (s) (string= symbol (pico8-symbol-symbol s)))
            (or symbols (pico8--find-all-functions))))

(defun pico8--make-xref-of-symbol (symbol)
  "Make a xref of a symbol."
  (xref-make (pico8-symbol-symbol symbol)
             (xref-make-file-location buffer-file-name
                                      (pico8-symbol-line symbol)
                                      (pico8-symbol-column symbol))))

(cl-defmethod xref-backend-identifier-at-point
  ((_backend (eql xref-pico8)))
  "pico8 xref identifier-at-point."
  (lua-funcname-at-point))

(cl-defmethod xref-backend-definitions
  ((_backend (eql xref-pico8)) symbol)
  "pico8 xref definitions."
  (seq-map #'pico8--make-xref-of-symbol
           (pico8--filter-symbol symbol)))

(cl-defmethod xref-backend-apropos
  ((_backend (eql xref-pico8)) symbol)
  "pico8 xref apropos."
  (seq-map #'pico8--make-xref-of-symbol
           (pico8--filter-symbol symbol)))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql xref-pico8)))
  "pico8 xref identifier completion table."
  (pico8--completion-symbols-without-builtins))

(defun xref-pico8-backend ()
  "Return pico8 xref backend name."
  'xref-pico8)

(defun pico8--completion-symbols ()
  "Return a list of all completion symbols.
Including Lua and pico8 built-ins."
  (append pico8--builtins
          (pico8--find-all-functions)
          (pico8--find-variables)
          (pico8--find-current-function-arguments)))

(defun pico8--completion-symbols-without-builtins ()
  "Return a list of all completion symbols.
Including Lua and pico8 built-ins."
  (append (pico8--find-all-functions)
          (pico8--find-variables)
          (pico8--find-current-function-arguments)))

;; based on lua-funcname-at-point code from lua-mode.el
(defun pico8--lua-funcname-bounds-at-point ()
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (bounds-of-thing-at-point 'symbol)))

(defun pico8--company-doc-buffer (symbol)
  (cons
   (with-current-buffer (get-buffer-create "*company-documentation*")
     (erase-buffer)
     (insert-file-contents pico8-documentation-file)
     (current-buffer))
   (pico8-symbol-doc-position symbol)))

(defun pico8--company-location (symbol)
  (cons (current-buffer)
        (pico8-symbol-location symbol)))

(defun pico8--completion-at-point-exit-function (arg status symbol)
  (when (boundp 'company-mode)
    (when-let* ((arguments (pico8-symbol-arguments symbol)))
      (let* ((split-args (split-string arguments))
             (args-template (concat "(" (string-join split-args ", ") ")")))
        (insert args-template)
        (company-template-c-like-templatify
         (concat arg args-template))))))

(defun pico8--completion-at-point ()
  (when-let* ((bounds (pico8--lua-funcname-bounds-at-point))
              (symbols (pico8--completion-symbols))
              (symbol-names (seq-map 'pico8-symbol-symbol symbols)))
    (fset 'symbol (lambda (arg) (pico8--find-symbol arg symbols)))
    (list (car bounds)
          (cdr bounds)
          symbol-names
          :exclude 'no
          :company-docsig (lambda (arg) (pico8-symbol-signature (symbol arg)))
          :annotation-function (lambda (arg) (pico8-symbol-doc (symbol arg)))
          :company-doc-buffer (lambda (arg)
                                (pico8--company-doc-buffer (pico8--find-symbol arg symbols)))
          :company-location (lambda (arg)
                              (pico8--company-location (pico8--find-symbol arg symbols)))
          :exit-function (lambda (arg status)
                           (pico8--completion-at-point-exit-function
                            arg status (pico8--find-symbol arg symbols))))))

(defun pico8--eldoc-documentation ()
  "eldoc documentation function for pico8"
  (save-excursion
    (condition-case nil
        (backward-up-list nil t)
      (error nil))
    (when-let* ((symbol (pico8--find-symbol (lua-funcname-at-point)
                                            (pico8--completion-symbols)))
                (signature (pico8-symbol-signature symbol)))
      (concat signature
              (when-let* ((doc (pico8-symbol-doc symbol)))
                (concat ": " doc))))))

(defun pico8--put-non-lua-overlay (beg end)
  "Put pico8 non-Lua overlay in region."
  (remove-overlays beg end 'face 'pico8--non-lua-overlay)
  (overlay-put (make-overlay beg end) 'face 'pico8--non-lua-overlay))

(defun pico8--scan-for-lua-block-in-region (beg end)
  "Try to find lua block in the region."
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "__\\([a-z]+\\)__" end t 1)
      (if (string= "lua" (match-string 1))
          (setq pico8--lua-block-start (match-end 0))
        (when (and (> (match-beginning 0) pico8--lua-block-start)
                   (or (not pico8--lua-block-end)
                       (string= (match-string-no-properties 1) pico8--lua-block-end-tag)
                       (< (match-beginning 0) pico8--lua-block-end)))
          (setq pico8--lua-block-end-tag (match-string-no-properties 1))
          (setq pico8--lua-block-end (match-beginning 0)))))))

(defun pico8--syntax-propertize (beg end)
  "pico8 syntax-table propertize function.
Sets an overlay on non-Lua code."
  (lua--propertize-multiline-bounds beg end)
  (pico8--scan-for-lua-block-in-region beg end)
  (when pico8-dim-non-code-sections
    (when (and pico8--lua-block-start (< beg pico8--lua-block-start))
      (pico8--put-non-lua-overlay beg (min end pico8--lua-block-start)))
    (when (and pico8--lua-block-end (> end pico8--lua-block-end))
      (pico8--put-non-lua-overlay (max beg pico8--lua-block-end) end))))

;;;###autoload
(define-derived-mode pico8-mode lua-mode "pico8"
  "pico8 major mode."
  (setq-local lua-font-lock-keywords (pico8--modified-lua-font-lock))
  (add-to-list 'xref-backend-functions #'xref-pico8-backend)
  (add-to-list 'completion-at-point-functions #'pico8--completion-at-point)
  (setq-local eldoc-documentation-function #'pico8--eldoc-documentation)
  (setq-local syntax-propertize-function #'pico8--syntax-propertize)
  (when (pico8--has-documentation-p)
    (pico8-build-documentation)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.p8\\'" . pico8-mode))

(provide 'pico8-mode)

;;; pico8-mode.el ends here
