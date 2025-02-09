(in-package #:org.shirakumo.alloy)

(defvar *keyboard-specs* (make-hash-table :test 'eql))

(defparameter *keyboard-short-map*
  '(("_" NIL)
    ("|" separator)
    ("⎋" :ESCAPE)
    ("$" :SECTION)
    ("-" :MINUS)
    ("=" :EQUAL)
    ("⌫" :BACKSPACE)
    ("↹" :TAB)
    ("[" :LEFT-BRACKET)
    ("]" :RIGHT-BRACKET)
    ("↵" :ENTER)
    ("⇪" :CAPS-LOCK)
    (";" :SEMICOLON)
    ("'" :APOSTROPHE)
    ("\\" :BACKSLASH)
    ("L⇧" :LEFT-SHIFT)
    ("<" :DEAD) ; FIXME: this
    ("," :COMMA)
    ("." :PERIOD)
    ("/" :SLASH)
    ("R⇧" :RIGHT-SHIFT)
    ("L⌃" :LEFT-CONTROL)
    ("L⌘" :LEFT-SUPER)
    ("L⌥" :LEFT-ALT)
    ("R⌥" :RIGHT-ALT)
    ("R⌘" :RIGHT-SUPER)
    ("R⌃" :RIGHT-CONTROL)
    ("␣" :SPACE)
    ("⇱" :HOME)
    ("⇲" :END)
    ("⌦" :DELETE)
    ("⌤" :INSERT)
    ("⇞" :PAGE-UP)
    ("⇟" :PAGE-DOWN)
    ("↑" :UP)
    ("↓" :DOWN)
    ("←" :LEFT)
    ("→" :RIGHT)
    ("⎙" :PRINT-SCREEN)
    ("⇳" :SCROLL-LOCK)
    ("⎉" :PAUSE)
    ("⇭" :NUM-LOCK)
    ("K0" :KP-0)
    ("K1" :KP-1)
    ("K2" :KP-2)
    ("K3" :KP-3)
    ("K4" :KP-4)
    ("K5" :KP-5)
    ("K6" :KP-6)
    ("K7" :KP-7)
    ("K8" :KP-8)
    ("K9" :KP-9)
    ("K/" :KP-DIVIDE)
    ("K*" :KP-MULTIPLY)
    ("K-" :KP-SUBTRACT)
    ("K+" :KP-ADD)
    ("K↵" :KP-ENTER)
    ("K." :KP-DECIMAL)))

(defun map-short-key (key)
  (let ((size 1)
        (pos (position #\_ key :start 1)))
    (cond ((null pos))
          ((= (1- (length key)) pos)
           (setf size T)
           (setf key (subseq key 0 pos)))
          (T
           (setf size (read-from-string key NIL NIL :start (1+ pos)))
           (setf key (subseq key 0 pos))))
    (values (unless (string= key "_")
              (or (second (assoc key *keyboard-short-map* :test #'string-equal))
                  (intern (string-upcase key) "KEYWORD")))
            size)))

(defun parse-keyboard-spec (spec)
  (with-input-from-string (s spec)
    (loop for line = (read-line s NIL NIL)
          for trimmed = (string-trim " " line)
          while line
          when (string/= "" trimmed)
          collect (loop for start = 0 then (position-if-not (lambda (c) (char= #\Space c)) line :start end)
                        for end = (or (position #\Space line :start start)
                                      (length line))
                        for key = (subseq line start end)
                        when (string/= "" key)
                        collect (multiple-value-list (map-short-key key))
                        until (<= (length line) end)))))

(defun keyboard-spec (name &optional (errorp T))
  (or (gethash name *keyboard-specs*)
      (when errorp (error "No such keyboard spec ~s" name))))

(defun (setf keyboard-spec) (value name)
  (setf (gethash name *keyboard-specs*)
        (etypecase value
          (string (parse-keyboard-spec value))
          (cons value))))

(defun ensure-keyboard-spec (thing)
  (etypecase thing
    (cons thing)
    (string (parse-keyboard-spec thing))
    (symbol (keyboard-spec thing))))

(defmacro define-keyboard-spec (name &rest spec)
  `(setf (keyboard-spec ',name) (progn ,@spec)))

(define-keyboard-spec :100%
  "⎋ __ F1 F2 F3 F4 __ F5 F6 F7 F8 __ F9 F10 F11 F12 _ ⎙ ⇳ ⎉ _ F13 F14 F15 F16
     $  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ _ ⌤ ⇱ ⇞ _ ⇭ K/ K* K-
   ↹_1.5 Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ _ ⌦ ⇲ ⇟ _ K7 K8 K9 K+
   ⇪_2    A  S  D  F  G  H  J  K  L  ;  ' \\ ↵_ _ _ _ _ _ K4 K5 K6 K+
   L⇧_1.5 < Z  X  C  V  B  N  M  ,  .  /    R⇧_ _ _ ↑ _ _ K1 K2 K3 K↵
   L⌃ L⌘ L⌥           ␣_             R⌥ R⌘ R⌃ _ ← ↓ → _ K0_2 K. K↵")

(define-keyboard-spec :80%
  "⎋ _ F1 F2 F3 F4 _ F5 F6 F7 F8 _ F9 F10 F11 F12_ ⎙ ⇳ ⎉
     $  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ _ ⌤ ⇱ ⇞
   ↹_1.5 Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ _ ⌦ ⇲ ⇟
   ⇪_2    A  S  D  F  G  H  J  K  L  ;  ' \\ ↵_ _ _ _ _
   L⇧_1.5 < Z  X  C  V  B  N  M  ,  .  /    R⇧_ _ _ ↑ _
   L⌃ L⌘ L⌥           ␣_             R⌥ R⌘ R⌃ _ ← ↓ →")

(define-keyboard-spec :75%
  "⎋ F1 F2 F3 F4 F5 F6 F7 F8 F9 F10 F11 F12 ⎙ ⇳ ⎉
     $  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ ⇱
   ↹_1.5 Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ ⇞
   ⇪_2    A  S  D  F  G  H  J  K  L  ;  ' \\ ↵_ ⇟
   L⇧_1.5 < Z  X  C  V  B  N  M  ,  .  /  R⇧_ ↑ ⇲
   L⌃ L⌘ L⌥          ␣_            R⌥ R⌘ R⌃ ← ↓ →")

(define-keyboard-spec :65%
  "⎋  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ $
   ↹_1.5 Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ ⌦
   ⇪_2    A  S  D  F  G  H  J  K  L  ;  ' \\ ↵_ ⇞
   L⇧_1.5 < Z  X  C  V  B  N  M  ,  .  /  R⇧_ ↑ ⇟
   L⌃ L⌘ L⌥           ␣_           R⌥ R⌘ R⌃ ← ↓ →")

(define-keyboard-spec :60%
  "⎋  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_
   ↹_1.5 Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_
   ⇪_2    A  S  D  F  G  H  J  K  L  ;  ' \\ ↵_
   L⇧_1.5 < Z  X  C  V  B  N  M  ,  .  /    R⇧_
   L⌃ L⌘ L⌥           ␣_              R⌥ R⌘ R⌃")

(define-keyboard-spec :40%
  "⎋_  Q  W  E  R  T  Y  U  I  O  P  -  =  ⌫_
   ↹_   A  S  D  F  G  H  J  K  L  ;  '  \ ↵_
   L⇧_   Z  X  C  V  B  N  M  ,  .  /     R⇧_
   L⌃ L⌘ L⌥          ␣_              R⌥ R⌘ R⌃")

(defclass virtual-key (direct-value-component button)
  ((keyboard :initarg :keyboard :accessor keyboard)))

(defmethod text ((key virtual-key))
  (or (key-text (value key) (ui key))
      (first (find (value key) *keyboard-short-map* :key #'second))
      (string (value key))))

(defmethod key-text ((key virtual-key) (ui ui))
  (case (value key)
    (:space " ")
    (T
     (let ((text (key-text (value key) ui))
           (mods (modifiers (keyboard key))))
       (when text
         (let ((caps (member :caps-lock mods))
               (shift (member :shift mods)))
           (if (and (or caps shift)
                    (not (and caps shift)))
               (string-upcase text)
               text)))))))

(defun key-modifier (value)
  (case value
    ((:caps-lock :scroll-lock :num-lock)
     value)
    ((:left-shift :right-shift)
     :shift)
    ((:left-alt :right-alt)
     :alt)
    ((:left-control :right-control)
     :control)))

(defmethod activate ((key virtual-key))
  (let ((ui (ui key))
        (value (value key))
        (mods (modifiers (keyboard key)))
        (target (target (keyboard key))))
    (when target
      (handle (make-instance 'key-down :key value :code 0 :modifiers mods) target)
      (handle (make-instance 'key-up :key value :code 0 :modifiers mods) target)
      (let ((text (key-text key ui)))
        (when text (handle (make-instance 'text-event :text text) target))))))

(defmethod handle ((event pointer-down) (key virtual-key))
  (let ((mod (key-modifier (value key)))
        (mods (modifiers (keyboard key))))
    (cond (mod
           (if (find mod mods)
               (setf (modifiers (keyboard key)) (setf mods (remove mod mods)))
               (setf (modifiers (keyboard key)) (setf mods (list* mod mods)))))
          (T
           (with-global-bounds (bounds key)
             (if (contained-p (location event) bounds)
                 (setf (pressed key) T)
                 (call-next-method)))))))

(defmethod handle ((event pointer-up) (key virtual-key))
  (cond ((pressed key)
         (activate key)
         (unless (key-modifier (value key))
           (setf (pressed key) NIL)))
        (T
         (exit key))))

(defmethod handle ((event key-down) (key virtual-key))
  (when (eq (value key) (key event))
    (activate key)
    (unless (key-modifier (value key))
      (setf (pressed key) T)))
  (decline))

(defmethod handle ((event key-up) (key virtual-key))
  (when (eq (value key) (key event))
    (unless (key-modifier (value key))
      (setf (pressed key) NIL)))
  (decline))

(defclass virtual-keyboard (grid-bag-layout visual-focus-manager)
  ((cell-margins :initform (margins 1))
   (modifiers :initform () :accessor modifiers)
   (target :initform NIL :initarg :target :accessor target)))

(defmethod handle ((ev key-event) (layout virtual-keyboard))
  (do-elements (element layout)
    (handle ev element)))

(defmethod (setf bounds) :after (bounds (layout virtual-keyboard))
  ;; FIXME: probably better to do this in suggest-size
  (let* ((cols (length (col-sizes layout)))
         (rows (length (row-sizes layout)))
         (margin (cell-margins layout))
         (c (min (/ (- (pxh bounds) (* (+ (pxu margin) (pxb margin)) rows)) rows)
                 (/ (- (pxw bounds) (* (+ (pxl margin) (pxr margin)) cols)) cols))))
    (setf (col-sizes layout) (loop for col across (col-sizes layout)
                                   collect (if (eql T col) T (/ c 2))))
    (setf (row-sizes layout) (loop for row across (row-sizes layout)
                                   collect c))))

(defmethod initialize-instance :after ((layout virtual-keyboard) &key (keyboard-spec :100%))
  (let* ((keyboard-spec (ensure-keyboard-spec keyboard-spec))
         (cols (loop for row in keyboard-spec maximize
                     (loop for (_ size) in row sum (if (eql size T) 1 size))))
         (ratio (/ cols (length keyboard-spec))))
    (setf (col-sizes layout) (append '(T) (loop repeat (* 2 cols) collect 25) '(T)))
    (setf (row-sizes layout) (loop repeat (length keyboard-spec) collect 50))
    (setf (sizing-strategy layout) (make-instance 'proportional :aspect-ratio ratio))
    (loop for y from 0
          for row in keyboard-spec
          for flex-count = (cl:count T row :key #'second)
          for flex-space = (* 2 (- cols (loop for (_ size) in row sum (if (eql T size) 0 size))))
          for flex = (floor flex-space flex-count)
          do (loop for x = 1 then (+ x w)
                   for (key size) in row
                   for w = (round (cond ((numberp size)
                                         (* 2 size))
                                        ((= 1 flex-count)
                                         flex-space)
                                        (T
                                         (decf flex-count)
                                         (decf flex-space flex)
                                         flex)))
                   for component = (if key
                                       (make-instance 'virtual-key :value key :keyboard layout)
                                       (make-instance 'component :data NIL))
                   do (enter component layout :x x :y y :w w :h 1)))))

(defmethod (setf modifiers) :after (mods (keyboard virtual-keyboard))
  (do-elements (key keyboard)
    (when (typep key 'virtual-key)
      (let ((mod (key-modifier (value key))))
        (when mod (setf (pressed key) (member mod mods)))))))
