(in-package #:org.shirakumo.alloy)

(defparameter *100%-keyboard*
  "
⎋ _ F1 F2 F3 F4 _ F5 F6 F7 F8 _ F9 F10 F11 F12 | ⎙ ⇳ ⎉ | F13 F14 F15 F16
$  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ | ⌤ ⇱ ⇞ | ⇭ K/ K* K-
↹_  Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ | ⌦ ⇲ ⇟ | K7 K8 K9 K+
⇪_   A  S  D  F  G  H  J  K  L  ;  '  \ ↵_ | _ _ _ | K4 K5 K6 K+
L⇧_   Z  X  C  V  B  N  M  ,  .  /     R⇧_ | _ ↑ _ | K1 K2 K3 K↵
L⌃ L⌘ L⌥           ␣_             R⌥ R⌘ R⌃ | ← ↓ → | K0_   K. K↵
")

(defparameter *80%-keyboard*
  "
⎋ _ F1 F2 F3 F4 _ F5 F6 F7 F8 _ F9 F10 F11 F12 | ⎙ ⇳ ⎉
$  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ | ⌤ ⇱ ⇞
↹_  Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ | ⌦ ⇲ ⇟
⇪_   A  S  D  F  G  H  J  K  L  ;  '  \ ↵_ | _ _ _
L⇧_   Z  X  C  V  B  N  M  ,  .  /     R⇧_ | _ ↑ _
L⌃ L⌘ L⌥           ␣_             R⌥ R⌘ R⌃ | ← ↓ →
")

(defparameter *75%-keyboard*
  "
⎋ F1 F2 F3 F4 F5 F6 F7 F8 F9 F10 F11 F12 ⎙ ⇳ ⎉
$  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ ⇱
↹_  Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ ⇞
⇪_   A  S  D  F  G  H  J  K  L  ;  '  \ ↵_ ⇟
L⇧_   Z  X  C  V  B  N  M  ,  .  /   R⇧_ ↑ ⇲
L⌃ L⌘ L⌥          ␣_          R⌥ R⌘ R⌃ ← ↓ →
")

(defparameter *65%-keyboard*
  "
⎋  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_ $
↹_  Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_ ⌦
⇪_   A  S  D  F  G  H  J  K  L  ;  '  \ ↵_ ⇞
L⇧_   Z  X  C  V  B  N  M  ,  .  /   R⇧_ ↑ ⇟
L⌃ L⌘ L⌥          ␣_          R⌥ R⌘ R⌃ ← ↓ →
")

(defparameter *60%-keyboard*
  "
⎋  1  2  3  4  5  6  7  8  9  0  -  =   ⌫_
↹_  Q  W  E  R  T  Y  U  I  O  P  [  ]  ↵_
⇪_   A  S  D  F  G  H  J  K  L  ;  '  \ ↵_
L⇧_   Z  X  C  V  B  N  M  ,  .  /     R⇧_
L⌃ L⌘ L⌥          ␣_              R⌥ R⌘ R⌃
")

(defvar *keyboard-short-map*
  '(("_" NIL)
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
  (let ((extended NIL))
    (when (char= #\_ (char key (1- (length key))))
      (setf extended T)
      (setf key (subseq key 0 (1- (length key)))))
    (values (or (second (assoc key *keyboard-short-map* :test #'string-equal))
                (intern (string-upcase key) "KEYWORD"))
            extended)))

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
                        collect (multiple-value-list (map-short-key key))
                        until (<= (length line) end)))))

(defclass virtual-keyboard (structure)
  ())

(defmethod initialize-instance :after ((keyboard virtual-keyboard) &key (layout *100%-keyboard*)))

