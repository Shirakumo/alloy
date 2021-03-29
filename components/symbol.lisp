#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass symb (input-line filtered-text-input validated-text-input transformed-text-input)
  ((constrained-package :initform NIL :initarg :package :accessor constrained-package)
   (allow-interning :initform T :initarg :allow-interning :accessor allow-interning)))

(defmethod component-class-for-object ((symbol symbol)) (find-class 'symb))

(defmethod accept-character ((symb symb) c &optional (state (list NIL NIL)))
  (destructuring-bind (escaped colon) state
    (cond (escaped
           (values T (list NIL colon)))
          ((char= #\\ c)
           (values T (list T colon)))
          ((char= #\: c)
           (if (constrained-package symb)
               (values NIL (list NIL T))
               (values (not colon) (list NIL T))))
          (T
           (values T (list NIL colon))))))

(defun parse-symbol-designator (text)
  (with-input-from-string (in text)
    (let (colon)
      (flet ((process ()
               (setf colon NIL)
               (with-output-to-string (*standard-output*)
                 (loop for c = (read-char in NIL)
                       while c
                       do (case c
                            (#\\ (write-char (or (read-char in NIL) #\NUL)))
                            (#\: (setf colon T) (return))
                            (T (write-char c)))))))
        (let ((package/name (process))
              (package NIL))
          (when colon
            (setf package (if (string= "" package/name) "KEYWORD" package/name))
            (setf package/name (process)))
          (list package package/name))))))

(defun locked-package-p (package)
  #+sbcl (sb-ext:package-locked-p package)
  NIL)

(defmethod valid-p ((symb symb) text)
  (and (call-next-method)
       (destructuring-bind (package name) (parse-symbol-designator text)
         (and (if (constrained-package symb)
                  (null package)
                  (find-package package))
              (or (and (allow-interning symb)
                       (not (locked-package-p (or (constrained-package symb) package))))
                  (find-symbol name (or (constrained-package symb) package)))))))

(defmethod value->text ((symb symb) symbol)
  (with-output-to-string (*standard-output*)
    (flet ((out (s)
             (loop for c across s
                   do (case c
                        (#\\ (write-char #\\))
                        (#\: (write-char #\\)))
                      (write-char c))))
      (unless (constrained-package symb)
        (unless (eq (symbol-package symbol) (find-package "KEYWORD"))
          (out (package-name (symbol-package symbol))))
        (write-char #\:))
      (out (symbol-name symbol)))))

(defmethod text->value ((symb symb) text)
  (destructuring-bind (package name) (parse-symbol-designator text)
    (if (and (allow-interning symb)
             #+sbcl (not (sb-ext:package-locked-p package)))
        (intern name (or (constrained-package symb) package))
        (find-symbol name (or (constrained-package symb) package)))))
