;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006-2008 by Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2022 by Andrea De Michele (andrea.demichele@gmail.com)
;;; ---------------------------------------------------------------------------

(in-package :climi)

(defun complete-input (stream func &key
                                     partial-completers allow-any-input
                                     (possibility-printer #'possibility-printer)
                                     (help-displays-possibilities t))
  (let ((so-far (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
        (input-position (stream-scan-pointer stream)))
    (with-dynamic-gestures (*accelerator-gestures* (append *help-gestures*
                                                           *possibilities-gestures*
                                                           *accelerator-gestures*
                                                           *completion-gestures*))
      (flet ((insert-input (input)
               (adjust-array so-far (length input)
                             :fill-pointer (length input))
               (replace so-far input)
               ;; XXX: Relies on non-specified behavior of :rescan.
               (replace-input stream input :rescan nil :buffer-start input-position))
             (read-possibility (stream possibilities)
               (unwind-protect
                    (handler-case
                        (with-input-context
                            (`(completion ,possibilities) :override nil)
                            (object type event)
                            (prog1 nil (read-gesture :stream stream :peek-p t))
                          (t object))
                      (abort-gesture () nil))
                 (clear-typeout stream))))
        (loop
          (multiple-value-bind (gesture mode)
              (read-completion-gesture stream
                                       partial-completers
                                       help-displays-possibilities)
            (cond
              (mode
               (multiple-value-bind
                     (input success object nmatches possibilities)
                   (funcall func (if (eq mode :complete-limited)
                                     (concatenate 'string (subseq so-far 0) (list gesture))
                                     (subseq so-far 0))
                            mode)
                 (when (and (or (zerop nmatches)
                                (and (not (stream-rescanning-p stream)) success))
                            (eq mode :complete-limited)
                            (complete-gesture-p gesture))
                   ;; Gesture is both a partial completer and a
                   ;; delimiter e.g., #\space.  If no partial match,
                   ;; try again with a total match.
                   (setf (values input success object nmatches possibilities)
                         (funcall func (subseq so-far 0) :complete))
                   (setf mode :complete))
                 (when (stream-rescanning-p stream)
                   (vector-push-extend gesture so-far))
                 ;; Preserve the delimiter
                 (when (and success (eq mode :complete))
                   (unread-gesture gesture :stream stream))
                 (when (and (> nmatches 0) (eq mode :possibilities))
                   (print-possibilities possibilities possibility-printer stream)
                   (redraw-input-buffer stream)
                   (if-let ((possibility (read-possibility stream possibilities)))
                     (setf input (first possibility)
                           object (second possibility)
                           success t
                           nmatches 1)
                     (setf success nil
                           nmatches 0)))
                 (unless (or (and (eq mode :complete) (not success))
                             (stream-rescanning-p stream))
                   (if (> nmatches 0)
                       (insert-input input)
                       (beep)))
                 (cond ((and success (eq mode :complete))
                        (return-from complete-input
                          (values object success input)))
                       ((activation-gesture-p gesture)
                        (if allow-any-input
                            (return-from complete-input
                              (values nil t (subseq so-far 0)))
                            (error 'simple-completion-error
                                   :format-control "Input ~S does not match"
                                   :format-arguments (list so-far)
                                   :input-so-far so-far))))))
              ((null gesture) ; e.g. end-of-input if STREAM is a string stream
               (return-from complete-input (values nil nil so-far)))
              (t
               (vector-push-extend gesture so-far)))))))))

