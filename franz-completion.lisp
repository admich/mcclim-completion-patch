(in-package :climi)
;;; Copyright (c) 2022, Andrea De Michele (andrea.demichele@gmail.com)
;;;; Franz (clim-tos) chunkwise completion adapted for McCLIM

;;; Copyright (c) 1985-2016 Franz, Inc.
;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates."

;;; Redistribution and use in source and binary forms are permitted
;;; provided that the above copyright notice and this paragraph are
;;; duplicated in all such forms and that any documentation, advertising
;;; materials, and other materials related to such distribution and use
;;; acknowledge that the software was developed by the organizations
;;; listed above. The name of the organizations may not be used to endorse
;;; or promote products derived from this software without specific prior
;;; written permission.  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE.



(defvar *null-object* '#:null)

;; Complete STRING chunk-wise against the completion possibilities in the
;; COMPLETIONS, using DELIMITERS to break the strings into chunks.  ACTION
;; should be :COMPLETE, :COMPLETE-LIMITED, :COMPLETE-MAXIMAL, or
;; :POSSIBILITIES (see below).  NAME-KEY and VALUE-KEY are used to extract
;; the completion string and object from the entries in COMPLETIONS, and
;; PREDICATE (if supplied) is applied to filter out unwanted objects.
;; Returns five values, the completed string, whether or not the completion
;; successfully matched, the object associated with the completion, the
;; number of things that matches, and (if ACTION is :POSSIBILITIES) a list
;; of possible completions.
;;
;; When ACTION is :COMPLETE, this completes the input as much as possible,
;; except that if the user's input exactly matches one of the possibilities,
;; even if it is a left substring of another possibility, the shorter
;; possibility is returned as the result.
;; When ACTION is :COMPLETE-LIMITED, this completes the input up to the next
;; partial delimiter.
;; When ACTION is :COMPLETE-MAXIMAL, this completes the input as much as possible.
;; When ACTION is :POSSIBILITIES or :APROPOS-POSSIBILITIES, this returns a list
;; of the possible completions.
(defun complete-from-possibilities (string completions delimiters
                                    &key (action :complete) predicate
                                         (name-key #'first) (value-key #'second))
  (when (and (not (eq action :possibilities))
             (not (eq action :apropos-possibilities))
             (zerop (length string)))
    (return-from complete-from-possibilities 
      (values nil nil nil 0 nil)))
  (let* ((best-completion nil)
         (best-length nil)
         (best-object *null-object*)
         (nmatches 0)
         (possibilities nil))
    (flet ((complete-1 (possibility)
             (let ((completion (funcall name-key possibility))
                   (object (funcall value-key possibility)))
               (when (or (null predicate)
                         (funcall predicate object))
                 ;; If we are doing simple completion and the user-supplied string is
                 ;; exactly equal to this completion, then claim success (even if there
                 ;; are other completions that have this one as a left substring!).
                 (when (and (eq action :complete)
                            (string-equal string completion))
                   (return-from complete-from-possibilities
                     (values completion t object 1)))
                 (multiple-value-setq
                     (best-completion best-length best-object nmatches possibilities)
                   (chunkwise-complete-string string completion object action delimiters
                                              best-completion best-length best-object
                                              nmatches possibilities))))))
      (declare (dynamic-extent #'complete-1))
      (map nil #'complete-1 completions))
    (values (if best-completion (subseq best-completion 0 best-length) string)
            (not (eq best-object *null-object*))
            (if (eq best-object *null-object*) nil best-object)
            nmatches
            (nreverse possibilities))))

;; Just like COMPLETE-FROM-POSSIBILITIES, except that the possibilities are
;; gotten by funcalling a generator rather than from a completion alist.
(defun complete-from-generator (string generator delimiters
                                &key (action :complete) predicate)
  (declare (dynamic-extent generator))
  (when (and (not (eq action :possibilities))
             ;; (not (eq action :apropos-possibilities))
             (zerop (length string)))
    (return-from complete-from-generator 
      (values nil nil nil 0 nil)))
  (let* ((best-completion nil)
         (best-length nil)
         (best-object *null-object*)
         (possibilities nil)
         (nmatches 0))
    (flet ((suggest-handler (completion object &optional presentation-type)
             (declare (ignore presentation-type))        ;for now
             (when (or (null predicate)
                       (funcall predicate object))
               (when (and (eq action :complete)
                          (string-equal string completion))
                 (return-from complete-from-generator
                   (values completion t object 1)))
               (multiple-value-setq
                   (best-completion best-length best-object nmatches possibilities)
                 (chunkwise-complete-string string completion object action delimiters
                                            best-completion best-length best-object
                                            nmatches possibilities)))))
      (declare (dynamic-extent #'suggest-handler))
      (funcall generator string #'suggest-handler))
    (values (if best-completion (subseq best-completion 0 best-length) string)
            (not (eq best-object *null-object*))
            (if (eq best-object *null-object*) nil best-object)
            nmatches
            (nreverse possibilities))))

;; The common subroutine used to do chunkwise completion.
;;--- Extending this to support completion aarrays is pretty straightforward
(defun chunkwise-complete-string (string completion object action delimiters
                                  best-completion best-length best-object
                                  nmatches possibilities)
  (let* ((length (length string))
         (matches (if (eq action :apropos-possibilities)
                      (if (search string completion :test #'char-equal) length 0)
                      (chunkwise-string-compare string completion delimiters))))
    (when (= matches length)
      (incf nmatches)
      (case action
        ((:possibilities :apropos-possibilities)
         (push (list completion object) possibilities))
        ((:complete :complete-maximal)
         nil)
        (:complete-limited
         ;; Match up only as many chunks as the user has typed
         (flet ((delimiter-p (char)
                  (member char delimiters)))
           (declare (dynamic-extent #'delimiter-p))
           (let* ((nchunks (1+ (count-if #'delimiter-p string)))
                  (cutoff (let ((start 0)
                                (cutoff nil))
                            (dotimes (i nchunks cutoff)
                              ;; #-(or Genera Minima allegro aclpc) (declare (ignore i))
                              (let ((new (position-if #'delimiter-p completion :start start)))
                                (unless new (return nil))
                                (setq cutoff new
                                      start (1+ new)))))))
             (when cutoff
               (setq completion (subseq completion 0 (1+ cutoff)))
               ;; Increment this once more to make the higher level think
               ;; that the completion is ambiguous
               (incf nmatches))))))
      (cond (best-completion
             (let ((new-length (chunkwise-string-compare best-completion completion delimiters
                                                         t best-length)))
               (cond ((or (null best-length)
                          (> new-length best-length))
                      (setq best-length new-length
                            best-object object))
                     (t
                      (setq best-length new-length
                            best-object *null-object*)))))
            (t
             (setq best-completion (copy-seq completion)
                   best-length (length best-completion)
                   best-object object)))))
  (values best-completion best-length best-object nmatches possibilities))

;; Compare STRING1 against STRING2 in "chunks", using DELIMITERS to break
;; the strings into chunks.  Returns two values, the index of the first place
;; where the strings mismatches and the index of the last character that was
;; unambiguous.  When MERGE-P, STRING1 gets side-effected.
(defun chunkwise-string-compare (string1 string2 delimiters &optional merge-p end1)
  (let ((len1 (or end1 (length string1)))
        (len2 (length string2))
        (matched 0)
        ambiguous
        (i1 0) (i2 0)
        char1 char2)
    (loop
      (unless (and (< i1 len1) (< i2 len2))
        (return))
      (setq char1 (aref string1 i1)
            char2 (aref string2 i2))
      (cond ((or (eql char1 char2)
                 (char-equal char1 char2))
             (when merge-p
               (setf (aref string1 matched) char1))
             (incf matched) (incf i1) (incf i2))
            (t
             (unless ambiguous
               (setq ambiguous matched))
             (cond ((member char1 delimiters)
                    (when (or (and (not merge-p)
                                   (> i1 matched))
                              (member char2 delimiters))
                      (return nil)))
                   ((member char2 delimiters)
                    (when (and (not merge-p)
                               (> i2 matched))
                      (return nil)))
                   (t (unless merge-p
                        (return nil))))
             (loop
               (when (or (member (aref string1 i1) delimiters)
                         (>= (incf i1) len1))
                 (return)))
             (loop
               (when (or (member (aref string2 i2) delimiters)
                         (>= (incf i2) len2))
                 (return))))))
    (values matched (or ambiguous matched))))
