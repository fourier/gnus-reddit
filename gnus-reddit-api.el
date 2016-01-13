;;; gnus-reddit-api.el --- Reddit api wrapper -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;; Created: 2016-01-13
;; Version: 1.0.0
;; Keywords: 
;; URL: https://github.com/fourier/gnus-reddit
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

;; we are using structs from cl-lib here
(eval-when-compile (require 'cl-lib))
(require 'dash)   ; for -when-let*
(require 'reddit)

(cl-defstruct gnus-reddit-listing before after modhash children)

(defvar gnus-reddit-json-parsers
  '((String . #'identity)
    (special . #'identity)
    (int . #'identity)
    (long . #'identity)
    (boolean . (lambda (x) (cond ((eql x :json-false) nil)
                                 ((eql x :json-true) t)
                                 (t nil))))
    (array 	. #'identity)
    (Message? . #'identity)
    (List<String> . #'identity)
    (List<thing> . #'identity))
  "A mapping between json types used in reddit api and corresponding parsers")


(defvar gnus-reddit-format-listing
  '((String before "The fullname of the listing that follows before this page. null if there is no previous page.")
    (String after "The fullname of the listing that follows after this page. null if there is no next page.")
    (String modhash "This modhash is not the same modhash provided upon login. You do not need to update your user's modhash everytime you get a new modhash. You can reuse the modhash given upon login.")
    (List<thing> children "A list of things that this Listing wraps.")))

(defun gnus-reddit-get-subreddits ()
  (-when-let (sr (reddit-get "/subreddits/default.json" nil))
    (let ((data (cdr (assq 'data sr))))
      (dolist
          
      ;;      (before (cdr (assq 'before data)))
      ;;      (after (cdr (assq 'after data)))
      ;;      (modhash (cdr (assq 'modhash data)))
      ;;      (children (cdr (assq 'children data))))
      ;; ;; if at least anything
      ;; (when (or data before after children (vectorp children))
      ;;   (let ((sr 
      ;;          (make-gnus-reddit-listing :before before
      ;;                                    :after after
      ;;                                    :modhash modhash
      ;;                                    :children children)))
      ;;     sr)))))
    

  


(provide 'gnus-reddit-api)
;;; gnus-reddit-api.el ends here
