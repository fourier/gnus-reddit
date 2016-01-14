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
(require 'reddit)

(defvar gnus-reddit-json-parsers
  '((String . identity)
    (special . identity)
    (int . identity)
    (long . identity)
    (boolean . (lambda (x) (cond ((eql x :json-false) nil)
                                 ((eql x :json-true) t)
                                 (t nil))))
    (array 	. identity)
    (Message? . identity)
    (List<String> . identity)
    (List<thing> . identity))
  "A mapping between json types used in reddit api and corresponding parsers")


(defmacro gnus-reddit-create-parser (name &rest format)
  "Create a struct NAME representing the FORMAT and create function.
Usage example:
Given call

\(gnus-reddit-create-parser listing
\  (String before \"The fullname of the listing that follows before this page. null if there is no previous page.\")
\  (String after \"The fullname of the listing that follows after this page. null if there is no next page.\")
\  (String modhash \"This modhash is not the same modhash provided upon login. You do not need to update your user's modhash everytime you get a new modhash. You can reuse the modhash given upon login.\")
\  (List<thing> children \"A list of things that this Listing wraps.\"))

Will generate the following code:

\(progn
\  (cl-defstruct gnus-reddit-listing id name kind before after modhash children)
\  (defun gnus-reddit-create-listing
\      (content)
\    (let*
\	((data
\	  (cdr
\	   (assq 'data content)))
\	 (id
\	  (cdr
\	   (assq 'id content)))
\	 (name
\	  (cdr
\	   (assq 'name content)))
\	 (kind
\	  (cdr
\	   (assq 'kind content)))
\	 (before
\	  (cdr
\	   (assq 'before data)))
\	 (after
\	  (cdr
\	   (assq 'after data)))
\	 (modhash
\	  (cdr
\	   (assq 'modhash data)))
\	 (children
\	  (cdr
\	   (assq 'children data))))
\      (when
\	  (or before after modhash children)
\	(make-gnus-reddit-listing :id
\				  (funcall
\				   (cdr
\				    (assq 'String gnus-reddit-json-parsers))
\				   id)
\				  :name
\				  (funcall
\				   (cdr
\				    (assq 'String gnus-reddit-json-parsers))
\				   name)
\				  :kind
\				  (funcall
\				   (cdr
\				    (assq 'String gnus-reddit-json-parsers))
\				   kind)
\				  :before
\				  (funcall
\				   (cdr
\				    (assq 'String gnus-reddit-json-parsers))
\				   before)
\				  :after
\				  (funcall
\				   (cdr
\				    (assq 'String gnus-reddit-json-parsers))
\				   after)
\				  :modhash
\				  (funcall
\				   (cdr
\				    (assq 'String gnus-reddit-json-parsers))
\				   modhash)
\				  :children
\				  (funcall
\				   (cdr
\				    (assq 'List<thing> gnus-reddit-json-parsers))
\				   children))))))

These struct and function could be used in the following way:

Declare some variable containing json output of listings:

\(defvar gnus-reddit-example-subreddits
\  (reddit-get \"/subreddits/default.json\" nil))

and then we can fill in the appropriate struct and use it:

\(let ((sr
\       (gnus-reddit-create-listing gnus-reddit-example-subreddits)))
\  (cl-prettyprint
\   (gnus-reddit-listing-kind sr)))

"
  (declare (indent 1) (debug ((symbolp form &optional form) format)))
  (let* ((struct-name (concat "gnus-reddit" "-" (symbol-name name)))
         (struct-symbol (intern struct-name))
         (make-symbol (intern (concat "make-" struct-name)))
         (create-func-symbol (intern (concat "gnus-reddit-create-" (symbol-name name))))
         (field-parsers (mapcar (lambda (x)
                                  (let* ((field (cadr x))
                                         (keyword
                                          (intern (concat ":" (symbol-name field))))
                                         (type (car x)))
                                    `(,keyword
                                      (funcall (cdr (assq ',type
                                                          gnus-reddit-json-parsers))
                                               ,field))))
                                ;;field))
                                format))
         (flatten-field-parsers nil))
    (mapc (lambda (x)
            (mapc (lambda (y) (push y flatten-field-parsers)) x))
          field-parsers)
    `(progn 
       (cl-defstruct ,struct-symbol id name kind
                     ,@(mapcar #'cadr format))
       (defun ,create-func-symbol (content)
         ;; common from Thing
         (let* ((data (cdr (assq 'data content)))
                (id (cdr (assq 'id content)))
                (name (cdr (assq 'name content)))
                (kind (cdr (assq 'kind content)))
                ;; own from Listing
                ,@(mapcar (lambda (x)
                            (let ((field (cadr x)))
                              `(,field (cdr (assq ',field data)))))
                          format))
           (when (or ,@(mapcar #'cadr format))
             (,make-symbol
              ;; from Thing
              :id (funcall (cdr (assq 'String gnus-reddit-json-parsers)) id)
              :name (funcall (cdr (assq 'String gnus-reddit-json-parsers)) name)
              :kind (funcall (cdr (assq 'String gnus-reddit-json-parsers)) kind)
              ,@(nreverse flatten-field-parsers)
              )))))))


(gnus-reddit-create-parser listing
  (String before "The fullname of the listing that follows before this page. null if there is no previous page.")
  (String after "The fullname of the listing that follows after this page. null if there is no next page.")
  (String modhash "This modhash is not the same modhash provided upon login. You do not need to update your user's modhash everytime you get a new modhash. You can reuse the modhash given upon login.")
  (List<thing> children "A list of things that this Listing wraps."))


(defun gnus-reddit-get-subreddits ()
  (let ((content (reddit-get "/subreddits/default.json" nil)))
    (when content
      (gnus-reddit-create-listing content))))
    



(provide 'gnus-reddit-api)
;;; gnus-reddit-api.el ends here
