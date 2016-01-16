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


(gnus-reddit-create-parser subreddit
  (int 	accounts_active 	"number of users active in last 15 minutes")
  (int 	comment_score_hide_mins 	"number of minutes the subreddit initially hides comment scores")
  (String 	description 	"sidebar text")
  (String 	description_html 	"sidebar text, escaped HTML format")
  (String 	display_name 	"human name of the subreddit")
  (String 	header_img 	"full URL to the header image, or null")
  (array 	header_size 	"width and height of the header image, or null")
  (String 	header_title 	"description of header image shown on hover, or null")
  (boolean 	over18 	"whether the subreddit is marked as NSFW")
  (String 	public_description 	"Description shown in subreddit search results?")
  (boolean 	public_traffic 	"whether the subreddit's traffic page is publicly-accessible")
  (long 	subscribers 	"the number of redditors subscribed to this subreddit")
  (String 	submission_type 	"the type of submissions the subreddit allows - one of \"any\", \"link\" or \"self\"")
  (String 	submit_link_label 	"the subreddit's custom label for the submit link button, if any")
  (String 	submit_text_label 	"the subreddit's custom label for the submit text button, if any")
  (String 	subreddit_type 	"the subreddit's type - one of \"public\", \"private\", \"restricted\", or in very special cases \"gold_restricted\" or \"archived\"")
  (String 	title 	"title of the main page")
  (String 	url 	"The relative URL of the subreddit. Ex: \"/r/pics/\"")
  (boolean 	user_is_banned 	"whether the logged-in user is banned from the subreddit")
  (boolean 	user_is_contributor 	"whether the logged-in user is an approved submitter in the subreddit")
  (boolean 	user_is_moderator 	"whether the logged-in user is a moderator of the subreddit")
  (boolean 	user_is_subscriber 	"whether the logged-in user is subscribed to the subreddit"))


(defun gnus-reddit-get-subreddits (&optional content)
  "Returns the listing struct of subreddits from reddit or optional CONTENT.
The children of the subreddit are subreddit structs"
  (let ((content (or content ;; either content given or get from the server
                     (reddit-get "/subreddits/popular.json" nil))))
    (when content
      (let ((sr (gnus-reddit-create-listing content)))
        ;; parse children of the listing
        (dotimes (n (length (gnus-reddit-listing-children sr)))
          (setf (aref (gnus-reddit-listing-children sr) n)
                (gnus-reddit-create-subreddit (aref (gnus-reddit-listing-children sr) n))))
        sr))))


    



(provide 'gnus-reddit-api)
;;; gnus-reddit-api.el ends here
