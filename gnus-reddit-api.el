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
    (Boolean . (lambda (x) (cond ((eql x :json-false) nil)
                                 ((eql x :json-true) t)
                                 (t nil))))
    (array 	. identity)
    (Message? . identity)
    (List<String> . identity)
    (List<thing> . identity)
    (Object . identity))
  "A mapping between json types used in reddit api and corresponding parsers")


(defmacro gnus-reddit-create-parser (name inherits &rest format)
  "Create a struct NAME inheriting fields from all in the list INHERITS representing the FORMAT and create function.
Usage example:
Given call

\(gnus-reddit-create-parser listing ()
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
  (declare (indent 1))
  (let ((fmt (cl-copy-list format)))
    (dolist (base inherits)
      (let* ((inherits-from (intern (concat "gnus-reddit-"
                                            (symbol-name base))))
             (props (get `,inherits-from 'format)))
        (dolist (p props)
          (cl-pushnew p fmt :test
                      #'(lambda (x y) (and (equal (car x) (car y))
                                           (equal (cadr x) (cadr y))))))))
    (let* ((struct-name (concat "gnus-reddit" "-" (symbol-name name)))
           (struct-symbol (intern struct-name))
           (make-struct-symbol (intern (concat "make-" struct-name)))
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
                                  fmt))
           (flatten-field-parsers nil))
      (mapc (lambda (x)
              (mapc (lambda (y) (push y flatten-field-parsers)) x))
            field-parsers)
      `(progn 
         (cl-defstruct ,struct-symbol
           ,@(mapcar #'cadr fmt))
         (defun ,create-func-symbol (content)
           ;; common from Thing
           (let* ((data (cdr (assq 'data content)))
                  ;; own from Listing
                  ,@(mapcar (lambda (x)
                              (let ((field (cadr x)))
                                `(,field (cdr (assq ',field data)))))
                            fmt))
             (when (or ,@(mapcar #'cadr fmt))
               (,make-struct-symbol
                ;; from Thing
                ,@(nreverse flatten-field-parsers)))))
         ,@(mapcar (lambda (field) `(put ',(intern (concat struct-name "-" (symbol-name (cadr field))))
                                         'function-documentation
                                         ,(nth 2 field)))
                   fmt)
         (put ',struct-symbol 'format ',fmt)))))


(gnus-reddit-create-parser thing nil 
  (String 	id 	"this item's identifier, e.g. \"8xwlg\"")
  (String 	name 	"Fullname of comment, e.g. \"t1_c3v7f8u\"")
  (String 	kind 	"All things have a kind. The kind is a String identifier that denotes the object's type. Some examples: Listing, more, t1, t2"))


(gnus-reddit-create-parser listing nil
  (String kind 	"All things have a kind. The kind is a String identifier that denotes the object's type. Some examples: Listing, more, t1, t2")
  (String before "The fullname of the listing that follows before this page. null if there is no previous page.")
  (String after "The fullname of the listing that follows after this page. null if there is no next page.")
  (String modhash "This modhash is not the same modhash provided upon login. You do not need to update your user's modhash everytime you get a new modhash. You can reuse the modhash given upon login.")
  (List<thing> children "A list of things that this Listing wraps."))

(gnus-reddit-create-parser votable nil
 (int 	ups 	"the number of upvotes. (includes own)")
 (int 	downs 	"the number of downvotes. (includes own)")
 (Boolean 	likes 	"true if thing is liked by the user, false if thing is disliked, null if the user has not voted or you are not logged in. Certain languages such as Java may need to use a boolean wrapper that supports null assignment."))

(gnus-reddit-create-parser created nil
 (long 	created 	"the time of creation in local epoch-second format. ex: 1331042771.0")
 (long 	created_utc 	"the time of creation in UTC epoch-second format. Note that neither of these ever have a non-zero fraction."))

(gnus-reddit-create-parser subreddit (thing)
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

(gnus-reddit-create-parser link (thing votable created)
  (String  author  "the account name of the poster. null if this is a promotional link")
  (String 	author_flair_css_class 	"the CSS class of the author's flair. subreddit specific")
  (String 	author_flair_text 	"the text of the author's flair. subreddit specific")
  (boolean 	clicked 	"probably always returns false")
  (String 	domain 	"the domain of this link. Self posts will be self.<subreddit> while other examples include en.wikipedia.org and s3.amazon.com")
  (boolean 	hidden 	"true if the post is hidden by the logged in user. false if not logged in or not hidden.")
  (boolean 	is_self 	"true if this link is a selfpost")
  (String 	link_flair_css_class 	"the CSS class of the link's flair.")
  (String 	link_flair_text 	"the text of the link's flair.")
  (boolean 	locked 	"whether the link is locked (closed to new comments) or not.")
  (Object 	media 	"Used for streaming video. Detailed information about the video and it's origins are placed here")
  (Object 	media_embed 	"Used for streaming video. Technical embed specific information is found here.")
  (int 	num_comments 	"the number of comments that belong to this link. includes removed comments.")
  (boolean 	over_18 	"true if the post is tagged as NSFW. False if otherwise")
  (String 	permalink 	"relative URL of the permanent link for this link")
  (boolean 	saved 	"true if this post is saved by the logged in user")
  (int 	score 	"the net-score of the link. note: A submission's score is simply the number of upvotes minus the number of downvotes. If five users like the submission and three users don't it will have a score of 2. Please note that the vote numbers are not \"real\" numbers, they have been \"fuzzed\" to prevent spam bots etc. So taking the above example, if five users upvoted the submission, and three users downvote it, the upvote/downvote numbers may say 23 upvotes and 21 downvotes, or 12 upvotes, and 10 downvotes. The points score is correct, but the vote totals are \"fuzzed\".")
  (String 	selftext 	"the raw text. this is the unformatted text which includes the raw markup characters such as ** for bold. <, >, and & are escaped. Empty if not present.")
  (String 	selftext_html 	"the formatted escaped HTML text. this is the HTML formatted version of the marked up text. Items that are boldened by ** or *** will now have <em> or *** tags on them. Additionally, bullets and numbered lists will now be in HTML list format. NOTE: The HTML string will be escaped. You must unescape to get the raw HTML. Null if not present.")
  (String 	subreddit 	"subreddit of thing excluding the /r/ prefix. \"pics\"")
  (String 	subreddit_id 	"the id of the subreddit in which the thing is located")
  (String 	thumbnail 	"full URL to the thumbnail for this link; \"self\" if this is a self post; \"default\" if a thumbnail is not available")
  (String 	title 	"the title of the link. may contain newlines for some reason")
  (String 	url 	"the link of this post. the permalink if this is a self-post")
  (long 	edited 	"Indicates if link has been edited. Will be the edit timestamp if the link has been edited and return false otherwise. https://github.com/reddit/reddit/issues/581")
  (String 	distinguished 	"to allow determining whether they have been distinguished by moderators/admins. null = not distinguished. moderator = the green [M]. admin = the red [A]. special = various other special distinguishes http://bit.ly/ZYI47B")
  (boolean 	stickied 	"true if the post is set as the sticky in its subreddit."))


(gnus-reddit-create-parser comment (thing votable created)
  (String 	approved_by 	"who approved this comment. null if nobody or you are not a mod")
  (String 	author 	"the account name of the poster")
  (String 	author_flair_css_class 	"the CSS class of the author's flair. subreddit specific")
  (String 	author_flair_text 	"the text of the author's flair. subreddit specific")
  (String 	banned_by 	"who removed this comment. null if nobody or you are not a mod")
  (String 	body 	"the raw text. this is the unformatted text which includes the raw markup characters such as ** for bold. <, >, and & are escaped.")
  (String 	body_html 	"the formatted HTML text as displayed on reddit. For example, text that is emphasised by * will now have <em> tags wrapping it. Additionally, bullets and numbered lists will now be in HTML list format. NOTE: The HTML string will be escaped. You must unescape to get the raw HTML.")
  (special 	edited 	"false if not edited, edit date in UTC epoch-seconds otherwise. NOTE: for some old edited comments on reddit.com, this will be set to true instead of edit date.")
  (int 	gilded 	"the number of times this comment received reddit gold")
  (String 	link_author 	"present if the comment is being displayed outside its thread (user pages, /r/subreddit/comments/.json, etc.). Contains the author of the parent link")
  (String 	link_id 	"ID of the link this comment is in")
  (String 	link_title 	"present if the comment is being displayed outside its thread (user pages, /r/subreddit/comments/.json, etc.). Contains the title of the parent link")
  (String 	link_url 	"present if the comment is being displayed outside its thread (user pages, /r/subreddit/comments/.json, etc.). Contains the URL of the parent link")
  (int 	num_reports 	"how many times this comment has been reported, null if not a mod")
  (String 	parent_id 	"ID of the thing this comment is a reply to, either the link or a comment in it")
  (List<thing> 	replies 	"A list of replies to this comment")
  (boolean 	saved 	"true if this post is saved by the logged in user")
  (int 	score 	"the net-score of the comment")
  (boolean 	score_hidden 	"Whether the comment's score is currently hidden.")
  (String 	subreddit 	"subreddit of thing excluding the /r/ prefix. \"pics\"")
  (String 	subreddit_id 	"the id of the subreddit in which the thing is located")
  (String 	distinguished 	"to allow determining whether they have been distinguished by moderators/admins. null = not distinguished. moderator = the green [M]. admin = the red [A]. special = various other special distinguishes http://redd.it/19ak1b"))

(defun gnus-reddit-preprocess-subreddit (subreddit)
  "Append leading and trailing / in SUBREDDIT short url"
  (concat (unless (string-prefix-p "/" subreddit) "/")
          subreddit
          (unless (string-suffix-p "/" subreddit) "/")))

                           
(cl-defun gnus-reddit-get-subscribed-subreddits (&optional
                                                 &key (limit 25)
                                                 &key before
                                                 &key after)
  "Returns the listing struct of subscribed subreddits from reddit.
Optional arguments:
LIMIT (default 25) is a limit of subreddits, maximum 100 according to the Reddit API.
BEFORE is the name of the listing before this page
AFTER is the name of the listing before this page
The children of the subreddit is a vector of subreddit structs"
  (let ((args (list :limit limit)))
    (when before
      (push before args)
      (push :before args))
    (when after
      (push after args)
      (push :after args))
;;    (let ((content (reddit-get "/subreddits/popular" args)))
      (let ((content (reddit-get "/subreddits/mine/subscriber" args)))
      (when content
        (let ((sr (gnus-reddit-create-listing content)))
          ;; parse children of the listing
          (dotimes (n (length (gnus-reddit-listing-children sr)))
            (setf (aref (gnus-reddit-listing-children sr) n)
                  (gnus-reddit-create-subreddit (aref (gnus-reddit-listing-children sr) n))))
          sr)))))

(cl-defun gnus-reddit-get-subreddit-topics (subreddit
                                            &optional
                                            &key (type 'hot)
                                            &key limit
                                            &key before
                                            &key after)
  "Returns a listing of link structs for given SUBREDDIT.
Subreddit is a relative url: r/emacs, r/lisp etc.
TYPE is a optional type of topics, one of 'HOT (by default), 'TOP or 'NEW
LIMIT (default 25) is a limit of topics, maximum 100 according to the Reddit API.
BEFORE is the name of the listing before this page
AFTER is the name of the listing before this page"
  (let ((args (list :t "all")))
    (when before
      (push before args)
      (push :before args))
    (when after
      (push after args)
      (push :after args))
    (let ((type-arg 
           (cond ((eql type 'new) "new")
                 ((eql type 'top) "top")
                 (t "hot"))))
      (let* ((api (concat (gnus-reddit-preprocess-subreddit subreddit) type-arg))
             (content (reddit-get api args)))
        (when content
          (let ((listing (gnus-reddit-create-listing content)))
            (when listing
              ;; parse children of the listing
              (dotimes (n (length (gnus-reddit-listing-children listing)))
                (setf (aref (gnus-reddit-listing-children listing) n)
                      (gnus-reddit-create-link (aref (gnus-reddit-listing-children listing) n))))
              listing)))))))
      
    



(provide 'gnus-reddit-api)
;;; gnus-reddit-api.el ends here
