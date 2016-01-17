;;; gnus-reddit.el --- Gnus method for Reddit.com access -*- lexical-binding: t; -*-

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


(require 'nnheader)
(require 'nnoo)
(eval-when-compile (require 'cl-lib))

(require 'gnus-reddit-api)
;;
;; Constants
;;


;;
;; Configurable variables
;; 

(gnus-declare-backend "nnreddit" 'post 'respool 'address)

(nnoo-declare nnreddit nnml nnmh)

(defun nnreddit-retrieve-headers (articles &optional group server fetch-old)
	(with-current-buffer nntp-server-buffer
	  (erase-buffer))
    t
  )

(defun nnreddit-open-server (server &optional definitions)
  "Open the connection to the server"
  (unless reddit-session
    ;; take the first for authinfo
    (let ((source (car (auth-source-search :host "reddit.com"))))
      (when source
        (let ((user (plist-get source :user))
              (password (funcall (plist-get source :secret))))
          (reddit-login user password))))))

(defun nnreddit-close-server (&optional server)
  "Close the server"
  ;; (setf reddit-session nil))
  )
     
(defun nnreddit-request-close ()
  "Close all sessions (on Gnus exit)"
  ;;(setf reddit-session nil))
  )

(defun nnreddit-server-opened (&optional server)
  "Determine if the server is opened"
  reddit-session)

(defun nnreddit-status-message (&optional server)
  	(with-current-buffer nntp-server-buffer
      (erase-buffer)
    t))

(defun nnreddit-request-article (article &optional group server to-buffer)
  	(with-current-buffer nntp-server-buffer
      (erase-buffer)
)  
    t)

(defun nnreddit-request-group (group &optional server fast info)
  	(with-current-buffer nntp-server-buffer
      (erase-buffer)
    t))

(defun nnreddit-close-group (group &optional server)
    )

(defun nnreddit-request-list (&optional server)
  "Returns (via nntp-server-buffer) the list of 5000 popular subreddits"
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (insert "ifi.test 1 0 n\nifi.discussion 1 0 n")
    (let (after)
      
    )
  t))

(defun nnreddit-request-post (&optional server)
  "Post current buffer to reddit"
    )

(nnoo-define-basics nnreddit)


(add-to-list 'gnus-secondary-select-methods
             '(nnreddit "someserver"))




(provide 'gnus-reddit)
;;; gnus-reddit.el ends here
