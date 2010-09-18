;;; skype.el --- skype UI for emacs users..

;; Copyright (C) 2009, 2010  SAKURAI Masashi
;; Revision 1.3

;; Author: SAKURAI Masashi <m.sakurai@kiwanami.net>
;; Keywords: skype, chat

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Improving Skype GUI.
;; This is under development...
;; 
;; To use this program, locate this file to load-path directory,
;; and add the following code to your .emacs.
;; ------------------------------
;; (require 'skype)
;; (setq skype--my-user-handle "your skype account")
;; ------------------------------
;; If you have anything.el, bind `skype--anything-command' to key,
;; like (global-set-key (kbd "M-9") 'skype--anything-command).

;;; History:

;; Revision 1.3  2010/09/15 
;; Bug fixed: skype--split. (thanks to @naota344)
;;
;; Revision 1.2  2010/01/08 
;; Bug fixed and refactored.
;;
;; Revision 1.1  2009/03/01 
;; Automatically detect icon path.
;;
;; Revision 1.0  2009/02/21 
;; First release.
;;


;;; Development memo

;; TODO
;; 
;; main buffer
;;  missed 
;;  contacts
;;  menu
;; change my profile
;; 
;; total incoming buffer
;;  replay
;;
;; contacts buffer
;;  call, chat, remove
;;  change display name
;;  last login time
;;  show profile
;; 
;; additional 
;;   custom user icon

;; [naming rule]
;; 
;; skype--com...
;; skype--cache...
;; skype--timer...
;; 
;; skype--my-status...
;; skype--emoticon...
;; skype--user...
;; skype--chat...
;; skype--chatmsg...
;; 
;; skype--chat-mode...
;; skype--message-mode...
;; 
;; [major-mode: chat]
;;
;; buffer-local: 
;;   * skype-chat-handle
;;   * skype-last-updated-time
;;   * skype-chatmsg-table
;;   * skype-send-history
;;   * skype-mode-line-chat-info
;;   * skype-auto-read-state
;; 
;; [major-mode: chatmsg]
;;
;; buffer-local: 
;;   * skype-chat-handle
;;   * skype-chat-buffer
;;   * skype-send-history
;;   * skype-history-pos
;;   * skype-writing-text
;;   * skype-commit-function
;;   * skype-mode-line-prompt
;; 
;; [major-mode: member]
;;
;; buffer-local: 
;;   * skype-chat-handle
;;   * skype-chat-buffer
;;   * skype-member-getter-function
;;   * skype-mode-line-prompt
;; 

;;; Code:

(require 'dbus)
(eval-when-compile (require 'cl))

(defvar skype--my-user-handle "(your skype account name)"
  "Your user account name.")

(defvar skype--libpath (file-name-directory (locate-library "skype"))
  "skype.el directory. [automatically detected by locate-library]")

(defvar skype--icon-path (concat skype--libpath "/icons")
  "Directory for the skype icons. [automatically detected]")
  
(defvar skype--emoticon-path (concat skype--libpath "/emoticons")
  "Directory for the skype emoticons. [automatically detected]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; private fundamental functions

(defun skype--string-trim (txt)
  "Remove white space characters at head and tail
from the given string."
  (let ((ret txt))
    (setq ret (if (string-match "^\\s-*" ret)
                  (substring ret (match-end 0))
                ret))
    (or
     (loop for i downfrom (1- (length ret)) downto 0 do 
           (if (/= 32 (char-syntax (aref ret i)))
               (return (substring ret 0 (1+ i)))))
     "")))

(defun skype--split (txt)
  (mapcar 'skype--string-trim (split-string txt ", ")))

(defmacro skype--collect (arg &rest forms)
  (declare (indent 1))
  (let ((rvar (gensym "--skype--")))
  `(let (,rvar)
     (dolist (it ,arg)
       (let ((val (progn ,@forms)))
         (if val (push val ,rvar))))
    (nreverse ,rvar))))

(defun skype--join (arg sep)
  (mapconcat (lambda(i) i) arg sep))

(defun skype--icon (text img)
  (put-text-property 0 (length text) 'display img text)
  text)

(defun skype--message (text)
  (unless (active-minibuffer-window)
    (message text)))

;; cache control

(defun skype--cache-init-db (builder expire-sec)
  "Return a cache database object.  The database object has 4
methods: get, expire, each and clear.  If a value indicated by
key is not found or expired, a new object is created by the
BUILDER function."
  (lexical-let* 
      ((db (make-hash-table :test 'equal)) ; key -> (object . time)
       (builder builder) ; lexical bind
       (expire-sec expire-sec) ; lexical bind
       (dispatch 
        (lambda (cmd args)
          (let ((cftime (float-time (current-time))))
            (cond
             ((eq cmd 'get) ; (get key)
              (let* ((key (car args))
                     (obj (gethash key db)))
                (when (or (null obj)
                          (< (+ expire-sec (cdr obj)) cftime))
                  (setq obj (cons (funcall builder key) cftime))
                  (puthash key obj db))
                (car obj)))
             ((eq cmd 'expire) ; (expire key)
              (let ((key (car args)))
                (remhash key db)))
             ((eq cmd 'clear)  ; (clear)
              (clrhash db))
             ((eq cmd 'each)   ; (each lambda(key obj time) )
              (lexical-let ((func (car args)))
                (maphash (lambda(k v) 
                           (funcall func k (car v) (cdr v))) 
                         db)))
             (t (error "Unknown command [%s]" cmd)))))))
    dispatch))

(defsubst skype--cache-get (key db)
  (funcall db 'get (list key)))

(defsubst skype--cache-expire (key db)
  (funcall db 'expire (list key)))

(defsubst skype--cache-clear (db)
  (funcall db 'clear nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skype Low-level API
;;;   (without gui)

(defvar skype--com-debug-mode nil
  "Debug for skype communication port.")

(defun skype--com (message)
  "This function provides the primitive communicate with the Skype API.
If the API returns an error output, the function throws a skype-error signal.
If `skype--com-debug-mode' is non-nil, this function logs I/O into the debug buffer."
  (let ((ret 
         (dbus-call-method 
          :session 
          "com.Skype.API" ; service name
          "/com/Skype"    ; path
          "com.Skype.API" ; interface name
          "Invoke"        ; method name
          message)))
    (when skype--com-debug-mode
      (let* ((buf-name "*skype-com debug*")
             (buf (get-buffer buf-name)))
        (save-excursion
          (unless buf
            (setq buf (get-buffer-create buf-name))
            (buffer-disable-undo buf))
          (set-buffer buf)
          (goto-char (point-max))
          (insert ">> " message "\n")
          (insert "<< " ret "\n"))))
    (if (= (or (string-match "^ERROR [0-9]+" ret) -1) 0)
        (signal 'skype-error (list ret))
      ret)))

(defun skype--com-get (object-name)
  "Return the value that can be get like following 
Skype API command: 
 <- GET (object-name)
 -> (object-name) (value)"
  (let ((ret (skype--com (concat "GET " object-name))))
    (skype--string-trim (substring ret (length object-name)))))

(defun skype--com-get-object-attr (id type attr)
  "Return the value that can be get like following Skype API command:\n
-> GET (type) (id) (attr)\n
<- (type) (id) (attr) (value)\n."
  (skype--com-get (concat type " " id " " attr)))

;; emoticons

(defvar skype--emoticon-map (make-hash-table :test 'equal)
  "[internal use] Hash-table object for emoticons [code -> emoticon object].")

(defvar skype--emoticon-list nil
  "[internal use] Emoticon list for selection. (text . code)")

(defvar skype--emoticon-table
  '(("smile" ":)" ":=)" ":-)")
    ("sadsmile" ":(" ":=(" ":-(")
    ("bigsmile" ":D" ":=D" ":-D" ":d" ":=d" ":-d")
    ("cool" "8)" "8=)" "8-)" "B)" "B=)" "B-)" "(cool)")
    ("wink" ":o" ":=o" ":-o" ":O" ":=O" ":-O")
    ("crying" ";(" ";-(" ";=(")
    ("sweating" "(sweat)" "(:|")
    ("speechless" ":|" ":=|" ":-|")
    ("kiss" ":*" ":=*" ":-*")
    ("tongueout" ":P" ":=P" ":-P" ":p" ":=p" ":-p")
    ("blush" "(blush)" ":$" ":-$" ":=$" ":\">")
    ("wondering" ":^)")
    ("sleepy" "|-)" "I-)" "I=)" "(snooze)")
    ("dull" "|(" "|-(" "|=(")
    ("inlove" "(inlove)")
    ("evilgrin" "]:)" ">:)" "(grin)")
    ("talking" "(talk)")
    ("yawn" "(yawn)" "|-()")
    ("puke" "(puke)" ":&" ":-&" ":=&")
    ("doh" "(doh)")
    ("angry" ":@" ":-@" ":=@" "x(" "x-(" "x=(" "X(" "X-(" "X=(")
    ("itwasntme" "(wasntme)")
    ("party" "(party)")
    ("worried" ":S" ":-S" ":=S" ":s" ":-s" ":=s")
    ("mmm" "(mm)")
    ("nerd" "8-|" "B-|8" "|B" "|8=|" "B=|" "(nerd)")
    ("lipssealed" ":x" ":-x" ":X" ":-X" ":#" ":-#" ":=x" ":=X" ":=#")
    ("hi" "(hi)" "(wave)")
    ("call" "(call)")
    ("devil" "(devil)")
    ("angel" "(angel)")
    ("envy" "(envy)")
    ("wait" "(wait)")
    ("bear" "(bear)" "(hug)")
    ("makeup" "(makeup)" "(kate)")
    ("giggle" "(giggle)" "(chuckle)")
    ("clapping" "(clap)")
    ("thinking" "(think)" ":?" ":-?" ":=?")
    ("bow" "(bow)")
    ("rofl" "(rofl)")
    ("whew" "(whew)")
    ("happy" "(happy)")
    ("smirk" "(smirk)")
    ("nod" "(nod)")
    ("shake" "(shake)")
    ("punch" "(punch)")
    ("emo" "(emo)")
    ("yes" "(y)" "(Y)" "(ok)")
    ("no" "(n)" "(N)")
    ("handshake" "(handshake)")
    ("skype" "(skype)" "(ss)")
    ("heart" "(h)" "(H)" "(l)" "(L)")
    ("brokenheart" "(u)" "(U)")
    ("mail" "(e)" "(m)")
    ("flower" "(f)" "(F)")
    ("rain" "(rain)" "(london)" "(st)")
    ("sun" "(sun)")
    ("time" "(o)" "(O)" "(time)")
    ("music" "(music)")
    ("movie" "(~)" "(film)" "(movie)")
    ("phone" "(mp)" "(ph)")
    ("coffee" "(coffee)")
    ("pizza" "(pizza)" "(pi)")
    ("cash" "(cash)" "(mo)" "($)")
    ("muscle" "(muscle)" "(flex)")
    ("cake" "(^)" "(cake)")
    ("beer" "(beer)")
    ("drink" "(d)" "(D)")
    ("dance" "(dance)" "\\o/" "\\:D/" "\\:d/")
    ("ninja" "(ninja)")
    ("star" "(*)")
    ("mooning" "(mooning)")
    ("middlefinger" "(finger)")
    ("bandit" "(bandit)")
    ("drunk" "(drunk)")
    ("smoke" "(smoking)" "(smoke)" "(ci)")
    ("toivo" "(toivo)")
    ("rock" "(rock)")
    ("headbang" "(headbang)" "(banghead)")
    ("bug" "(bug)")
    ("fubar" "(fubar)")
    ("poolparty" "(poolparty)")
    ("swear" "(swear)")
    ("tmi" "(tmi)")) 
  "Emoticon translation table. (filename text...)")

(defun skype--emoticons-init ()
  (when (file-directory-p skype--emoticon-path)
    (lexical-let
        ((icon-table (make-hash-table :test 'equal))
         (emoticon-map (make-hash-table :test 'equal))
         (emoticon-list nil))
      (dolist (path (directory-files (expand-file-name skype--emoticon-path)))
        (let ((filename (file-name-nondirectory path)))
          (if (string-match "-\\([a-z]+\\)\\.png$" filename)
              (puthash (match-string 1 filename)
                       (create-image
                        (concat 
                         (file-name-as-directory skype--emoticon-path) path)
                        'png nil ':ascent 90)
                       icon-table))))
      (dolist (i skype--emoticon-table)
        (let* ((name (car i))
               (img (gethash name icon-table))
               (codes (cdr i)))
          (dolist (code codes)
            (puthash code img emoticon-map))
          (let ((icon " "))
            (put-text-property 0 1 'display img icon)
            (push (cons (concat (substring name 0)
                                " " icon " "
                                (skype--join codes " "))
                        (car codes))
                  emoticon-list))))
      (setq skype--emoticon-list emoticon-list)
      (setq skype--emoticon-map emoticon-map))))

(defun skype--emoticons-replace (arg)
  "Replace emoticon texts by corresponding icons."
  (let ((buf (get-buffer-create " *skype work*"))
        (text (substring arg 0)) name pos)
    (buffer-disable-undo buf)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert text)
      (maphash
       (lambda (code img)
         (goto-char (point-min))
         (while (setq pos (search-forward code nil t))
           (put-text-property (- pos (length code)) pos
                              'display img)))
       skype--emoticon-map)
      (buffer-string))))

;;; User status

(defvar skype--status-codes (make-hash-table :test 'equal)
  "[internal use] Hash-table object for status codes [code -> status object].")

(defstruct skype-status name code icon)

(defun skype--get-my-status ()
  "Return the current user status code."
  (skype--com-get "USERSTATUS"))

(defun skype--set-my-status (status-code)
  "Set a user status. 
Valid status codes are UNKNOWN, ONLINE, OFFLINE, SKYPEME, 
AWAY, NA, DND, INVISIBLE and LOGGEDOUT."
  (skype--com (concat "SET USERSTATUS " status-code)))

(defun skype--status-init ()
  (clrhash skype--status-codes)
  (dolist (i
           '(("Unknown" "UNKNOWN" "StatusPending")
             ("Online" "ONLINE" "StatusOnline")
             ("Offline" "OFFLINE" "StatusOffline")
             ("Skype me" "SKYPEME" "StatusSkypeMe")
             ("Away" "AWAY" "StatusAway")
             ("Not available" "NA" "StatusNotAvailable")
             ("Do not disturb" "DND" "StatusDoNotDisturb")
             ("Invisible" "INVISIBLE" "StatusInvisible")
             ("Logged out" "LOGGEDOUT" "StatusOffline")))
    (let ((name (car i))
          (code (cadr i))
          (file-header (caddr i)))
      (puthash code (make-skype-status
                     :name name :code code 
                     :icon (append 
                            (create-image 
                             (format "%s/%s_16x16.png" 
                                     skype--icon-path file-header) 'png)
                            '(:ascent 90)))
               skype--status-codes))))

(defun skype--status-get-icon (code)
  (let ((obj (gethash code skype--status-codes))
        (text (substring code 0)))
    (when obj
      (skype--icon text (skype-status-icon obj)))
    text))

;;; mood text

(defun skype--set-my-mood-text (text)
  "Change the your mood text."
  (skype--com (concat "SET PROFILE MOOD_TEXT " 
                      (encode-coding-string text 'utf-8-dos))))

;;; chat message type 

(defvar skype--chatmsg-body-map (make-hash-table :test 'equal)
  "[internal use] translation map for message type.")

(defun skype--init-chatmsg ()
  (dolist
      (i 
       '(("SETTOPIC"           "" "The chat topic was changed: -> ")
         ("SAID"               "said" "")
         ("ADDEDMEMBERS"       "" "Added members.  ")
         ("SAWMEMBERS"         "saw members" "")
         ("CREATEDCHATWITH"    "" "Created a new chat.  ")
         ("LEFT"               "left" "A member was left.  ")
         ("POSTEDCONTACTS"     "" "Posted contacts")
         ("GAP_IN_CHAT"        "gap in chat" "")
         ("SETROLE"            "" "Set role")
         ("KICKED"             "" "Kicked")
         ("KICKBANNED"         "" "Kick banned")
         ("SETOPTIONS"         "" "Set options")
         ("SETPICTURE"         "" "Set picture")
         ("SETGUIDELINES"      "" "Set guidelines")
         ("JOINEDASAPPLICANT"  "" "Joined as applicant")
         ("EMOTED"            "emoted" "---------oOOo--------\n")
         ("UNKNOWN"            "UNKNOWN" "")
         ))
    (puthash (car i) (cdr i) skype--chatmsg-body-map)))

;;; Users

(defun skype--get-group-users (group-handle)
  "Return a list of user-handles those belong to the given group."
  (skype--split (skype--com-get (concat "GROUP " group-handle " USERS"))))

(defun skype--user-get-attr (user-handle attr)
  "Return a property value.
The argument USER-HANDLE can be `skype-user' object."
  (skype--com-get-object-attr user-handle "USER" attr))

(defstruct skype-user 
  handle fullname mood-text display-name status 
  t-name t-viewtext)

(defvar skype--user-object-cache 
  (skype--cache-init-db 
   (lambda(key) (skype--user-create-object key)) 300)
  "[internal use] user object cache.")

(defun skype--user-create-object (user-handle &optional last-obj)
  "Build the user object from the given user-handle."
  (if last-obj
      (progn
        (setf (skype-user-mood-text last-obj) 
              (skype--user-get-attr user-handle "MOOD_TEXT"))
        (setf (skype-user-status last-obj)
              (skype--user-get-attr user-handle "ONLINESTATUS"))
        (setf (skype-user-t-viewtext last-obj) nil)
        last-obj)
    (make-skype-user 
     :handle user-handle
     :fullname (skype--user-get-attr user-handle "FULLNAME")
     :mood-text (skype--user-get-attr user-handle "MOOD_TEXT")
     :display-name (skype--user-get-attr user-handle "DISPLAYNAME")
     :status (skype--user-get-attr user-handle "ONLINESTATUS"))))

(defsubst skype--user-handle-to-object (handle)
  (if (skype-user-p handle)
      handle
    (skype--cache-get handle skype--user-object-cache)))

(defun skype--user-get-name (user)
  "Return the appropriate user name from the given user object."
  (labels ((sn (str) (if (or (null str) (= (length str) 0)) nil str)))
    (let* ((obj (skype--user-handle-to-object user))
           (name (skype-user-t-name obj)))
      (if name name ; return
        (setq name 
              (or (sn (skype-user-display-name obj))
                  (sn (skype-user-fullname obj))
                  (sn (skype-user-handle obj))))
        (setf (skype-user-t-name obj) name)
        name))))
  
(defun skype--user-get-viewtext (user)
  "Return the user name, status icon and mood-text."
  (let* ((obj (skype--user-handle-to-object user))
         (viewtext (skype-user-t-viewtext obj)))
    (if viewtext viewtext ; return
      (let* ((name (skype--user-get-name obj))
             (status-obj (gethash (skype-user-status obj) skype--status-codes))
             (status-name (concat "[" (substring (skype-status-name status-obj) 0) "]"))
             (mood-text (skype-user-mood-text obj)))
        (setq viewtext
              (concat name " " 
                      (skype--icon status-name (skype-status-icon status-obj))
                      (if (> (length mood-text) 0) 
                          (concat " < " mood-text))))
        (setf (skype-user-t-viewtext obj) viewtext)
        viewtext))))

(defun skype--get-all-contact-users (&optional group-type)
  "Return a list of user handles those belong to the ALL_FRIENDS group."
  (let ((c-group-type (or group-type "ALL_FRIENDS"))
        (groups (skype--get-all-groups)))
    (if (listp groups)
        (lexical-let 
            ((friend-group 
              (some (lambda (gid) 
                      (and (string= 
                            c-group-type
                            (skype--com-get-object-attr gid "GROUP" "TYPE"))
                           gid))
                    groups)))
          (if friend-group
              (skype--get-group-users friend-group))))))

;;; User actions

(defun skype--start-chat (user-handle)
  "Start a skype chat and return the chat-handle.
The argument USER-HANDLE can be `skype-user' object."
  (when (skype-user-p user-handle)
    (setq user-handle (skype-user-handle user-handle)))
  (let ((ret (skype--com (concat "CHAT CREATE " user-handle))))
    (if (string-match "^CHAT \\(.+\\) STATUS DIALOG" ret)
        (match-string-no-properties 1 ret))))

(defun skype--start-call (user-handle)
  "Start a skype call and return the call-handle.
The argument USER-HANDLE can be `skype-user' object."
  (when (skype-user-p user-handle)
    (setq user-handle (skype-user-handle user-handle)))
  (let ((ret (skype--com (concat "CALL " user-handle))))
    (if (string-match "^CALL \\([^ ]+\\) " ret)
        (match-string-no-properties 1 ret))))

;;; Groups

(defun skype--get-all-groups ()
  "Return a list of all group handles."
  (let ((ret (skype--com "SEARCH GROUPS ALL")))
    (skype--split (substring ret 7))))

;;; Chats

(defvar skype--chat-object-cache 
  (skype--cache-init-db
   (lambda (key) (skype--chat-create-object key))
   (* 15 60))
  "[internal use] chat object cache.")

(defstruct skype-chat chat-handle title time)

(defun skype--chat-handle-to-object (handle)
  (if (skype-chat-p handle)
      handle
    (let ((obj (skype--cache-get handle skype--chat-object-cache)))
      obj)))

(defun skype--chat-create-object (chat-handle)
  "Build a chat object from the chat that is indicated by the given chat-handle."
  (make-skype-chat
   :chat-handle chat-handle 
   :title (skype--chat-get-attr chat-handle "FRIENDLYNAME")
   :time (skype--chat-get-last-timestamp chat-handle)))

(defun skype--chat-get-last-timestamp (chat-handle)
  "Return last chat message time as float seconds."
  (skype--convert-from-skype-time 
   (skype--chat-get-attr chat-handle "ACTIVITY_TIMESTAMP")))

(defun skype--chat-get-attr (chat-handle attr)
  "Return a property value."
  (skype--com-get-object-attr chat-handle "CHAT" attr))

(defun skype--chat-get-buffername (chat-handle)
  (let ((obj (skype--chat-handle-to-object chat-handle)))
    (concat "SkypeChat:[" (skype-chat-title obj) "]")))

(defun skype--chat-get-recent-objects ()
  "Return a list of recent chat objects."
  (skype--chat-get-objects "RECENTCHATS"))

(defun skype--chat-get-bookmarked-objects ()
  "Return a list of bookmarked chat objects."
  (skype--chat-get-objects "BOOKMARKEDCHATS"))

(defun skype--chat-get-missed-objects ()
  "Return a list of missed chat objects."
  (skype--chat-get-objects "MISSEDCHATS"))

(defun skype--chat-get-active-objects ()
  "Return a list of active chat objects."
  (skype--chat-get-objects  "ACTIVECHATS"))

(defun skype--chat-get-objects (search-item)
  "Return a list of chat objects those are indicated by the given keyword.
This function is used by `skype--chat-get-recent-objects', `skype--chat-get-bookmarked-objects',
`skype--chat-get-missed-objects' and `skype--chat-get-active-objects'."
  (let ((ret (skype--com (concat "SEARCH " search-item))))
    (if (string-match "^CHATS #" ret)
        (mapcar 'skype--chat-handle-to-object
                (skype--split (substring ret 5)))
      nil)))

;;; Chat actions

(defun skype--chat-send-message (chat-handle text)
  "Send a chat message.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (skype--com 
   (concat "CHATMESSAGE " chat-handle " " 
           (encode-coding-string 
            (if skype--chat-send-message-function 
                (funcall skype--chat-send-message-function
                         (skype--chat-handle-to-object chat-handle) text)
              text)
            'utf-8-dos))))

(defvar skype--chat-send-message-function nil
  "Abnormal hook for sending a message with two argument,
the skype-chat object and the sending text message. The returned
text is sent to the skype.") ; TODO test

(defun skype--chat-set-topic (chat-handle topic)
  "Set a chat title.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (skype--com (concat "ALTER CHAT " chat-handle " SETTOPIC " 
                      (encode-coding-string topic 'utf-8-dos))))

(defun skype--chat-add-member (chat-handle new-member)
  "Add a member into to a chat.
The argument CHAT-HANDLE can be `skype-chat' object.
The argument NEW-MEMBER is user-handle or `skype-user' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (when (skype-user-p new-member)
    (setq new-member (skype-user-handle new-member)))
  (skype--com (concat "ALTER CHAT " chat-handle " ADDMEMBERS " new-member)))

(defun skype--chat-leave (chat-handle)
  "Leave the chat.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (skype--com (concat "ALTER CHAT " chat-handle " LEAVE")))

(defun skype--chat-missed-p (chat-handle)
  "Return non-nil, if missed-flag for the chat is true."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (some (lambda (c) (string= chat-handle (skype-chat-chat-handle c)))
        (skype--chat-get-missed-objects)))

(defun skype--chat-clear-missed-flag (chat-handle)
  "Set a seen flag for all recent chat messages."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (dolist (i (skype--chat-get-recent-chatmsg-handles chat-handle))
    (skype--chatmsg-clear-missed-flag i)))

(defun skype--chat-bookmarked-p (chat-handle)
  "Return non-nil, if the chat is bookmarked."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (string= "TRUE" (skype--chat-get-attr chat-handle "BOOKMARKED")))
  
(defun skype--chat-bookmark (chat-handle)
  "Add a chat to the list of bookmarked chats.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (skype--com (concat "ALTER CHAT " chat-handle " BOOKMARK")))

(defun skype--chat-unbookmark (chat-handle)
  "Remove a chat from the list of bookmarked chats.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (skype--com (concat "ALTER CHAT " chat-handle " UNBOOKMARK")))

(defun skype--chat-toggle-bookmark (chat-handle)
  "Toggle the bookmark state of the chat.
If the chat changes to bookmarked, return t.
If unbookmarked, return nil.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (if (skype--chat-bookmarked-p chat-handle)
      (progn
        (skype--chat-unbookmark chat-handle)
        nil)
    (skype--chat-bookmark chat-handle)
    t))

(defun skype--chat-get-members (chat-handle)
  "Return handles of chat members who participates in the chat."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (split-string (skype--chat-get-attr chat-handle "MEMBERS") " "))

(defun skype--chat-members-get-buffername (chat-handle)
  (let ((obj (skype--chat-handle-to-object chat-handle)))
    (format "Skype Chat members:[%s]" 
            (skype-chat-title obj))))

;;; ChatMessages

;; t-state: 'update 'new 'unchange
(defstruct skype-chatmsg handle from-handle time status type body t-state)

(defun skype--chatmsg-create-object (handle last-object)
  "Build a chatmsg object from the given chatmsg-handle and
register the object to the local variable `skype-chatmsg-table'."
  (let* 
      ((last-ftime (float-time skype-last-updated-time))
       (edited-ftime (string-to-number (skype--chatmsg-get-attr handle "EDITED_TIMESTAMP")))
       (last-status (and last-object (skype-chatmsg-status last-object)))
       new-object)
    (cond
     (last-object ; 1 cached object
      (cond
       ((memq last-status '(received sending)) ; 1-1 state change
        (setf (skype-chatmsg-status last-object)
              (skype--chatmsg-get-status handle))
        (setf (skype-chatmsg-t-state last-object) 
              (if (eq last-status (skype-chatmsg-status last-object))
                  'unchange 'update)))
       ((< last-ftime edited-ftime) ; 1-2 edited
        (setf (skype-chatmsg-body last-object)
              (skype--chatmsg-get-body handle (skype-chatmsg-type last-object)))
        (setf (skype-chatmsg-t-state last-object) 'update))
       (t ; 1-3 unchange
        (setf (skype-chatmsg-t-state last-object) 'unchange)))
      (setq new-object last-object))
     (t ; 2 new object
      (let ((type (skype--chatmsg-get-attr handle "TYPE")))
        (setq new-object
              (make-skype-chatmsg
               :handle handle
               :from-handle (skype--chatmsg-get-attr handle "FROM_HANDLE")
               :time (skype--convert-from-skype-time
                      (skype--chatmsg-get-attr handle "TIMESTAMP"))
               :status (skype--chatmsg-get-status handle)
               :type type
               :body (skype--chatmsg-get-body handle type)
               :t-state 'new)))
      (puthash handle new-object skype-chatmsg-table)))
    new-object))

(defun skype--chatmsg-handle-to-object (handle)
  "Return a chatmsg object. This function uses the buffer local
hash-table `skype-chatmsg-table' and time-stamp
`skype-last-updated-time'."
  (skype--chatmsg-create-object handle (skype--chatmsg-get-cache-object handle)))

(defsubst skype--chatmsg-get-cache-object (handle)
  (gethash handle skype-chatmsg-table))

(defsubst skype--chatmsg-get-attr (chat-handle attr)
  "Return a property value."
  (skype--com-get-object-attr chat-handle "CHATMESSAGE" attr))

(defsubst skype--convert-from-skype-time (str)
  "Return a float time value from a string time value 
of the skype API."
  (seconds-to-time (string-to-number str)))

(defun skype--strtime (time)
  (if (equal (cdddr (decode-time time))
             (cdddr (decode-time (current-time))))
      (format-time-string "%H:%M:%S" time)
    (format-time-string "%Y/%m/%d %H:%M:%S" time)))

(defun skype--chatmsg-get-body (message-handle type)
  (concat 
   (cadr (gethash type skype--chatmsg-body-map))
   (skype--chatmsg-get-attr message-handle "BODY")))

(defun skype--chatmsg-get-status (message-handle)
  (let ((str (skype--chatmsg-get-attr message-handle "STATUS")))
    (intern (downcase str))))

(defun skype--chat-get-chatmsgs (chat-handle)
  "(Not work because skype API...) Return a list of chat message
handles those belong to the chat that is indicated by the given
chat-handle.  The argument CHAT-HANDLE can be `skype-chat'
object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (let ((line (skype--chat-get-attr chat-handle "CHATMESSAGES")))
    (if line (skype--split line))))

(defun skype--chat-get-recent-chatmsg-handles (chat-handle)
  "Return a list of recent chat message handles those belong to 
the chat that is indicated by the given chat-handle.
The argument CHAT-HANDLE can be `skype-chat' object."
  (when (skype-chat-p chat-handle)
    (setq chat-handle (skype-chat-chat-handle chat-handle)))
  (let ((line (skype--chat-get-attr chat-handle "RECENTCHATMESSAGES")))
    (if line (skype--split line))))

(defun skype--chatmsg-mine-p (message-handle)
  "Return non-nil if the given message is sent by you."
  (let ((user-handle 
         (if (skype-chatmsg-p message-handle) 
             (skype-chatmsg-from-handle message-handle)
           (skype--chatmsg-get-attr message-handle "FROM_HANDLE"))))
    (string= user-handle skype--my-user-handle)))

(defun skype--chatmsg-clear-missed-flag (message-handle)
  "Set a seen flag for the chat message."
  (skype--com (concat "SET CHATMESSAGE " message-handle " SEEN")))

(defun skype--chatmsg-get-buffername (chat-obj)
  (format "SkypeMessage:[%s]" (skype-chat-title chat-obj)))

(defun skype--chatmsg-edit-message (chatmsg-handle text)
  "Edit a chat message."
  (skype--com (concat "SET CHATMESSAGE " chatmsg-handle " BODY " 
                      (encode-coding-string text 'utf-8-dos))))

;; initialize

(defun skype--init ()
  "Initialize the Skype API connection."
  (interactive)
  (if (not (dbus-ping :session "com.Skype.API"))
      nil
    (skype--com "NAME emacs23-dbus")
    (skype--com "PROTOCOL 5")
    (skype--status-init)
    (skype--emoticons-init)
    (skype--init-chatmsg)
    (skype--cache-clear skype--chat-object-cache)
    (skype--cache-clear skype--user-object-cache)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skype High-level API
;;;   commands and gui

(defvar skype--default-auto-read-state nil
  "If this variable is non-nil, the function
`skype--update-chat-buffer' makes the state of incoming chat
messages read state automatically." )

(defun skype--open-chat-buffer (chat-handle)
  "Create and switch to the skype chat buffer. 
This function create some buffer-local variables
`skype-chat-handle' and `skype-last-updated-time'."
  (let* ((chat-obj 
          (if (skype-chat-p chat-handle) chat-handle
            (skype--chat-handle-to-object chat-handle)))
         (buf (get-buffer-create (skype--chat-get-buffername chat-obj))))
    (save-excursion
      (set-buffer buf)
      (unless (eq major-mode 'skype--chat-mode)
        (skype--chat-mode)
        (make-local-variable 'skype-chat-handle)
        (make-local-variable 'skype-last-updated-time)
        (make-local-variable 'skype-chatmsg-table)
        (make-local-variable 'skype-auto-read-state)
        (setq skype-chat-handle (skype-chat-chat-handle chat-obj)
              skype-last-updated-time nil
              skype-auto-read-state skype--default-auto-read-state
              skype-chatmsg-table (make-hash-table :test 'equal))
        (buffer-disable-undo buf)
        (skype--chat-mode-update-mode-line))
      (skype--timer-start-update)
      (skype--update-chat-buffer buf))
    buf))

(defun skype--chat-buffer-p (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (eq 'skype--chat-mode 
      (buffer-local-value 'major-mode buffer)))

(defun skype--update-chat-buffer (&optional buffer)
  "Getting recent chat messages, update the current buffer.
If the current buffer does not have the local-variable
`skype-chat-handle', do nothing. Return number of updated chat
messages."
  (let ((update-num 0))
    (save-current-buffer
      (when buffer (set-buffer buffer))
      (setq buffer-read-only nil)
      (unwind-protect
          (when (skype--chat-buffer-p)
            (let* ((chat-obj (skype--chat-handle-to-object skype-chat-handle))
                   (msg-handles (skype--chat-get-recent-chatmsg-handles chat-obj))
                   (last-ftime (and skype-last-updated-time 
                                    (float-time skype-last-updated-time)))
                   (cftime (current-time)) (last-user "")
                   (last-point (point)) (last-eobp (eobp)))
              ;; update chat buffer
              (unless skype-last-updated-time
                (erase-buffer))
              (dolist (msg (mapcar 'skype--chatmsg-handle-to-object msg-handles))
                (when (or (null skype-last-updated-time)
                          (not (eq (skype-chatmsg-t-state msg) 'unchange)))
                  (skype--update-chat-buffer-one
                   msg (string= last-user (skype-chatmsg-from-handle msg)))
                  (incf update-num))
                (setq last-user (skype-chatmsg-from-handle msg)))
              ;; change chatmsg status 
              (when skype-auto-read-state
                (maphash (lambda(key val)
                           (if (eq 'received (skype-chatmsg-status val))
                               (skype--chatmsg-clear-missed-flag key)))
                         skype-chatmsg-table))
              ;; point move
              (if last-eobp (goto-char (point-max))
                (goto-char last-point)
                (if (< 0 update-num) ; not smart
                    (beginning-of-line)))
              (setq skype-last-updated-time cftime)))
        (setq buffer-read-only t)) ; <- finally
      (skype--chat-mode-update-mode-line))
    update-num))

(defun skype--update-chat-buffer-one (chatmsg-object &optional same-user)
  "Search the chat message region and replace the new text."
  (let ( (pos (point-min)) 
         (hmsg (skype-chatmsg-handle chatmsg-object))
         begin end )
    (if (eq 'new (skype-chatmsg-t-state chatmsg-object))
        ;; new
        (goto-char (point-max))
      ;; update
      (loop do
            (when (string= 
                   hmsg
                   (get-text-property 
                    pos 'skype-message-handle))
              (setq begin pos)
              (return))
            (setq pos (next-single-property-change
                       pos 'skype-message-handle))
            (unless pos (return)))
      (if begin
          (progn
            (setq end 
                  (or (next-single-property-change 
                       pos 'skype-message-handle)
                      (point-max)))
            (delete-region begin end)
            (goto-char begin))
        (goto-char (point-max))))
    (insert (skype--layout-chatmsg skype-chat-handle chatmsg-object same-user))))

(defface skype--face-my-time-field '((t (:foreground "chocolate4"))) 
  "Face for time field of chat messages." :group 'skype)
(defface skype--face-other-time-field '((t (:foreground "dark slate blue"))) 
  "Face for time field of chat messages." :group 'skype)
(defface skype--face-user-field '((t (:weight bold)))
  "Face for user field of chat messages." :group 'skype)
(defface skype--face-optional-field '((t (:foreground "dark sea green")))
  "Face for optional field of chat messages." :group 'skype)
(defface skype--face-my-message '((t (:background "khaki")))
  "Face for header line of my chat messages." :group 'skype)
(defface skype--face-other-message '((t (:background "LightSkyBlue1")))
  "Face for header line of others' chat messages." :group 'skype)

(defvar skype--layout-chatmsg-function nil
  "Abnormal hook for layouting a message with two argument,
 the skype-chat object and the received message object. The
returned text is shown in a buffer.") ; TODO test

(defun skype--layout-chatmsg (chat-handle msg &optional same-user)
  "Return a layouted text for one chat message.
If the variable SAME-USER is non-nil, the user name is omitted."
  (let*
      ((hmsg (skype-chatmsg-handle msg))
       (time-field
        (format "# %19s"
                (skype--strtime (skype-chatmsg-time msg))))
       (user-field
        (if same-user "..."
          (skype--user-get-name
           (skype-chatmsg-from-handle msg))))
       (type-field
        (car (or (gethash (skype-chatmsg-type msg)
                          skype--chatmsg-body-map)
                 (cons (skype-chatmsg-type msg) nil))))
       (status-field
        (symbol-name (skype-chatmsg-status msg)))
       (edited-time
        (skype--chatmsg-get-attr hmsg "EDITED_TIMESTAMP"))
       (edited-field
        (if (string= "0" edited-time) ""
          (concat " / Edited at " (skype--strtime 
                                (skype--convert-from-skype-time 
                                 edited-time)))))
       (optional-field 
        (concat type-field " " status-field edited-field))
       (message-field
        (skype--emoticons-replace 
         (if skype--layout-chatmsg-function
             (skype--layout-chatmsg-function 
              (skype--chat-handle-to-object chat-handle) msg)
           (skype-chatmsg-body msg))))
       (line-bgcolor (if (skype--chatmsg-mine-p msg)
                         'skype--face-my-message
                       'skype--face-other-message))
       (face-time-field (if (skype--chatmsg-mine-p msg)
                            'skype--face-my-time-field
                          'skype--face-other-time-field))
       (sep " | ") (sep-num (length sep))
       line total pos-start pos-end)
    (setq line (concat time-field sep user-field sep optional-field "\n"))
    (add-text-properties 0 (length line) 
                         (list 'face line-bgcolor) line)
    (setq pos-start 0
          pos-end   (length time-field))
    (put-text-property pos-start pos-end 
                       'face (list line-bgcolor face-time-field) line)
    (setq pos-start (+ sep-num pos-end)  
          pos-end   (+ pos-start (length user-field)))
    (put-text-property pos-start pos-end 
                       'face (list line-bgcolor 'skype--face-user-field) line)
    (setq pos-start (+ sep-num pos-end)
          pos-end   (+ pos-start (length optional-field)))
    (put-text-property pos-start pos-end 
                       'face (list line-bgcolor 'skype--face-optional-field) line)
    (setq total (concat line message-field "\n\n"))
    (put-text-property 0 (length total)
                       'skype-message-handle hmsg
                       total)
    total))

(defvar skype--completing-read-function 'completing-read
  "Completing function.")

(defun skype--completing-read (prompt src value-mapper &optional init-value)
  "Return a value that the user choose from src-map keys.
PROMPT is a prompt string. SRC is a hash-table object or a list.
VALUE-MAPPER is a function that accepts one or two arguments and
returns a cons cell (TITLE . RETURN-VALUE).
INIT-VALUE can be nil or an initial value."
  (let* ((result-hash (make-hash-table :test 'equal))
         (status-list
          (let (alist)
            (cond
             ((listp src) ; list
              (dolist (value src)
                (let* ((pair
                        (if value-mapper
                            (funcall value-mapper value)
                          (cons value value)))
                       (name (car pair))
                       (content (cdr pair)))
                  (when pair
                    (puthash name content result-hash)
                    (push name alist)))))
             ((hash-table-p src) ; hash-table
              (maphash 
               (lambda (key value) 
                 (let* ((pair 
                         (if value-mapper
                             (funcall value-mapper key value)
                           (cons key value)))
                        (name (car pair))
                        (content (cdr pair)))
                   (when pair
                     (puthash name content result-hash)
                     (push name alist))))
               src))
             (t (error "src is not a list or a hash-table. %s" src)))
            alist))
         (completion-ignore-case t)
         (ret (funcall skype--completing-read-function prompt status-list nil t init-value)))
    (gethash ret result-hash)))

(defun skype--set-my-status-command ()
  (interactive)
  (let ((status (skype--completing-read
                 "Skype Status: " skype--status-codes 
                 (lambda (key value)
                   (let ((text (skype-status-name value))
                         (icon (skype--icon " " (skype-status-icon value))))
                     (if (string= key "UNKNOWN") nil
                       (cons (concat text icon) key)))))))
    (when status
      (skype--set-my-status status))))

(defun skype--select-user (prompt)
  "Using completing-read function, select an user from all
contact users and return an user handle."
  (skype--completing-read
   prompt
   (mapcar 'skype--user-handle-to-object 
           (skype--get-all-contact-users))
   (lambda (u)
     (cons (concat (skype--user-get-viewtext u) 
                   " (" (skype-user-handle u) ")")
           u))))

(defun skype--open-chat-buffer-gen (prompt function)
  (let ((chats (funcall function)) chat)
    (cond
     (chats
      (setq chat (skype--completing-read
                  prompt chats
                  (lambda (c)
                    (cons (skype-chat-title c) c))))
      (when chat
        (switch-to-buffer (skype--open-chat-buffer chat))))
     (t 
      (message "Chat not found.")))))

(defun skype--open-recent-chat-buffer-command ()
  (interactive)
  (skype--open-chat-buffer-gen "Skype Recent Chat: " 'skype--chat-get-recent-objects) )

(defun skype--open-bookmarked-chat-buffer-command ()
  (interactive)
  (skype--open-chat-buffer-gen "Skype Bookmarked Chat: " 'skype--chat-get-bookmarked-objects) )

(defun skype--open-missed-chat-buffer-command ()
  (interactive)
  (skype--open-chat-buffer-gen "Skype Missed Chat: " 'skype--chat-get-missed-objects) )

(defun skype--update-chat-buffer-command ()
  (interactive)
  (skype--update-chat-buffer))

(defun skype--refresh-chat-buffer-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (setq skype-last-updated-time nil)
    (skype--update-chat-buffer)))

(defun skype--open-new-chat-command (user-handle)
  (interactive "P")
  (let ((target-user
         (or user-handle 
             (skype--select-user "Open New Chat for... : "))))
    (when target-user
      (switch-to-buffer
       (skype--open-chat-buffer 
        (skype--start-chat target-user))))))

(defun skype--set-my-mood-text-command (text)
  (interactive "MMood Text: ")
  (skype--set-my-mood-text text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse chat messages mode

(defun skype--chat-mode-next-command ()
  (interactive)
  (let ((next-pos
         (or 
          (next-single-property-change 
           (point) 'skype-message-handle (current-buffer))
          (point-max))))
    (goto-char next-pos)))

(defun skype--chat-mode-prev-command ()
  (interactive)
  (let ((prev-pos
         (or 
          (previous-single-property-change 
           (point) 'skype-message-handle (current-buffer))
          (point-min))))
    (goto-char prev-pos)))

(defvar skype--chatmsg-window-height 10
  "Window height for skype chat message.")

(defun skype--chat-mode-get-selected-message ()
  "Return selected message text."
  (save-excursion
    (let ((start-pos 
            (if (or (bobp) 
                    (not (string= 
                          (get-text-property (point) 'skype-message-handle)
                          (get-text-property (1- (point)) 'skype-message-handle))))
                (point) ; head of a message
              (or
               (previous-single-property-change 
                (point) 'skype-message-handle (current-buffer))
               (point-min))))
          (end-pos
           (or 
            (next-single-property-change 
             (point) 'skype-message-handle (current-buffer))
            (point-max))))
      (skype--string-trim 
       (buffer-substring-no-properties start-pos end-pos)))))

(defun skype--chat-mode-copy-command ()
  (interactive)
  (kill-new (skype--chat-mode-get-selected-message)))

(defun skype--chat-mode-quit-command ()
  (interactive)
  (bury-buffer))

(defun skype--kill-chat-buffer-command ()
  (interactive)
  (let ((msgbuf 
         (get-buffer
          (skype--chatmsg-get-buffername
           (skype--chat-handle-to-object skype-chat-handle))))
        kill-ng)
    (when msgbuf 
      (save-current-buffer
        (set-buffer msgbuf)
        (when (< 0 (length 
                    (skype--string-trim (buffer-string))))
          (setq kill-ng (not (yes-or-no-p "There is some text. Kill anyway?"))))
        (unless kill-ng
          (skype--message-mode-quit-command))))
    (unless kill-ng
      (kill-buffer))))

(defun skype--chat-mode-reply-command ()
  (interactive)
  (let ((text (skype--chat-mode-get-selected-message)))
    (skype--chat-mode-message-command 
     (mapconcat
      (lambda (i) (concat "> " i))
      (split-string text "\n") "\n"))))

(defun skype--chat-mode-toggle-bookmark-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (message 
     (if (skype--chat-toggle-bookmark skype-chat-handle)
         "SkypeChat: %s bookmarked." "SkypeChat: %s unbookmarked.")
     (skype-chat-title (skype--chat-handle-to-object skype-chat-handle)))
    (skype--chat-mode-update-mode-line)))

(defun skype--chat-mode-toggle-auto-read-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (setq skype-auto-read-state (not skype-auto-read-state))
    (message
     (if skype-auto-read-state
         "SkypeChat: %s auto read ON" "SkypeChat: %s auto read OFF")
     (skype-chat-title (skype--chat-handle-to-object skype-chat-handle)))
    (skype--chat-mode-update-mode-line)))

(defun skype--chat-mode-add-chat-member-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (let ((target-user
           (skype--select-user "Add chat member: ")))
      (when target-user
        (skype--chat-add-member
         skype-chat-handle target-user)))))

(defun skype--chat-mode-set-chat-topic-command (text)
  (interactive "MChat topic: ")
  (when (skype--chat-buffer-p)
    (skype--chat-set-topic skype-chat-handle text)))

(defun skype--chat-mode-leave-chat-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (if (yes-or-no-p "Leave this chat? ")
        (skype--chat-leave skype-chat-handle))))

(defun skype--chat-mode-clear-missed-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (skype--chat-clear-missed-flag skype-chat-handle)
    (skype--update-chat-buffer)))

(defun skype--chat-mode-message-command (&optional initial-text)
  (interactive "P")
  (when (skype--chat-buffer-p)
    (skype--chat-mode-edit-message-gen 
     "[new]"
     (lambda (text)
       (skype--chat-send-message skype-chat-handle text)
       'keep-buffer)
     initial-text)))

(defun skype--chat-mode-edit-message-command ()
  (interactive)
  (when (skype--chat-buffer-p)
    (lexical-let
        ((chatmsg-handle (get-text-property (point) 'skype-message-handle)))
      (skype--chat-mode-edit-message-gen 
       "[re-edit]"
       (lambda (text)
         (skype--chatmsg-edit-message chatmsg-handle text)
         'kill-buffer)
       (skype--chat-mode-get-selected-message)))))

(defun skype--chat-mode-edit-message-gen (prompt commit-function initial-text)
  (let* ((chat-buf (current-buffer))
         (chat-obj (skype--chat-handle-to-object skype-chat-handle))
         (buf-name (skype--chatmsg-get-buffername chat-obj))
         (buf (get-buffer-create buf-name))
         (send-history skype-send-history) msg-window)
      (save-excursion
        (set-buffer buf)
        (skype--message-mode)
        (make-local-variable 'skype-chat-handle)
        (make-local-variable 'skype-chat-buffer)
        (make-local-variable 'skype-send-history)
        (make-local-variable 'skype-commit-function)
        (make-local-variable 'skype-mode-line-prompt)
        (setq skype-chat-handle (skype-chat-chat-handle chat-obj)
              skype-chat-buffer chat-buf
              skype-send-history send-history
              skype-commit-function commit-function
              skype-mode-line-prompt prompt)
        (force-mode-line-update)
        (when initial-text (insert initial-text)))
      (setq msg-window (or (get-buffer-window buf)
                           (split-window (get-buffer-window chat-buf))))
      (goto-char (point-max))
      (pop-to-buffer buf msg-window)
      (if (< skype--chatmsg-window-height (window-height msg-window))
          (shrink-window (- (window-height msg-window) skype--chatmsg-window-height)))))

(defvar skype--chat-mode-hook nil
  "Hook for `skype--chat-mode'")

(defvar skype--chat-mode-map nil
  "Keymap for `skype--chat-mode'.")
(setq skype--chat-mode-map nil) ; for debug
(unless skype--chat-mode-map
  (setq skype--chat-mode-map (make-sparse-keymap))
  (mapc (lambda (i)
          (define-key skype--chat-mode-map 
            (car i) (cdr i)))
          '(("n" . skype--chat-mode-next-command)
            ("j" . skype--chat-mode-next-command)
            ("p" . skype--chat-mode-prev-command)
            ("k" . skype--chat-mode-prev-command)
            ("a" . skype--chat-mode-message-command)
            ("c" . skype--chat-mode-copy-command)
            ("q" . skype--chat-mode-quit-command)
            ("u" . skype--update-chat-buffer-command)
            ("U" . skype--refresh-chat-buffer-command)
            ("Q" . skype--kill-chat-buffer-command)
            ("A" . skype--chat-mode-reply-command)
            ("B" . skype--chat-mode-toggle-bookmark-command)
            ("R" . skype--chat-mode-toggle-auto-read-command)
            ("+" . skype--chat-mode-add-chat-member-command)
            ("L" . skype--chat-mode-leave-chat-command)
            ("t" . skype--chat-mode-set-chat-topic-command)
            ("C" . skype--chat-mode-clear-missed-command)
            ("E" . skype--chat-mode-edit-message-command)
            ("l" . skype--open-chat-member-buffer-command)

            ("m" . skype--open-new-chat-command)
            ("M" . skype--set-my-mood-text-command)
            )))

(defvar skype--chat-mode-line-format 
  '("-" mode-line-mule-info "  %20b" 
    "    " skype-mode-line-chat-info
    "    %l,%c " mode-line-modes "%-")
  "Mode line format for skype chat mode.")

(defun skype--chat-mode-update-mode-line ()
  "Update the local variable `skype-mode-line-chat-info' and 
update the mode line."
  (when (skype--chat-buffer-p)
    (setq skype-mode-line-chat-info 
          (concat 
           (if (skype--chat-bookmarked-p skype-chat-handle)
               "<Bookmarked>" nil)
           (if (skype--chat-missed-p skype-chat-handle)
               "<Missed>" nil)
           (if skype-auto-read-state
               "<AutoRead>" nil)
           ))
    (force-mode-line-update)))

(defun skype--chat-mode ()
  "Just prepare the major mode. In this function, the local variable 
`skype-chat-handle' can not be used."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'skype-send-history)
  (make-local-variable 'skype-mode-line-chat-info)
  (setq major-mode 'skype--chat-mode
        mode-name "Skype-Chat"
        mode-line-format skype--chat-mode-line-format
        skype-send-history nil)
  (use-local-map skype--chat-mode-map)
  (setq paragraph-start "^# ")
  (run-hooks 'skype--chat-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writing a chat message mode

(defvar skype--message-send-history-size 5
  "Size of the send history.")

(defvar skype--message-mode-hook nil
  "Hook for `skype--message-mode'")

(defvar skype--message-mode-line-format 
  '("-" mode-line-mule-info "  %20b " skype-mode-line-prompt 
    "   %l,%c " mode-line-modes "%-")
  "Mode line format for skype message mode.")

(defvar skype--message-mode-map nil
  "Keymap for skype--message-mode.")
(setq skype--message-mode-map nil) ; for debug
(unless skype--message-mode-map
  (setq skype--message-mode-map (make-sparse-keymap))
  (mapc (lambda (i)
            (define-key skype--message-mode-map
              (car i) (cdr i)))
          '(("\C-c\C-c" . skype--message-mode-send-command)
            ("\C-c\C-q"    . skype--message-mode-quit-command)
            ("\C-c\C-e"    . skype--message-mode-emoticon-command)
            ("\C-c\C-p"    . skype--message-mode-prev-message-command)
            ("\C-c\C-n"    . skype--message-mode-next-message-command)
            ([?\C-\S-b]    . skype--message-mode-chat-scroll-bottom-command)
            ([?\C-\S-p]    . skype--message-mode-chat-scroll-down-command)
            ([?\C-\S-n]    . skype--message-mode-chat-scroll-up-command)
            ([?\C-\S-j]    . skype--message-mode-chat-scroll-down-command)
            ([?\C-\S-k]    . skype--message-mode-chat-scroll-up-command)

            ([?\C-\S-q]    . skype--message-mode-chat-quit-command)
            ([?\C-\S-c]    . skype--message-mode-chat-clear-missed-command)
            )))

(defun skype--message-mode-chat-quit-command ()
  (interactive)
  (let ((main-buf skype-chat-buffer))
    (skype--message-mode-quit-command)
    (set-buffer main-buf)
    (skype--kill-chat-buffer-command)))

(defun skype--message-mode-chat-clear-missed-command ()
  (interactive)
  (save-current-buffer 
    (set-buffer skype-chat-buffer)
    (skype--chat-mode-clear-missed-command)))

(defun skype--message-buffer-p (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (eq 'skype--message-mode (buffer-local-value 'major-mode buffer)))

(defun skype--message-mode-chat-scroll-bottom-command ()
  (interactive)
  (when (skype--message-buffer-p)
    (let ((win (get-buffer-window skype-chat-buffer)))
      (save-current-buffer 
        (set-buffer skype-chat-buffer)
        (set-window-point win (point-max))))))

(defun skype--message-mode-chat-scroll-down-command ()
  (interactive)
  (let ((other-window-scroll-buffer skype-chat-buffer))
    (scroll-other-window 3)))

(defun skype--message-mode-chat-scroll-up-command ()
  (interactive)
  (let ((other-window-scroll-buffer skype-chat-buffer))
    (scroll-other-window -3)))

(defun skype--message-mode-send-command ()
  (interactive)
  (let ((text (buffer-string))
        (chat-buf skype-chat-buffer)
        send-history next-action)
    (setq next-action (funcall skype-commit-function text))
    (erase-buffer)
    (push text skype-send-history)
    (if (> (length skype-send-history) skype--message-send-history-size)
        (nbutlast skype-send-history))
    (setq send-history skype-send-history)
    (save-excursion
      (set-buffer chat-buf)
      (setq skype-send-history send-history)
      (skype--update-chat-buffer-command))
    (let ((other-window-scroll-buffer chat-buf))
      (scroll-other-window 3)) ; not smart
    (cond
     ((eq 'kill-buffer next-action)
      (skype--message-mode-quit-command))
     (t
      ))))

(defun skype--message-mode-prev-message-command ()
  (interactive)
  (cond 
   ((= 0 (length skype-send-history)) (message "No history."))
   ((= -1 skype-history-pos)
    (setq skype-writing-text
          (buffer-substring-no-properties 
           (point-min) (point-max)))
    (erase-buffer)
    (incf skype-history-pos)
    (insert (car skype-send-history)))
   ((< skype-history-pos (1- (length skype-send-history)))
    (erase-buffer)
    (incf skype-history-pos)
    (insert (nth skype-history-pos skype-send-history)))
   (t (message "No history."))
   ))

(defun skype--message-mode-next-message-command ()
  (interactive)
  (cond 
   ((= skype-history-pos 0)
    (erase-buffer)
    (decf skype-history-pos)
    (insert skype-writing-text))
   ((> skype-history-pos 0)
    (erase-buffer)
    (decf skype-history-pos)
    (insert (nth skype-history-pos skype-send-history)))
   (t (message "No history."))
   ))

(defun skype--message-mode-quit-command ()
  (interactive)
  (kill-buffer)
  (if (not (one-window-p t))
      (delete-window)))

(defun skype--message-mode-emoticon-command ()
  (interactive)
  (let ((ecode
         (skype--completing-read 
          "Emoticon : "
          skype--emoticon-list
          (lambda (pair) pair))))
    (when ecode
      (insert (skype--icon (substring ecode 0) 
                           (gethash ecode skype--emoticon-map))))))

(defun skype--message-mode ()
  (kill-all-local-variables)
  (make-local-variable 'skype-history-pos)
  (make-local-variable 'skype-writing-text)
  (make-local-variable 'skype-mode-line-prompt)
  (setq major-mode 'skype--message-mode
        mode-name "Skype-Message"
        skype-history-pos -1
        skype-writing-text ""
        skype-mode-line-prompt ""
        mode-line-format skype--message-mode-line-format)
  (use-local-map skype--message-mode-map)
  (run-hooks 'skype--message-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chat member buffer

(defvar skype--member-mode-hook nil
  "Hook for `skype--member-mode'")

(defvar skype--member-mode-line-format 
  '("-" mode-line-mule-info "  %20b " skype-mode-line-prompt 
    "   %l,%c " mode-line-modes "%-")
  "Mode line format for skype member mode.")

(defvar skype--member-mode-map nil
  "Keymap for skype--member-mode.")
(setq skype--member-mode-map nil) ; for debug
(unless skype--member-mode-map
  (setq skype--member-mode-map (make-sparse-keymap))
  (mapc (lambda (i)
            (define-key skype--member-mode-map
              (car i) (cdr i)))
          '(("m" . skype--member-mode-open-chat-command)
            ("q" . skype--member-mode-quit-command)
            ("l" . skype--member-mode-quit-command)
            ("u" . skype--member-mode-update-command)
            ("a" . skype--member-mode-add-member-command)
            
            ("n" . next-line)
            ("p" . previous-line)
            ("j" . next-line)
            ("k" . previous-line)
            )))

(defun skype--member-mode-add-member-command ()
  "Add a member to the chat."
  (interactive)
  (when (and (skype--member-buffer-p) skype-chat-handle)
    (let ((target-user
           (skype--select-user "Add chat member: ")))
      (when target-user
        (skype--chat-add-member
         skype-chat-handle target-user)
        (skype--member-mode-update-buffer)))))

(defun skype--member-mode-open-chat-command ()
  "Open a new chat for the user on which "
  (interactive)
  (let ((user-handle (get-text-property (point) 'skype-user-handle)))
    (when user-handle
      (skype--open-new-chat-command user-handle))))

(defun skype--member-mode-quit-command ()
  (interactive)
  (when (skype--member-buffer-p)
    (kill-buffer)
    (if (not (one-window-p t))
        (delete-window))))

(defun skype--member-mode-update-command ()
  (interactive)
  (skype--member-mode-update-buffer))

(defun skype--open-all-users-buffer-command ()
  "Open a buffer for all contact users."
  (interactive)
  (let* ((buf (skype--open-all-users-buffer))
         (msg-window (or (get-buffer-window buf)
                         (get-buffer-window (current-buffer)))))
    (pop-to-buffer buf msg-window)))

(defun skype--open-all-users-buffer ()
  "Return a buffer for all contact users"
  (skype--open-member-buffer-gen 
   "Skype All contact users"
   'skype--get-all-contact-users))

(defun skype--open-chat-member-buffer-command ()
  "Open a chat member buffer for the current chat."
  (interactive)
  (when skype-chat-handle
    (let* ((chat-buf (current-buffer))
           (buf (skype--open-chat-member-buffer skype-chat-handle))
           (msg-window (or (get-buffer-window buf)
                           (split-window (get-buffer-window chat-buf) nil t))))
      (pop-to-buffer buf msg-window))))

(defun skype--open-chat-member-buffer (chat-handle)
  "Return a buffer for the chat members."
  (lexical-let ((chat-handle chat-handle))
    (skype--open-member-buffer-gen
     (skype--chat-members-get-buffername chat-handle)
     (lambda () (skype--chat-get-members chat-handle)))))

(defun skype--open-member-buffer-gen (buffername getter-function)
  "Return chat member buffer. The function GETTER-FUNCTION is a
function that returns a list of member handles."
  (let* ((chat-obj (and (local-variable-p 'skype-chat-handle (current-buffer))
                        (skype--chat-handle-to-object skype-chat-handle)))
         (buf (get-buffer-create buffername))
         (chat-buf (current-buffer)))
    (save-excursion
      (set-buffer buf)
      (unless (eq major-mode 'skype--member-mode)
        (skype--member-mode)
        (when chat-obj
          (make-local-variable 'skype-chat-handle)
          (setq skype-chat-handle (skype-chat-chat-handle chat-obj)))
        (make-local-variable 'skype-chat-buffer)
        (make-local-variable 'skype-member-getter-function)
        (setq skype-chat-buffer chat-buf
              skype-member-getter-function getter-function)
        (buffer-disable-undo buf))
      (skype--member-mode-update-buffer buf))
    buf))

(defun skype--member-buffer-p (&optional buf)
  (unless buf
    (setq buf (current-buffer)))
  (eq 'skype--member-mode (buffer-local-value 'major-mode buf)))

(defun skype--member-mode-update-buffer (&optional buf)
  "Update member buffer."
  (unless buf (setq buf (current-buffer)))
  (when (skype--member-buffer-p buf)
    (save-excursion
      (unwind-protect
          (progn
            (set-buffer buf)
            (setq buffer-read-only nil)
            (erase-buffer)
            (dolist (m (funcall skype-member-getter-function))
              (let ((text (substring (skype--user-get-viewtext m) 0)))
                (put-text-property 0 (length text) 'skype-user-handle m text)
                (insert text "\n"))))
        (setq buffer-read-only t)))))

(defun skype--member-mode ()
  (kill-all-local-variables)
  (make-local-variable 'skype-mode-line-prompt)
  (setq major-mode 'skype--member-mode
        mode-name "Skype-Members"
        mode-line-format skype--member-mode-line-format
        skype-mode-line-prompt "")
  (use-local-map skype--member-mode-map)
  (run-hooks 'skype--member-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update timer

(defvar skype--timer-inhibit-update-chat-buffers nil "[internal use]")
(defvar skype--timer-update-handle nil "[internal use]")
(defvar skype--timer-update-counter 0 "[internal use]")
(defvar skype--timer-update-chat-buffers-interval 4 
  "Seconds for update chat buffers.")
(defvar skype--timer-update-chat-buffers-inactive-skip 5
  "Skip times for updating inactive skype buffers.")
(defvar skype--timer-hook nil
  "If this hook is not nil, the update timer calls this hook each
  update time.")

(defun skype--timer-start-update ()
  "Start the update timer that updates skype chat buffers."
  (interactive)
  (unless skype--timer-update-handle
    (setq skype--timer-update-handle 
          (run-at-time
           skype--timer-update-chat-buffers-interval
           skype--timer-update-chat-buffers-interval
           'skype--timer-update-chat-buffers))
    (skype--message "skype update timer started.")))

(defun skype--timer-stop-update ()
  "Stop the update timer that updates skype chat buffers."
  (interactive)
  (if skype--timer-update-handle
      (progn
        (cancel-timer skype--timer-update-handle)
        (setq skype--timer-update-handle nil))
    (error "skype--timer-update-handle is nil."))
  (skype--message "skype update timer stopped."))

(defun skype--timer-update-chat-buffers ()
  "Update some chat buffers."
  (unless skype--timer-inhibit-update-chat-buffers
    (setq skype--timer-inhibit-update-chat-buffers t)
    (unwind-protect
        (let ((buffers (skype--collect-chat-buffers 
                        (< 0 skype--timer-update-counter)))
              (update-buffer-num 0))
          (if buffers 
              (dolist (buf buffers) 
                (if (< 0 (skype--update-chat-buffer buf))
                    (incf update-buffer-num))))
          (unless (skype--collect-chat-buffers nil)
            (skype--timer-stop-update))
          (when (< 0 update-buffer-num)
            (skype--message (format "Skype: update %d buffers " 
                            update-buffer-num ))))
      (setq skype--timer-inhibit-update-chat-buffers nil)
      (if (> skype--timer-update-counter
             skype--timer-update-chat-buffers-inactive-skip)
          (setq skype--timer-update-counter 0)
        (incf skype--timer-update-counter)))
    (run-hooks skype--timer-hook)))

(defun skype--collect-chat-buffers (active-p)
  "Return skype chat buffers. If ACTIVE-P is non-nil, return the
active buffers those are shown in the some windows."
  (skype--collect 
   (buffer-list)
   (if (and (local-variable-p 'skype-chat-handle it)
            (local-variable-p 'skype-last-updated-time it)
            (or (get-buffer-window it t)
                (null active-p)))
       it nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Anything interface

(when (featurep 'anything)

  (setq skype--completing-read-function
        (lambda (prompt source &optional predicate require-match initial-input hist)
          (let (ret)
            (anything
             `((
               (name ,@prompt)
               (candidates ,@source)
               (action . (("Select" . (lambda (i) (setq ret i)))))
               (migemo)))
             nil prompt nil nil)
            ret)))

  (defun skype--anything-command ()
    "anything interface for skype."
    (interactive)
    (let* ((chat-action '(("Show" . 
                           (lambda(i) 
                             (switch-to-buffer
                              (skype--open-chat-buffer i))))))
           (chat-trans (lambda (chats) 
                          (skype--collect chats
                            (cons (skype-chat-title it) it))))
           (source-status
            `((name ,@(format "Change my status [%s] "
                              (skype-status-name 
                               (gethash (skype--get-my-status)
                                        skype--status-codes))))
              (candidates 
               . (lambda ()
                   (let (statuses)
                     (maphash (lambda (key value)
                                (if (string= key "UNKNOWN") nil
                                  (push value statuses)))
                              skype--status-codes)
                     (nreverse statuses))))
              (candidate-transformer 
               . (lambda (candidates)
                   (mapcar
                     (lambda (value) 
                      (let ((text (skype-status-name value))
                            (icon (skype--icon " " (skype-status-icon value)))
                            (code (skype-status-code value)))
                        (cons (concat "-> " text icon) code)))
                    candidates)))
              (action . (("Change status" . (lambda(i) (skype--set-my-status i)))))))
           (source-contacts 
            `((name . "Contact")
              (candidates . (lambda () (skype--get-all-contact-users)))
              (action . (("Start chat" 
                          . (lambda (i) (skype--open-new-chat-command i)))))
              (candidate-transformer 
               . (lambda (contacts) 
                   (skype--collect contacts 
                     (cons (skype--user-get-viewtext it) 
                           it))))
              (migemo)))
           (source-recents
           `((name . "Recent chats")
             (candidates . (lambda () (skype--chat-get-recent-objects)))
             (action ,@chat-action)
             (candidate-transformer ,@chat-trans)
             (migemo)))
          (source-bookmarked
           `((name . "Bookmarked chats")
             (candidates . (lambda () (skype--chat-get-bookmarked-objects)))
             (action ,@chat-action)
             (candidate-transformer ,@chat-trans)
             (migemo)))
          (source-missed
           `((name . "Missed chats")
             (candidates . (lambda () (skype--chat-get-missed-objects)))
             (action ,@chat-action)
             (candidate-transformer ,@chat-trans)
             (migemo))))
      (anything
       '(source-status source-missed source-contacts 
                       source-recents source-bookmarked)
       nil "Skype : " nil nil)))

  ;; skype
  ;; (global-set-key (kbd "M-9") 'skype--anything-command)
  ) ; anything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(skype--init)
(provide 'skype)

;;; skype.el ends here
