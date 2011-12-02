(ql:quickload "yason")

(ql:quickload "drakma")

(defun create_url_likes (id acces_token)
	   (concatenate 'string "https://graph.facebook.com/" id "/likes?" acces_token))

(defun drakma_likes (id acces_token)
		 (drakma:http-request (create_url_likes id acces_token)))

(defun json_likes (id acces_token)
	   (yason:parse (drakma_likes id acces_token)))

(defun list_likes (h)
  (let ((output nil))
    (maphash #'(lambda ( key val )
		 (setq output (cons val output)))
	     h)))
(defun likes (id acces_token)
  (gethash "data"  (json_likes id acces_token)))

(defun create_url_friends (acces_token)
  (concatenate 'string "https://graph.facebook.com/me/friends?" acces_token))

(defun drakma_friends (ID)
  (drakma:http-request (create_url_friends ID)))

(defun json_friends (id)
  (yason:parse (drakma_friends id)))

(defun friends (acces_token)
  (gethash "data" (json_friends acces_token)))




