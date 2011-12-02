(defun digest-canonical-string (key string)
  (base64:usb8-array-to-base64-string (ironclad:hmac-digest
				       (ironclad:update-hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key) 'ironclad:sha256)
							     (ironclad:ascii-string-to-byte-array string)))))

(defun replace-string (from to string)
  (ppcre:regex-replace-all from string to))

(defun percent-encoder (string)
  (replace-string "\\+" "%20"
		  (replace-string "\\*" "%2A"
				  (replace-string "%7E" "~" string))))


(defvar *endpoint* "webservices.amazon.com")

(defun get-timestamp ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~D-~2,'0D-~2,'0DT~2,'0D%3A~2,'0D%3A~2,'0DZ" year month date hour minute second)))

(defun join-params (params &optional (string ""))
  (if (null params)
      string
      (join-params (rest params)
		   (format nil "~a~a=~a~a"
			   string (caar params) (cdar params)
			   (if (null (rest params)) "" "&")))))

(defun custom-lexicographic (string1 string2)
	   (let ((length1 (length string1))
		 (length2 (length string2)))
	     (labels ((get-code (string offset) (char-code (aref string offset)))
		      (compare-chars (offset1 offset2)
			(if (>= offset1 length1)
			    t
			    (if (>= offset2 length2)
				nil
				(let ((code1 (get-code string1 offset1))
				      (code2 (get-code string2 offset2)))
				  (if (eql code1 code2)
				      (compare-chars (1+ offset1) (1+ offset2))
				      (< code1 code2)))))))
	       (compare-chars 0 0))))

(defun get-canonical-string (request-uri key params &key timestamp)
  (format nil "GET~%~a~%~a~%~a" *endpoint* request-uri
	  (join-params (sort (acons "AWSAccessKeyId" key
				    (acons "Timestamp" (if timestamp timestamp (get-timestamp)) params))
			     #'custom-lexicographic :key #'car))))

(defun get-signature (request-uri key params &key timestamp)
  (percent-encoder (digest-canonical-string key (get-canonical-string request-uri key params :timestamp timestamp))))

;;

(define-test lexicographic
  (assert-false (custom-lexicographic "Aba" "ABa"))
  (assert-true (custom-lexicographic "Aba" "Abaa"))
  (assert-true (custom-lexicographic "ab" "ac")))
