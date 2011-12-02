;Serveur web :
(ql:quickload "hunchentoot")

;Compialtion de html :
(ql:quickload "cl-who")

;Expression régulière :
(ql:quickload "cl-ppcre")

(defpackage :webserver
  (:use :cl :hunchentoot :cl-who :cl-ppcre))

(in-package :webserver)

;Fonction générique qui transforme une alist en hash-table :
(defun alist->hashtable (al)
  (let ((h (make-hash-table :test 'equal)))
    (print al)
    (map 'list
	 #'(lambda(x)
	     (if (typep (cdr x) 'list)
		 (setf (gethash (first x) h) (second x))
		 (setf (gethash (car x) h) (cdr x)))) ; => utile pour les pairs
	 al)
    h))

;Cette fonction crée le code pour la boucle :
(defun loop-html (str varname varvalues h)
  (let ((output "")
	(sanit-str (regex-replace (concatenate 'string "<lisp loop " varname ">")
					   (regex-replace (concatenate 'string "</loop " varname ">$")
								   (regex-replace-all  (concatenate 'string "<with " varname)
												str
												(concatenate 'string "<lisp " varname))
								   "")
					   "")))
    (map 'list
	 #'(lambda (val)
	     (setf (gethash varname h) val)
	     (setq output
		   (concatenate 'string output
				(rec-html sanit-str h))))
	 varvalues)
    output))



;Cette fonction fait les meme remplacement que load-html, mais
;sur le string str en argument, pas sur un fichier. De plus,
;elle attend un hash bien-formé sur pour h :
(defun rec-html (str h)
  (let ((output str))

    ;On remplace :
    (maphash #'(lambda (key val)
		 ; Pour les variables :
		 (setq output (regex-replace-all (concatenate 'string "<lisp " key ">")
						 output
						 val))
		 ;Pour les boucles :
		 (map 'list
		      #'(lambda (str)
			  (setq output (regex-replace str
						      output
						      (loop-html str key val h))))
		      (all-matches-as-strings (concatenate 'string "<lisp loop " key ">.*</loop " key ">")
					      output)))
	     
	     h)
    output))


(defvar *global-args* (list (list "dev-name" "eXenon")))
(defvar *app-dir* "./")

;Charge le fichier *home*/html/name.html et le modifie
;grace aux balises <lisp> insérées dans le html. Les balises
;suivantes sont à disposition :
; - <lisp varname> : est simplement remplacé par la valeur de varname
; - <lisp loop varname></loop varname> : les lignes qui suivent cette instruction seront répétées pour chaque valeur contenue dans varname
;
;args est une liste de paire construite de la mnière suivante :
; args = (list (varname1 varvalue1) (varname2 varvalue2) ... )
; varname et varvalue doivent être des chaines de caractères, SAUF pour les boucles, dans quel cas varvalue est une liste de string.
;
(defun load-html (name args)
  (let* ((output "")
	 (real-args (concatenate 'list args *global-args*))
	 (h (alist->hashtable real-args)))

    (print h)
	
    ;On crée le string contenant le HTML :
    (with-open-file (stream (concatenate 'string *app-dir* "html/" name ".html"))
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(setq output (concatenate 'string output line))))

    (print "test2")

    ;On remplace :
    (maphash #'(lambda (key val)
		 ; Pour les variables :
		 (setq output (regex-replace-all (concatenate 'string "<lisp " key ">")
						 output
						 val))

		 (print key)
		 (print val)

		 ;Pour les boucles :
		 (map 'list
		      #'(lambda (str)
			  (print "str")
			  (print str)
			  (setq output (regex-replace str
						      output
						      (loop-html str key val h))))
		      (all-matches-as-strings (concatenate 'string "<lisp loop " key ">.*</loop " key ">")
					      output))
		 (print "test4"))
	     
	     h)
    output))
	     





;
; == GESTION DES DISPATCHER ==
; 
;Source : http://sean-ross.blogspot.com/2007/05/hunchentoot-and-packages.html
;
(defun index ()
  #'(lambda ()
      (with-html-output-to-string (x)
        (:html "The index page"))))

(defun seq-last (seq)
  (aref seq (1- (length seq))))

(defun string->handler (string package)
  (when (string= string "/") (return-from string->handler (index)))
  (multiple-value-bind (sym type) (find-symbol string package)
    (when (and (not (eql type :inherited))
               (fboundp sym))
      (symbol-function sym))))


(defun create-package-dispatcher (prefix)
  (check-type prefix string)
  (assert (eql (seq-last prefix) #\/) (prefix) "Prefix must end in a / (forward slash)")
  #'(lambda (request)
      (let* ((function (script-name request))
             (mismatch (mismatch (script-name request) prefix
                                 :test #'char=)))
	(print (script-name request))
        (when (or (null mismatch)
                  (>= mismatch (length prefix)))
          (string->handler (string-upcase (subseq function (1- (length prefix))))
                           *package*)))))
      

;Le dispatcher pour tout :
(push (create-package-dispatcher "/w/") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/data/" (make-pathname :defaults (format nil "~aweb-data/" *app-dir*))) *dispatch-table*)
;
;  == FIN ==
;





;
; == PARAMETRAGE ==
;

;Définition d'argument valables sur toutes les fenêtres :
(defun add-global-arg (key val)
  (push (list key val) *global-args*))

(add-global-arg "menu" (list "Acceuil" "Recherche" "Contact"))
(add-global-arg "for" (list "item1" "item2" "item3"))



(print (load-html "main" nil))

(defun /hello ()
  (load-html "main" nil))

(defun /start ()
  (load-html "index" nil))

(defun /entry ()
  (load-html "entry" nil))

(defvar *webserver* (make-instance 'acceptor :port 4242))
