

; Input : hash-table de like
; Un like :
;   - une hash table
;   - hash-table
;      name
;      category
;      ID
;      created_time
;
;Output :
;    (nom_de_la_category . mots clés)


(defun get-tags (likes)
  (let ((nb-hash (make-hash-table :test 'equal))
	(tag-hash (make-hash-table :test 'equal))
	(tp-max 0)
	(cat-max ""))

    ; On trie les like :
    (maphash #'(lambda(key x)
		 (let ((category (gethash "category" x))
		       (name (gethash "name" x)))
		   
	       ;On augmente le compteur de category :
		   (setq (gethash category nb-hash)
			 (+ 1 (gethash category cat-hash)))
		   
	       ;On rajoute le mot clé à la catégorie :
		   (setq (gethash category tag-hash)
			 (cons name (gethash category tag-hash)))))
	     likes)

    ; On trouve la categorie maximale :
    (maphash '#(lambda (key value)
		(if (> value tp-max)
		    ((setq tp-max value)
		     (setq cat-max key))))
	     nb-hash)
    
    ; On renvoie les mots clés :
    (cons cat-max (gethash cat-max tag-hash))))



		    
		
;Input :
;   categorie , mots clés
;
;Propose des cadeaux dans la categorie donnée
;Avec les motcs clé.
;
;Output :
;   liste de requete amazon , url
(defun traiter-categories (cat mc)
  (cond ((= cat "Film")
	 (mapcar #'(lambda(x)
		     (amazon-request x "film"))
		 (imdb-request mc)))
	(else
	 (mapcar #'(lambda(x)
		     (amazon-request x cat))
		 mc))))