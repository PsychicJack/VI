(setq matrica '(((0 1 2 3) (4 5 6 7) (8 9 A B) (C D E F)) ((G H I J) (K L M N) (O P Q R) (S T U V)) ((W X Y Z) (a b c d) (e f g h) (i j k l)) ((m - o p) (q - s t) (u v w x) (y z - +))))
(defun main()
    
    (princ "Unesite velicinu (4/6): ")
   ;; (setq velicinaTabele (read))
    (setq velicinaTabele 4)
    (setq trentuniIgrac 'H)
    (setq trenutnaBoja 'X)
    ;;(odabirPrvogIgraca)
   ;; (odabirBoje)

    ;;(setq velicinaTabele 4)
    //(setq tabela (inicijalizujTabelu '0))
    ;;(princ "---------")
   ; (setf (nth 1 (nth 5 (nth 5 tabela))) 'O)
    ;(setf (nth 5 (nth 0 (nth 4 tabela))) 'O)
    ;(setf (nth 4 (nth 0 (nth 3 tabela))) 'O)
    ;(setf (nth 3 (nth 0 (nth 2 tabela))) 'O)
    ;(setf (nth 2 (nth 0 (nth 1 tabela))) 'O)
    ;(setf (nth 1 (nth 0 (nth 0 tabela))) 'O)

    (setf (nth 3 (nth 0 (nth 0 tabela))) 'O)
    (setf (nth 2 (nth 1 (nth 1 tabela))) 'O)
    (setf (nth 1 (nth 2 (nth 2 tabela))) 'O)
    (setf (nth 0 (nth 3 (nth 3 tabela))) 'O)
    (format t "~%")
    (potez)
)

(defun odabirPrvogIgraca () 
  (princ "Izbor prvog igraca (H/C): ")
  (setq prviIgracTemp (read))
  (if 
     (or (equal prviIgracTemp 'C) (equal prviIgracTemp 'H)) 
     (setf trentuniIgrac prviIgracTemp) 
    (princ "Ulaz mora biti H ili C"))                       
 )

(defun odabirBoje () 
  (princ "Izbor boje prvog igraca (X/O): ")
  (setq bojaIgracaTemp (read))

  (if 
      (or (equal bojaIgracaTemp 'X) (equal bojaIgracaTemp 'O)) 
      (setf trenutnaBoja bojaIgracaTemp) 
      (princ "Ulaz mora biti X ili O"))                       
)

(defun inicijalizujTabelu (rbrReda)
    (cond
        ((>= rbrReda velicinaTabele) '())
        (t
            (cons  (inicijalizujRed rbrReda velicinaTabele) 
                (inicijalizujTabelu (+ 1 rbrReda))
            )
        )
     ) 
)
(defun inicijalizujRed (rbrReda rbrKolone);; promena iz '(- - - -) -> (list '- '- '- '-)
    (cond 
        ((<= rbrKolone 0) '())
		(t (if (= 4 velicinaTabele)
     			(cons (list '- '- '- '-) (inicijalizujRed rbrReda (- rbrKolone 1))) 
     			(if (= 6 velicinaTabele)
				(cons (list '- '- '- '- '- '-) (inicijalizujRed rbrReda (- rbrKolone 1)))
				(format t "Nije uneta ispravna vrednost za velicinu kocke")
			))
     		)
    )
)

(defun potpunPrikaz () ;; prikaz tabela sa prikazom numeracije polja
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (format t "~%")
    (prikazStanja velicinaTabele velicinaTabele 0 velicinaTabele)
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
)

(defun prikazStanja (od do ind1 ind2) ;; radi prika tabela
    (decf ind2)
    (cond ((= ind2 -1) (setq ind2 0) (incf ind1)))
    (decf od)
    (cond ((= od -1) (setq od 0) (decf do)))
    (cond ((= do 0) '()) (t (stampajRed od do 0 ind1 ind2) (format t "~%") (prikazStanja od do ind1 ind2)))
)
 
(defun stampajRed (od do count ind1 ind2) ;; stampa ceo red na prikazu, ne red u listi (npr. za prvi red "   -   -   -   -", za drugi "  --  --  --  --")

    (cond ((= count velicinaTabele) '()) 
    (t  (stampajBlankoOd od '0) 
        (stampajPolja od do count (1- ind1) (1- ind2))
        (stampajBlankoDo do velicinaTabele)
        
        (stampajRed od do (1+ count) ind1 ind2)
    )
    )
)
 
(defun stampajBlankoOd (od trenutni) ;; stampa razmake sa leve strane reda matrice
    (cond ((= od trenutni) '()) (t (format t " ") (stampajBlankoOd od (1+ trenutni))))
)

(defun stampajPolja (od do red ind1 ind2)
    (incf ind1)
    (incf ind2)
    (cond ((= od do) '())
    (t (princ (nth ind2 (nth ind1 (nth red tabela)))) (stampajPolja (1+ od) do red ind1 ind2))
    )
)

(defun stampajBlankoDo (stampajDo granica) ;; stampa razmake sa desne strane reda matrice
    (cond ((= stampajDo granica) '()) (t (format t " ") (stampajBlankoDo (1+ stampajDo) granica))) 
)

(defun potez ()
    (potpunPrikaz)
    (format t "~%Potez: ")
    (setq potez (read))
    (proveriPotez (write-to-string potez));; write-to-string je obican convert to string, jer proveri potez radi samo sa stringovim
    (cond ((jeKrajIgre 0 0) (potpunPrikaz) (proveriPoene)(format t "~%Kraj igre.")) 
    (t (proveriPoene) (potez)))
)

(defun proveriPotez (potez) ;; proverava da li je potez validan i odigrava ga
    (setq temp (- (char-code (char potez 0)) 55));; uzima prvi element iz string potez i odredjuje njegov char code. Oduzima 55 jer karakter A ima kod 65, a nama predstavlja broj 10. 
    (cond ((< temp 10) (incf temp 7)));; posto postoji 7 mesta izmedju velikih slova i brojeva u ascii tabeli ukoliko je broj dodajemo mu 7
    (setq rbrReda (floor temp velicinaTabele));; odredjujemo rbrReda celobrojnim deljenjem
    (setq rbrStubica (mod temp velicinaTabele))
    (cond ((or (>= rbrReda velicinaTabele) (< rbrReda 0)) (format t "Nepravilan potez ~%") '())
        (t 
            (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda tabela))) '-) (odigrajPotez rbrReda rbrStubica (- velicinaTabele 1)) (promeniIgraca)) (t (format t "Nepravilan potez ~%") '()))
        )
    )
)

(defun odigrajPotez (rbrReda rbrStubica trenutni) ;;ne proverava da li ima slobodnih mesta, jer je to provereno u proveriPotez
    (cond ((equal  (nth rbrStubica (nth trenutni (nth rbrReda tabela))) '-)
    (setf  (nth rbrStubica (nth trenutni (nth rbrReda tabela))) trenutnaBoja))
     (t (odigrajPotez rbrReda rbrStubica (1- trenutni)))
    )
)

(defun promeniIgraca () 
    (cond ((equal trentuniIgrac 'H) (setq trentuniIgrac 'C)) ((equal trentuniIgrac 'C) (setq trentuniIgrac 'H)))
    (cond ((equal trenutnaBoja 'X) (setq trenutnaBoja 'O)) ((equal trenutnaBoja 'O) (setq trenutnaBoja 'X)))
)


(defun jeKrajIgre (rbrReda rbrStubica)
    (cond ((>= rbrStubica velicinaTabele) (setq rbrStubica 0) (incf rbrReda)))
    (cond ((>= rbrReda velicinaTabele) t) (t 
    (cond ((not (equal (nth rbrStubica (nth 0 (nth rbrReda tabela))) '-)) (and t (jeKrajIgre rbrReda (1+ rbrStubica))))
            (t '())
    )))
    
)

(defun proveriPoene ()
    (setq xPoints 0)
    (setq oPoints 0)
    ;(proveriSveY 0 0)
    ;(proveriSveZ (1- velicinaTabele) 0)
    ;(proveriSveX 0 (1- velicinaTabele))
    (cond ((= velicinaTabele 4) 
           ; (proveriSveZY4 0)
            ;(proveriSveYX4 0)
            ;(proveriSveZX4 0)
            (proveri3DDijagonale4)
    ) 
         ((= velicinaTabele 6)
            ;(proveriSveZY6 0)
            ;(proveriSveYX6 0)
            ;(proveriSveZX6 0)
         )
    )
    (format t "~% xPoints: ")
    (princ xPoints)
    (format t "~% oPoints: ")
    (princ oPoints)
    (format t "~%")
)

(defun proveriSveY (z x) 
    (cond ((= z velicinaTabele) (setf z 0) (incf x)))
    (cond ((= x velicinaTabele) '())
    (t 
        (proveriY z (1- velicinaTabele) x 0 0)
        (proveriSveY (1+ z) x)
    ))
)

(defun proveriY (z y x xNiz oNiz)
    (cond ((= y -1) '()) 
    (t
        (setq polje (nth z (nth y (nth x tabela))))
        (cond ((equal polje '-) '()) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriY z (1- y) x xNiz oNiz))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriY z (1- y) x xNiz oNiz))
        )
    ))   
    
)

(defun proveriSveZ (y x)
    (cond ((= y -1) (setf y (1- velicinaTabele)) (incf x)))
    (cond ((= x velicinaTabele) '())
    (t 
        (proveriZ 0 y x 0 0)
        (proveriSveZ (1- y) x)
    ))
)

(defun proveriZ (z y x xNiz oNiz)
    (cond ((= z velicinaTabele) '()) 
    (t
        (setq polje (nth z (nth y (nth x tabela))))
        (cond ((equal polje '-) (setf oNiz 0) (setf xNiz 0) (proveriZ (1+ z) y x xNiz oNiz)) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriZ (1+ z) y x xNiz oNiz))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriZ (1+ z) y x xNiz oNiz))
        )
    ))   
)

(defun proveriSveX (z y)
    (cond ((= z velicinaTabele) (setf z 0) (decf y)))
    (cond ((= y -1) '())
    (t 
        (proveriX z y 0 0 0)
        (proveriSveX (1+ z) y)
    ))
)

(defun proveriX (z y x xNiz oNiz)
    (cond ((= x velicinaTabele) '()) 
    (t
        (setq polje (nth z (nth y (nth x tabela))))
        (cond ((equal polje '-) (setf oNiz 0) (setf xNiz 0) (proveriX z y (1+ x) xNiz oNiz)) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriX z y (1+ x) xNiz oNiz))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriX z y (1+ x) xNiz oNiz))
        )
    ))   
)

(defun proveriDijagonalu (z y x kraj xNiz oNiz incZ incY incX uslov uslovInc) 
    (cond ((= uslov kraj) '())
    (t
        (setq polje (nth z (nth y (nth x tabela))))
         (cond ((equal polje '-) (setf oNiz 0) (setf xNiz 0) (proveriDijagonalu (+ z incZ) (+ y incY) (+ x incX) kraj xNiz oNiz incZ incY incX (+ uslov uslovInc) uslovInc)) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriDijagonalu (+ z incZ) (+ y incY) (+ x incX) kraj xNiz oNiz incZ incY incX (+ uslov uslovInc) uslovInc))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriDijagonalu (+ z incZ) (+ y incY) (+ x incX) kraj xNiz oNiz incZ incY incX (+ uslov uslovInc) uslovInc))
        )
    ))
)


(defun proveriSveZY4 (x)
    (cond ((= x velicinaTabele) '()) (t 
        (proveriDijagonalu 0 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 0 1)
        (proveriDijagonalu 3 (1- velicinaTabele) x -1 0 0 -1 -1 0 3 -1)
        (proveriSveZY4 (1+ x))
    ))
)

(defun proveriSveZY6 (x)
     (cond ((= x velicinaTabele) '()) (t 
     ; glavna
        (proveriDijagonalu 0 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 0 1)
        (proveriDijagonalu 0 (- velicinaTabele 2) x (1- velicinaTabele) 0 0 1 -1 0 0 1)
        (proveriDijagonalu 0 (- velicinaTabele 3) x (- velicinaTabele 2) 0 0 1 -1 0 0 1)
        (proveriDijagonalu 1 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 1 1)
        (proveriDijagonalu 2 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 2 1)
        ;sporedna
        (proveriDijagonalu 5 (1- velicinaTabele) x -1 0 0 -1 -1 0 5 -1)
        (proveriDijagonalu 5 (- velicinaTabele 2) x 0 0 0 -1 -1 0 5 -1)
        (proveriDijagonalu 5 (- velicinaTabele 3) x 1 0 0 -1 -1 0 5 -1)
        (proveriDijagonalu 4 (1- velicinaTabele) x -1 0 0 -1 -1 0 4 -1)
        (proveriDijagonalu 3 (1- velicinaTabele) x -1 0 0 -1 -1 0 3 -1)
        (proveriSveZY6 (1+ x))
    ))
)

(defun proveriSveYX4 (z)
    (cond ((= z velicinaTabele) '()) (t 
        (proveriDijagonalu z (1- velicinaTabele) 0 velicinaTabele 0 0 0 -1 1 0 1)
        (proveriDijagonalu z (1- velicinaTabele) 3 -1 0 0 0 -1 -1 3 -1)
        (proveriSveYX4 (1+ z))
    ))
)

(defun proveriSveYX6 (z)
     (cond ((= z velicinaTabele) '()) (t 
     ; glavna
        (proveriDijagonalu z (1- velicinaTabele) 0 velicinaTabele 0 0 0 -1 1 0 1)
        (proveriDijagonalu z (- velicinaTabele 2) 0 (1- velicinaTabele) 0 0 0 -1 1 0 1)
        (proveriDijagonalu z (- velicinaTabele 3) 0 (- velicinaTabele 2) 0 0 0 -1 1 0 1)
        (proveriDijagonalu z (1- velicinaTabele) 1 velicinaTabele 0 0 0 -1 1 1 1)
        (proveriDijagonalu z (1- velicinaTabele) 2 velicinaTabele 0 0 0 -1 1 2 1)
     ;sporedna
        (proveriDijagonalu z (1- velicinaTabele) 5 -1 0 0 0 -1 -1 5 -1)
        (proveriDijagonalu z (- velicinaTabele 2) 5 0 0 0 0 -1 -1 5 -1)
        (proveriDijagonalu z (- velicinaTabele 3) 5 1 0 0 0 -1 -1 5 -1)
        (proveriDijagonalu z (1- velicinaTabele) 4 -1 0 0 0 -1 -1 4 -1)
        (proveriDijagonalu z (1- velicinaTabele) 3 -1 0 0 0 -1 -1 3 -1)
        (proveriSveYX6 (1+ z))
    ))
)

(defun proveriSveZX4 (y)
    (cond ((= y velicinaTabele) '()) (t 
        (proveriDijagonalu 0 y 0 velicinaTabele 0 0 1 0 1 0 1)
        (proveriDijagonalu 0 y 3 velicinaTabele 0 0 1 0 -1 0 1)
        (proveriSveZX4 (1+ y))
    ))
)

(defun proveriSveZX6 (y)
     (cond ((= y velicinaTabele) '()) (t 
     ; glavna
        (proveriDijagonalu 0 y 0 velicinaTabele 0 0 1 0 1 0 1)
        (proveriDijagonalu 0 y 1 (1- velicinaTabele) 0 0 1 0 1 0 1)
        (proveriDijagonalu 0 y 2 (- velicinaTabele 2) 0 0 1 0 1 0 1)
        (proveriDijagonalu 1 y 0 velicinaTabele 0 0 1 0 1 1 1)
        (proveriDijagonalu 2 y 0 velicinaTabele 0 0 1 0 1 2 1)
     ;sporedna
        (proveriDijagonalu 0 y 5 velicinaTabele 0 0 1 0 1 0 1)
        (proveriDijagonalu 0 y 4 (1- velicinaTabele) 0 0 1 0 1 0 1)
        (proveriDijagonalu 0 y 3 (- velicinaTabele 2) 0 0 1 0 1 0 1)
        (proveriDijagonalu 1 y 5 velicinaTabele 0 0 1 0 1 1 1)
        (proveriDijagonalu 2 y 5 velicinaTabele 0 0 1 0 1 2 1)
        (proveriSveZX6 (1+ y))
    ))
)

(defun proveri3DDijagonale4 () 
    (proveriDijagonalu 0 3 0 4 0 0 1 -1 1 0 1)
    (proveriDijagonalu 0 0 0 4 0 0 1 1 1 0 1)
    (proveriDijagonalu 3 3 0 -1 0 0 -1 -1 1 3 -1)
    (proveriDijagonalu 3 0 0 -1 0 0 -1 1 1 3 -1)
)


(main)

;;(nth ind2 (nth ind1 (nth red matrica)))
;;(princ (- (char-code (char "A" 0)) 55))


;; 1 -> od = 3, do = 4, (0, 3)
;; 2 -> od = 2, do = 4, (0, 2) (1, 3)
;; 3 -> od = 1, do = 4, (0, 1) (1, 2) (2, 3)
;; 4 -> od = 0, do = 4, (0, 0) (1, 1) (2, 2) (3, 3)
;; 5 -> od = 0, dp = 3, (1, 0) (2, 1) (3, 2)
;; 6 -> od = 0, do = 2, (2, 0) (3, 1)
;; 7 -> od = 0, do = 1, (3, 0)