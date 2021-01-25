;(setq tabela '(((X X X X) (X X X X) (X X X X) (X X X X)) ((X X X X) (X X X X) (X X X X) (X X X X)) ((X X X X) (X X X X) (X X X X) (X X X X)) ((O X - -) (X X X X) (X X X X) (X X X X))))

(load './Inference_engine.cl)
(defun main()
    
    (princ "Unesite velicinu (4/6): ")
    ;(setq velicinaTabele 4)
    ;(setq trentuniIgrac 'H)
    ;(setq trenutnaBoja 'X)
    (setq dubina 2)
    (setq velicinaTabele (read))
    (odabirPrvogIgraca)
    (odabirBoje)

    ;;(setq velicinaTabele 4)
    (setq tabela (inicijalizujTabelu '0))
    ;;(setq tabela '(((x o x o) (x o x o) (x o x o) (x o x o))
     ;;         ((x o x o) (x o x o) (x o x o) (x o x o))
      ;;        ((x o x o) (x o x o) (x o x o) (x o x o) )
       ;;       ((x o x o) (x o x o) (x o x o) (x o x o))))
    ;;(princ "---------")
    ;(setf (nth 4 (nth 3 (nth 4 tabela))) 'O)
    ;(setf (nth 3 (nth 2 (nth 3 tabela))) 'O)
    ;(setf (nth 2 (nth 1 (nth 2 tabela))) 'O)
    ;(setf (nth 1 (nth 0 (nth 1 tabela))) 'O)
    ;(setf (nth 0 (nth 0 (nth 0 tabela))) 'O)
    ;(setf (nth 0 (nth 0 (nth 0 tabela))) 'O)
    
    ;(princ (ispitajPoteze tabela))
    ;(setf tabela (potez tabela))
  
   
    ;(potpunPrikaz tabela)
   ; (proveriPotez "0" (copy-tree tabela))
   ; (potpunPrikaz (copy-tree tabela))
   ; (setf (nth 0 (nth 0 (nth 2 tabela))) 'O)
   ; (setf (nth 0 (nth 1 (nth 2 tabela))) 'O)
   ; (setf (nth 0 (nth 2 (nth 2 tabela))) 'O)
   ; (setf (nth 0 (nth 3 (nth 2 tabela))) 'O)

    (format t "~%")
    (potpunPrikaz tabela)
    (potez tabela)
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

(defun potpunPrikaz (stanje) ;; prikaz tabela sa prikazom numeracije polja
    (cond ((= velicinaTabele 4) (format t "~%0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (format t "~%")
    (prikazStanja velicinaTabele velicinaTabele 0 velicinaTabele stanje)
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
)

(defun prikazStanja (od do ind1 ind2 stanje) ;; radi prika tabela
    (decf ind2)
    (cond ((= ind2 -1) (setq ind2 0) (incf ind1)))
    (decf od)
    (cond ((= od -1) (setq od 0) (decf do)))
    (cond ((= do 0) '()) (t (stampajRed od do 0 ind1 ind2 stanje) (format t "~%") (prikazStanja od do ind1 ind2 stanje)))
)
 
(defun stampajRed (od do count ind1 ind2 stanje) ;; stampa ceo red na prikazu, ne red u listi (npr. za prvi red "   -   -   -   -", za drugi "  --  --  --  --")

    (cond ((= count velicinaTabele) '()) 
    (t  (stampajBlankoOd od '0) 
        (stampajPolja od do count (1- ind1) (1- ind2) stanje)
        (stampajBlankoDo do velicinaTabele)
        
        (stampajRed od do (1+ count) ind1 ind2 stanje)
    )
    )
)
 
(defun stampajBlankoOd (od trenutni) ;; stampa razmake sa leve strane reda matrice
    (cond ((= od trenutni) '()) (t (format t " ") (stampajBlankoOd od (1+ trenutni))))
)

(defun stampajPolja (od do red ind1 ind2 stanje)
    (incf ind1)
    (incf ind2)
    (cond ((= od do) '())
    (t (princ (nth ind2 (nth ind1 (nth red stanje)))) (stampajPolja (1+ od) do red ind1 ind2 stanje))
    )
)

(defun stampajBlankoDo (stampajDo granica) ;; stampa razmake sa desne strane reda matrice
    (cond ((= stampajDo granica) '()) (t (format t " ") (stampajBlankoDo (1+ stampajDo) granica))) 
)

(defun ispitajPoteze(tabelaZaIspitivanje)
  (setq lista '())
   (setq polja4 (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
   (setq polja6 (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36))
       (cond((= velicinaTabele 4)
           (proveriPoteze  (car polja4) tabelaZaIspitivanje (cdr polja4)))         
        (t (proveriPoteze  (car polja6) tabelaZaIspitivanje (cdr polja6)))
               )
  lista
  ;;(format t "~% Konacna lista: ") (princ lista) (format t "~%") 
)
 
(defun proveriPoteze (potez tabelaZaIspitivanje ostatakliste)
    
    (setq rbrReda (floor potez velicinaTabele))
    (setq rbrStubica (mod potez velicinaTabele))
    ;;(princ rbrReda) (format t "~%") (princ rbrStubica) (format t "~%")
    (cond ((or (>= rbrReda velicinaTabele) (< rbrReda 0)) '() )
        (t 
         (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda tabelaZaIspitivanje))) '-) 
                (push potez lista) ;(princ lista) (format t "~%") 
                )
               (t '() ))
        )
          ) 
    (cond ((equal (car ostatakliste) '()) ()) (t (proveriPoteze (car ostatakliste) tabelaZaIspitivanje (cdr ostatakliste)))) 
    
)


(defun potez (stanje)
   (cond ((equal trentuniIgrac 'H) 
        (format t "~%Potez: ")
        (setq potez (read))
        (proveriPotez (write-to-string potez) stanje)
    
        (potpunPrikaz stanje)
        (cond ((jeKrajIgre 0 0 tabela) (potpunPrikaz stanje) (proveriPoene tabela) (format t "~%Kraj igre. Crni je imao ~a poena, a beli ~a " xPoints oPoints) 
            (cond ((= (- xPoints oPoints) 0) (format t "Nereseno!")) ((> (- xPoints oPoints) 0) (format t "Pobednik je crni!")) ((< (- xPoints oPoints) 0) (format t "Pobednik je beli!")))
            stanje
        ) 
        (t ;(proveriPoene stanje) 
            (potez stanje) 
            stanje
        ))
   )
    (t 
        
       ;; (setf boi (alfaBeta (copy-tree stanje) 2 most-negative-fixnum most-positive-fixnum trenutnaBoja))
       (let ((potez (alfaBeta stanje '() most-negative-fixnum most-positive-fixnum dubina trenutnaBoja)))
            ;(print "IZASLI SMO IZ ALFABETA: ") (print potez)
            (cond ((null potez) (setq potez (car (ispitajPoteze tabela)))))
            (proveriPotezAlfaBeta potez stanje trenutnaBoja)
            ;;(setq stanje (proveriPotezAlfaBeta (nth 1 (alfaBeta tabela 4 most-negative-fixnum most-positive-fixnum trenutnaBoja)) stanje trenutnaBoja))
            (potpunPrikaz stanje)
            (promeniIgraca)
            (cond ((jeKrajIgre 0 0 tabela) (potpunPrikaz stanje) (proveriPoene tabela)  (format t "~%Kraj igre. Crni je imao ~a poena, a beli ~a " xPoints oPoints)
            (cond ((= (- xPoints oPoints) 0) (format t "Nereseno!")) ((> (- xPoints oPoints) 0) (format t "Pobednik je crni!")) ((< (- xPoints oPoints) 0) (format t "Pobednik je beli!")))
            stanje
            ) 
            (t ;(proveriPoene stanje) 
                (potez tabela) 
                tabela
            ))
       )
    )
    )
)


(defun proveriPotez (potez tabelaZaIspitivanje)
    (setq temp (- (char-code (char potez 0)) 55)) 
    (cond ((< temp 10) (incf temp 7)))
    (setq rbrReda (floor temp velicinaTabele))
    (setq rbrStubica (mod temp velicinaTabele))
    (cond ((or (>= rbrReda velicinaTabele) (< rbrReda 0)) (format t "Nepravilan potez ~%") '())
        (t 
         (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda tabelaZaIspitivanje))) '-) 
               
                (odigrajPotez rbrReda rbrStubica (- velicinaTabele 1) tabelaZaIspitivanje  trenutnaBoja) (promeniIgraca))
          
               (t (format t "Stubic je popunjen ~%") '()))
        )
    )
    
)


(defun odigrajPotez (rbrReda rbrStubica trenutni tabelaZaIspitivanje boja)
    (cond ((equal  (nth rbrStubica (nth trenutni (nth rbrReda tabelaZaIspitivanje))) '-)
           (setf  (nth rbrStubica (nth trenutni (nth rbrReda tabelaZaIspitivanje))) boja)
           )
     (t (odigrajPotez rbrReda rbrStubica (1- trenutni) tabelaZaIspitivanje boja) )
    )
  
)

(defun promeniIgraca () 
    (cond ((equal trentuniIgrac 'H) (setq trentuniIgrac 'C)) ((equal trentuniIgrac 'C) (setq trentuniIgrac 'H)))
    (cond ((equal trenutnaBoja 'X) (setq trenutnaBoja 'O)) ((equal trenutnaBoja 'O) (setq trenutnaBoja 'X)))
)


(defun jeKrajIgre (rbrReda rbrStubica stanje)
    (cond ((>= rbrStubica velicinaTabele) (setq rbrStubica 0) (incf rbrReda)))
    (cond ((>= rbrReda velicinaTabele) t) (t 
    (cond ((not (equal (nth rbrStubica (nth 0 (nth rbrReda stanje))) '-)) (and t (jeKrajIgre rbrReda (1+ rbrStubica) stanje)))
            (t '())
    )))
    
)

(defun proveriPoene (stanje)
    ;(PRINT "UNUTAR PROVERI POENE |||||||||||||||||||||||||||||||")
    ;(potpunPrikaz stanje)
    ;(PRINT "UNUTAR PROVERI POENE |||||||||||||||||||||||||||||||")
    (setq xPoints 0)
    (setq oPoints 0)
    ;(proveriSveY 0 0 stanje)
    (proveriSveZ (1- velicinaTabele) 0 stanje)
    (proveriSveX 0 (1- velicinaTabele) stanje)
   (cond ((= velicinaTabele 4) 
            (proveriSveZY4 0 stanje)
            (proveriSveYX4 0 stanje)
            (proveriSveZX4 0 stanje)
            (proveri3DDijagonale4 stanje)
    ) 
        ((= velicinaTabele 6)
            (proveriSveZY6 0 stanje)
            (proveriSveYX6 0 stanje)
            (proveriSveZX6 0 stanje)
            (proveri3DDijagonale6 5 stanje)
        )
    )
    
        
    ;; (format t "~% xPoints: ")
    ;; (princ xPoints)
    ;; (format t "~% oPoints: ")
    ;; (princ oPoints)
    ;; (format t "~%")
        
   
    (- xPoints oPoints)
)


(defun proveriSveY (z x stanje) 
    (cond ((= z velicinaTabele) (setf z 0) (incf x)))
    (cond ((= x velicinaTabele) '())
    (t 
        (proveriY z (1- velicinaTabele) x 0 0 stanje)
        (proveriSveY (1+ z) x stanje)
    ))
)

(defun proveriY (z y x xNiz oNiz stanje)
    (cond ((= y -1) '()) 
    (t
        (setq polje (nth z (nth y (nth x stanje))))
        (cond ((equal polje '-) '()) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriY z (1- y) x xNiz oNiz stanje))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriY z (1- y) x xNiz oNiz stanje))
        )
    ))   
    
)

(defun proveriSveZ (y x stanje)
    (cond ((= y -1) (setf y (1- velicinaTabele)) (incf x)))
    (cond ((= x velicinaTabele) '())
    (t 
        (proveriZ 0 y x 0 0 stanje)
        (proveriSveZ (1- y) x stanje)
    ))
)

(defun proveriZ (z y x xNiz oNiz stanje)
    (cond ((= z velicinaTabele) '()) 
    (t
        (setq polje (nth z (nth y (nth x stanje))))
        (cond ((equal polje '-) (setf oNiz 0) (setf xNiz 0) (proveriZ (1+ z) y x xNiz oNiz stanje)) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriZ (1+ z) y x xNiz oNiz stanje))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriZ (1+ z) y x xNiz oNiz stanje))
        )
    ))   
)

(defun proveriSveX (z y stanje)
    (cond ((= z velicinaTabele) (setf z 0) (decf y)))
    (cond ((= y -1) '())
    (t 
        (proveriX z y 0 0 0 stanje)
        (proveriSveX (1+ z) y stanje)
    ))
)

(defun proveriX (z y x xNiz oNiz stanje)
    (cond ((= x velicinaTabele) '()) 
    (t
        (setq polje (nth z (nth y (nth x stanje))))
        (cond ((equal polje '-) (setf oNiz 0) (setf xNiz 0) (proveriX z y (1+ x) xNiz oNiz stanje)) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriX z y (1+ x) xNiz oNiz stanje))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriX z y (1+ x) xNiz oNiz stanje))
        )
    ))   
)

(defun proveriDijagonalu (z y x kraj xNiz oNiz incZ incY incX uslov uslovInc stanje) 
    (cond ((= uslov kraj) '())
    (t
        (setq polje (nth z (nth y (nth x stanje))))
         (cond ((equal polje '-) (setf oNiz 0) (setf xNiz 0) (proveriDijagonalu (+ z incZ) (+ y incY) (+ x incX) kraj xNiz oNiz incZ incY incX (+ uslov uslovInc) uslovInc stanje)) 
            ((equal polje 'X) (incf xNiz) (cond ((= xNiz 4) (decf xNiz) (incf xPoints))) (setf oNiz 0) (proveriDijagonalu (+ z incZ) (+ y incY) (+ x incX) kraj xNiz oNiz incZ incY incX (+ uslov uslovInc) uslovInc stanje))
            ((equal polje 'O) (incf oNiz) (cond ((= oNiz 4) (decf oNiz) (incf oPoints))) (setf xNiz 0) (proveriDijagonalu (+ z incZ) (+ y incY) (+ x incX) kraj xNiz oNiz incZ incY incX (+ uslov uslovInc) uslovInc stanje))
        )
    ))
)


(defun proveriSveZY4 (x stanje)
    (cond ((= x velicinaTabele) '()) (t 
        (proveriDijagonalu 0 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 0 1 stanje)
        (proveriDijagonalu 3 (1- velicinaTabele) x -1 0 0 -1 -1 0 3 -1 stanje)
        (proveriSveZY4 (1+ x) stanje)
    ))
)

(defun proveriSveZY6 (x stanje)
     (cond ((= x velicinaTabele) '()) (t 
     ; glavna
        (proveriDijagonalu 0 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 0 1 stanje)
        (proveriDijagonalu 0 (- velicinaTabele 2) x (1- velicinaTabele) 0 0 1 -1 0 0 1 stanje)
        (proveriDijagonalu 0 (- velicinaTabele 3) x (- velicinaTabele 2) 0 0 1 -1 0 0 1 stanje)
        (proveriDijagonalu 1 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 1 1 stanje)
        (proveriDijagonalu 2 (1- velicinaTabele) x velicinaTabele 0 0 1 -1 0 2 1 stanje)
        ;sporedna
        (proveriDijagonalu 5 (1- velicinaTabele) x -1 0 0 -1 -1 0 5 -1 stanje)
        (proveriDijagonalu 5 (- velicinaTabele 2) x 0 0 0 -1 -1 0 5 -1 stanje)
        (proveriDijagonalu 5 (- velicinaTabele 3) x 1 0 0 -1 -1 0 5 -1 stanje)
        (proveriDijagonalu 4 (1- velicinaTabele) x -1 0 0 -1 -1 0 4 -1 stanje)
        (proveriDijagonalu 3 (1- velicinaTabele) x -1 0 0 -1 -1 0 3 -1 stanje)
        (proveriSveZY6 (1+ x) stanje)
    ))
)

(defun proveriSveYX4 (z stanje)
    (cond ((= z velicinaTabele) '()) (t 
        (proveriDijagonalu z (1- velicinaTabele) 0 velicinaTabele 0 0 0 -1 1 0 1 stanje)
        (proveriDijagonalu z (1- velicinaTabele) 3 -1 0 0 0 -1 -1 3 -1 stanje)
        (proveriSveYX4 (1+ z) stanje)
    ))
)

(defun proveriSveYX6 (z stanje)
     (cond ((= z velicinaTabele) '()) (t 
     ; glavna
        (proveriDijagonalu z (1- velicinaTabele) 0 velicinaTabele 0 0 0 -1 1 0 1 stanje)
        (proveriDijagonalu z (- velicinaTabele 2) 0 (1- velicinaTabele) 0 0 0 -1 1 0 1 stanje)
        (proveriDijagonalu z (- velicinaTabele 3) 0 (- velicinaTabele 2) 0 0 0 -1 1 0 1 stanje)
        (proveriDijagonalu z (1- velicinaTabele) 1 velicinaTabele 0 0 0 -1 1 1 1 stanje)
        (proveriDijagonalu z (1- velicinaTabele) 2 velicinaTabele 0 0 0 -1 1 2 1 stanje)
     ;sporedna
        (proveriDijagonalu z (1- velicinaTabele) 5 -1 0 0 0 -1 -1 5 -1 stanje)
        (proveriDijagonalu z (- velicinaTabele 2) 5 0 0 0 0 -1 -1 5 -1 stanje)
        (proveriDijagonalu z (- velicinaTabele 3) 5 1 0 0 0 -1 -1 5 -1 stanje)
        (proveriDijagonalu z (1- velicinaTabele) 4 -1 0 0 0 -1 -1 4 -1 stanje)
        (proveriDijagonalu z (1- velicinaTabele) 3 -1 0 0 0 -1 -1 3 -1 stanje)
        (proveriSveYX6 (1+ z) stanje)
    ))
)

(defun proveriSveZX4 (y stanje)
    (cond ((= y velicinaTabele) '()) (t 
        (proveriDijagonalu 0 y 0 velicinaTabele 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 0 y 3 velicinaTabele 0 0 1 0 -1 0 1 stanje)
        (proveriSveZX4 (1+ y) stanje)
    ))
)

(defun proveriSveZX6 (y stanje)
     (cond ((= y velicinaTabele) '()) (t 
     ; glavna
        (proveriDijagonalu 0 y 0 velicinaTabele 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 0 y 1 (1- velicinaTabele) 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 0 y 2 (- velicinaTabele 2) 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 1 y 0 velicinaTabele 0 0 1 0 1 1 1 stanje)
        (proveriDijagonalu 2 y 0 velicinaTabele 0 0 1 0 1 2 1 stanje)
     ;sporedna
        (proveriDijagonalu 0 y 5 velicinaTabele 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 0 y 4 (1- velicinaTabele) 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 0 y 3 (- velicinaTabele 2) 0 0 1 0 1 0 1 stanje)
        (proveriDijagonalu 1 y 5 velicinaTabele 0 0 1 0 1 1 1 stanje)
        (proveriDijagonalu 2 y 5 velicinaTabele 0 0 1 0 1 2 1 stanje)
        (proveriSveZX6 (1+ y) stanje)
    ))
)

(defun proveri3DDijagonale4 (stanje) 
    (proveriDijagonalu 0 3 0 4 0 0 1 -1 1 0 1 stanje)
    (proveriDijagonalu 0 0 0 4 0 0 1 1 1 0 1 stanje)
    (proveriDijagonalu 3 3 0 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 3 0 0 -1 0 0 -1 1 1 3 -1 stanje)
)

(defun proveri3DDijagonale6 (y stanje) 
    ;;(cond ((= y 2) '()) (t 
    ; . . . . . .
    ; . . . . . .
    ; . . . . . .
    ; , , , . . .
    ; , , , . . .
    ; , , , . . .
    
    (proveriDijagonalu 1 5 1 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 2 5 2 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 1 5 2 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 2 5 1 -1 0 0 1 -1 1 5 -1 stanje)

    (proveriDijagonalu 0 5 0 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 0 5 1 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 0 5 2 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 1 5 0 -1 0 0 1 -1 1 5 -1 stanje)
    (proveriDijagonalu 2 5 0 -1 0 0 1 -1 1 5 -1 stanje)
    
    (proveriDijagonalu 0 4 0 -1 0 0 1 -1 1 4 -1 stanje)
    (proveriDijagonalu 0 4 1 -1 0 0 1 -1 1 4 -1 stanje)
    (proveriDijagonalu 0 4 2 -1 0 0 1 -1 1 4 -1 stanje)
    (proveriDijagonalu 1 4 0 -1 0 0 1 -1 1 4 -1 stanje)    
    (proveriDijagonalu 2 4 0 -1 0 0 1 -1 1 4 -1 stanje)

    (proveriDijagonalu 0 3 0 -1 0 0 1 -1 1 3 -1 stanje)
    (proveriDijagonalu 0 3 1 -1 0 0 1 -1 1 3 -1 stanje)
    (proveriDijagonalu 0 3 2 -1 0 0 1 -1 1 3 -1 stanje)
    (proveriDijagonalu 1 3 0 -1 0 0 1 -1 1 3 -1 stanje)    
    (proveriDijagonalu 2 3 0 -1 0 0 1 -1 1 3 -1 stanje)

    ; . . . . . .
    ; . . . . . .
    ; . . . . . .
    ; . . . , , ,
    ; . . . , , ,
    ; . . . , , ,
    
    (proveriDijagonalu 1 5 3 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 2 5 3 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 1 5 4 -1 0 0 1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 2 5 4 -1 0 0 1 -1 -1 4 -1 stanje)

    (proveriDijagonalu 0 5 3 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 0 5 4 -1 0 0 1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 0 5 5 -1 0 0 1 -1 -1 5 -1 stanje)
    (proveriDijagonalu 1 5 5 -1 0 0 1 -1 -1 5 -1 stanje)
    (proveriDijagonalu 2 5 5 -1 0 0 1 -1 -1 5 -1 stanje)
    
    (proveriDijagonalu 0 4 3 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 0 4 4 -1 0 0 1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 0 4 5 -1 0 0 1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 1 4 5 -1 0 0 1 -1 -1 4 -1 stanje)    
    (proveriDijagonalu 2 4 5 -1 0 0 1 -1 -1 4 -1 stanje)

    (proveriDijagonalu 0 3 3 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 0 3 4 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 0 3 5 -1 0 0 1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 1 3 5 -1 0 0 1 -1 -1 3 -1 stanje)    
    (proveriDijagonalu 2 3 5 -1 0 0 1 -1 -1 3 -1 stanje)

    ; , , , . . .
    ; , , , . . .
    ; , , , . . .
    ; . . . . . .
    ; . . . . . .
    ; . . . . . .
    
    (proveriDijagonalu 3 5 1 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 3 5 2 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 4 5 2 -1 0 0 -1 -1 1 4 -1 stanje)
    (proveriDijagonalu 4 5 1 -1 0 0 -1 -1 1 4 -1 stanje)

    (proveriDijagonalu 5 5 0 -1 0 0 -1 -1 1 5 -1 stanje)
    (proveriDijagonalu 4 5 0 -1 0 0 -1 -1 1 4 -1 stanje)
    (proveriDijagonalu 3 5 0 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 5 5 1 -1 0 0 -1 -1 1 5 -1 stanje)
    (proveriDijagonalu 5 5 2 -1 0 0 -1 -1 1 5 -1 stanje)
    
    (proveriDijagonalu 5 4 0 -1 0 0 -1 -1 1 4 -1 stanje)
    (proveriDijagonalu 4 4 0 -1 0 0 -1 -1 1 4 -1 stanje)
    (proveriDijagonalu 3 4 0 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 5 4 1 -1 0 0 -1 -1 1 4 -1 stanje)    
    (proveriDijagonalu 5 4 2 -1 0 0 -1 -1 1 4 -1 stanje)

    (proveriDijagonalu 5 3 0 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 4 3 0 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 3 3 0 -1 0 0 -1 -1 1 3 -1 stanje)
    (proveriDijagonalu 5 3 1 -1 0 0 -1 -1 1 3 -1 stanje)    
    (proveriDijagonalu 5 3 2 -1 0 0 -1 -1 1 3 -1 stanje)

    ; . . . , , ,
    ; . . . , , ,
    ; . . . , , ,
    ; . . . . . .
    ; . . . . . .
    ; . . . . . .
    
    (proveriDijagonalu 3 5 3 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 3 5 4 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 4 5 3 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 4 5 4 -1 0 0 -1 -1 -1 4 -1 stanje)

    (proveriDijagonalu 5 5 3 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 4 5 5 -1 0 0 -1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 3 5 5 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 5 5 4 -1 0 0 -1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 5 5 5 -1 0 0 -1 -1 -1 5 -1 stanje)
    
    (proveriDijagonalu 5 4 3 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 4 4 5 -1 0 0 -1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 3 4 5 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 5 4 4 -1 0 0 -1 -1 -1 4 -1 stanje)
    (proveriDijagonalu 5 4 5 -1 0 0 -1 -1 -1 4 -1 stanje)

    (proveriDijagonalu 5 3 3 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 4 3 5 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 3 3 5 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 5 3 4 -1 0 0 -1 -1 -1 3 -1 stanje)
    (proveriDijagonalu 5 3 5 -1 0 0 -1 -1 -1 3 -1 stanje)
    

        ;(proveri3DDijagonale6 (1- y))
    ;))
)

(defun proveriPotezAlfaBeta (potez stanje boja)
        
        ;;(potpunPrikaz tabelaZaIspitivanje)
        (setq rbrReda (floor potez velicinaTabele))
        (setq rbrStubica (mod potez velicinaTabele))
        (cond ((or (>= rbrReda velicinaTabele) (< rbrReda 0)) '())
            (t 
            ;;(princ (nth rbrStubica (nth 0 (nth rbrReda tabelaZaIspitivanje))))
            (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda stanje))) '-) 
                        (odigrajPotez rbrReda rbrStubica (- velicinaTabele 1) stanje boja)
                        stanje
                    )
                (t '()))
            )
        )
    

)
;(defun alfaBeta (stanje dubina alfa beta igracNaPotezu)
 ;   (format t "~%")
  ;  (princ "DUBINA: " ) (princ dubina) (format t "~%")
   ; (princ "IGRAC NA POTEZU: ") (princ igracNaPotezu) (format t "~%")
    ;(print "UNUTAR ALFA BETA") (potpunPrikaz stanje)
    ;(cond ((or (= dubina 0) (jeKrajIgre 0 0 stanje)) (print "ALFA BETA ZAVRSIO") (list (proveriPoene stanje) -1))
    ;    (t 
    ;        (cond ((equal igracNaPotezu 'X)
    ;            (let ((vrednost (list most-negative-fixnum -1)))
    ;                (xIgra (copy-tree stanje) dubina alfa beta igracNaPotezu (ispitajPoteze stanje) vrednost)
    ;            )
    ;        )
    ;        ((equal igracNaPotezu 'O)
    ;            (let ((vrednost (list most-positive-fixnum -1)))
    ;                (oIgra (copy-tree stanje) dubina alfa beta igracNaPotezu (ispitajPoteze stanje) vrednost)
   ;             )
  ;          )
  ;          ) 
  ;  )
  ;  )
    
;)

(defun promeniIgracaAlfaBeta (value) 
    (if (equal value 'X) 'O 'X)
)

(defun alfaBeta (stanje potez alfa beta trenutnaDubina igracNaPotezu)
    (cond
        ((zerop trenutnaDubina) (list potez (heuristika stanje igracNaPotezu)))
        (t
            (let*
                (
                    (listaPoteza (ispitajPoteze stanje))
                    (vrednost
                        (cond ((equal igracNaPotezu 'X)
                                (maxIgrac listaPoteza '() trenutnaDubina alfa beta stanje igracNaPotezu))
                            (t
                                (minIgrac listaPoteza '() trenutnaDubina alfa beta stanje igracNaPotezu)
                            )
                        ) 
                    )
                )
                
                ;(format t "~%GENERATED potezS: ~a~%" listaPoteza)
                (cond
                    ((null listaPoteza) (list potez (proveriPoene stanje)))
                    ((equalp trenutnaDubina dubina) (car vrednost))
                    (t (list potez (cadr vrednost)))
                )
            
            )
        )       
    )
)

(defun maxIgrac (listaPoteza najboljiPotez dubina alfa beta predhodnoStanje igracNaPotezu)
    (cond 
        ((null listaPoteza) (list najboljiPotez alfa))
        (t ;(potpunPrikaz predhodnoStanje)
            (let*
                (                
                    (sledeceStanje (proveriPotezAlfaBeta (car listaPoteza) (copy-tree predhodnoStanje) igracNaPotezu))
                    (minpotez (alfaBeta sledeceStanje (car listaPoteza)  alfa beta (1- dubina) (promeniIgracaAlfaBeta igracNaPotezu)))
                    (noviPotez (if (>= alfa (cadr minpotez)) (list najboljiPotez alfa) minpotez))
                )
                (cond ((or (> (cadr noviPotez) beta) (null listaPoteza))
                          (list najboljiPotez (cadr noviPotez)))
                        (t
                            (maxIgrac (cdr listaPoteza) (car noviPotez) dubina (cadr noviPotez) beta predhodnoStanje igracNaPotezu)
                        )
                )
            )
        )
    )
)

(defun minIgrac (listaPoteza najboljiPotez dubina alfa beta predhodnoStanje igracNaPotezu)
    (cond 
        ((null listaPoteza) (list najboljiPotez beta))
        (t ;(potpunPrikaz predhodnoStanje)
            (let*
                (    
                    (sledeceStanje (proveriPotezAlfaBeta (car listaPoteza) (copy-tree predhodnoStanje) igracNaPotezu))     
                    (maxpotez (alfaBeta sledeceStanje (car listaPoteza) alfa beta (1- dubina) (promeniIgracaAlfaBeta igracNaPotezu)))
                    (noviPotez (if (<= beta (cadr maxpotez)) (list najboljiPotez beta) maxpotez))
                )
                (cond ((or (< (cadr noviPotez) alfa)(null (cdr listaPoteza)))
                        (list najboljiPotez (cadr noviPotez)))
                        (t
                            (minIgrac (cdr listaPoteza) (car noviPotez) dubina alfa (cadr noviPotez) predhodnoStanje igracNaPotezu)
                        )
                )
            )
        )
    )
)


(defun !daLiXPobedjuje (stanje)
    (cond ((> (proveriPoene stanje) 0) t) (t '()))
)

(defun !daLiOPobedjuje (stanje)
    (cond ((< (proveriPoene stanje) 0) t) (t '()))
)

(defun !daLiXDobijaPoene (stanje)
    (let* 
        (
            (praviPoeni (proveriPoene tabela))
            (noviPoeni (proveriPoene stanje))
        )
        (progn
            (cond ((> noviPoeni praviPoeni) t) (t '()))
        )
    )
)

(defun !daLiODobijaPoene (stanje)
    (let* 
        (
            (praviPoeni (proveriPoene tabela))
            (noviPoeni (proveriPoene stanje))
        )
        (progn
            (cond ((< noviPoeni praviPoeni) t) (t '()))
        )
    )
)

(defparameter *RULES*
    '(
        (if (!daLiXPobedjuje novoStanje) then (XPobednik))
        (if (!daLiOPobedjuje novoStanje) then (OPobednik))
        (if (!daLiXDobijaPoene novoStanje) then (XVodi))
        (if (!daLiODobijaPoene novoStanje) then (OVodi))
    )

)

(defun generisiCinjenice (stanje z y x)
    (cond ((= z velicinaTabele) (setq z 0) (incf y)))
    (cond ((= y velicinaTabele) (setq y 0) (incf x)))
    (cond ((= x velicinaTabele) '())
        (t (cons
             (list 'Na z y x (nth z (nth y (nth x stanje))))
             (generisiCinjenice stanje (1+ z) y x)
            )   
        )
    )
)

(defun heuristika (stanje igracNaPotezu)
    (setq novoStanje (copy-tree stanje))
    (let* 
        (
            (*FACTS* (generisiCinjenice stanje 0 0 0))
        )
        (progn
            (prepare-knowledge *RULES* *FACTS* 10)
            (let* 
                (
                    (pravilo1 (if (/= (count-results '(XPobednik)) 0) (if (equalp igracNaPotezu 'X) 1000 -1000) 0))
                    (pravilo2 (if (/= (count-results '(OPobednik)) 0) (if (equalp igracNaPotezu 'O) -1000 1000) 0))
                    (pravilo3 (if (/= (count-results '(XVodi)) 0) (if (equalp igracNaPotezu 'X) 500 -500) 0))
                    (pravilo4 (if (/= (count-results '(OVodi)) 0) (if (equalp igracNaPotezu 'O) -500 500) 0))
                    (acc (+ pravilo1 pravilo2 pravilo3 pravilo4))
                )
                (progn
                    acc
                )
            )
        )
    )
)





;(trace alfaBeta)
;(trace maxIgrac)
;(trace minIgrac)
;;(princ (ispitajPoteze matrica))
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