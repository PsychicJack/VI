(setq  polja '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
(setq matrica '(((0 1 2 3) (4 5 6 7) (8 9 A B) (C D E F)) ((G H I J) (K L M N) (O P Q R) (S T U V)) ((W X Y Z) (a b c d) (e f g h) (i j k l)) ((m - o p) (q - s t) (u v w x) (y z - +))))

(defun main()
    
    ;;(princ "Size of table: ")
    ;;(setq tableSize (read))

    (setq trentuniIgrac 'H)
    (setq trenutnaBoja 'X)
    ;;(odabirPrvogIgraca)
    ;;(odabirBoje)

    (setq tableSize 6)
    (setq table (initializeTable '0))
    ;;(princ "---------")

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

(defun initializeTable (rowIndex)
    (cond
        ((>= rowIndex tableSize) '())
        (t
            (cons  (initializeTableRow rowIndex tableSize) 
                (initializeTable (+ 1 rowIndex))
            )
        )
     ) 
)
(defun initializeTableRow (rowIndex columnIndex)
    (cond 
        ((<= columnIndex 0) '())
		(t (if (= 4 tableSize)
     			(cons (list '- '- '- '-) (initializeTableRow rowIndex (- columnIndex 1))) 
     			(if (= 6 tableSize)
				(cons (list '- '- '- '- '- '-) (initializeTableRow rowIndex (- columnIndex 1)))
				(format t "Nije uneta ispravna vrednost za velicinu kocke")
			))
     		)
    )
)

(defun potpunPrikaz ()
(cond ((= tableSize 4) (format t "0123456789ABCDEF")) ((= tableSize 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(format t "~%")
(prikazStanja tableSize tableSize 0 tableSize)
(cond ((= tableSize 4) (format t "0123456789ABCDEF")) ((= tableSize 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
)

(defun prikazStanja (od do ind1 ind2)
    (decf ind2)
    (cond ((= ind2 -1) (setq ind2 0) (incf ind1)))
    (decf od)
    (cond ((= od -1) (setq od 0) (decf do)))
    (cond ((= do 0) '()) (t (stampajRed od do 0 ind1 ind2) (format t "~%") (prikazStanja od do ind1 ind2)))
)
 
(defun stampajRed (od do count ind1 ind2)

    (cond ((= count tableSize) '()) 
    (t  (stampajBlankoOd od '0) 
        (stampajPolja od do count (1- ind1) (1- ind2))
        (stampajBlankoDo do tableSize)
        
        (stampajRed od do (1+ count) ind1 ind2)
    )
    )
)
 
(defun stampajBlankoOd (od trenutni)
    (cond ((= od trenutni) '()) (t (format t " ") (stampajBlankoOd od (1+ trenutni))))
)

(defun stampajPolja (od do red ind1 ind2)
    (incf ind1)
    (incf ind2)
    (cond ((= od do) '())
    (t (princ (nth ind2 (nth ind1 (nth red table)))) (stampajPolja (1+ od) do red ind1 ind2))
    )
)

(defun stampajBlankoDo (stampajDo granica) 
    (cond ((= stampajDo granica) '()) (t (format t " ") (stampajBlankoDo (1+ stampajDo) granica))) 
)

(defun potez ()
    (potpunPrikaz)
    (format t "~%Potez: ")
    (setq potez (read))
    (proveriPotez (write-to-string potez))
    (potez)
)

(defun proveriPotez (potez) 
    (setq temp (- (char-code (char potez 0)) 55))
    (cond ((< temp 10) (incf temp 7)))
    (setq rbrReda (floor temp tableSize))
    (setq rbrStubica (mod temp tableSize))
    ;;(princ rbrReda) (princ rbrStubica)
    (cond ((or (>= rbrReda tableSize) (< rbrReda 0)) (format t "Nepravilan potez ~%") '()) 
        (t 
            (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda table)) ) '-) (odigrajPotez rbrReda rbrStubica (- tableSize 1)) (promeniIgraca)) (t (format t "Nepravilan potez ~%") '()))
        )
    )
)

(defun odigrajPotez (rbrReda rbrStubica trenutni) ;;ne proverava da li ima slobodnih mesta, jer je to provereno u proveriPotez
    (cond ((equal  (nth rbrStubica (nth trenutni (nth rbrReda table))) '-)
    (setf  (nth rbrStubica (nth trenutni (nth rbrReda table))) trenutnaBoja))
     (t (odigrajPotez rbrReda rbrStubica (1- trenutni)))
    )
)

(defun promeniIgraca () 
(cond ((equal trentuniIgrac 'H) (setq trentuniIgrac 'C)) ((equal trentuniIgrac 'C) (setq trentuniIgrac 'H)))
(cond ((equal trenutnaBoja 'X) (setq trenutnaBoja 'O)) ((equal trenutnaBoja 'O) (setq trenutnaBoja 'X)))
)


(defun jeKrajIgre () 

)

(main)

;;(nth ind2 (nth ind1 (nth red matrica)))
;;(princ (- (char-code (char "A" 0)) 55))


