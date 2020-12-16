(setq  polja '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
(setq matrica '(((- - - -)(1 2 3 4))))

(defun main()
 (princ "Size of table: ")
  (setq tableSize (read))
  (setq table (initializeTable '1))
)

(defun initializeTable (rowIndex)
    (cond
        ((> rowIndex '4) '())
        (t
            (cons 
                (cons
                     rowIndex
                    (list (reverse (initializeTableRow rowIndex tableSize)))
                )
                (initializeTable (+ 1 rowIndex))
            )
        )
     )
)
(defun initializeTableRow (rowIndex columnIndex)
    (cond 
        ((<= columnIndex 0) '())
		(t (if(= 4 tableSize)
     			(cons (cons columnIndex '(- - - -)) (initializeTableRow rowIndex (- columnIndex 1))) 
     			(if (= 6 tableSize)
				(cons (cons columnIndex '(- - - - - -)) (initializeTableRow rowIndex (- columnIndex 1)))
				((format t "Nije uneta ispravna vrednost za velicinu kocke"))
			))
     		)
     )
)


(main)