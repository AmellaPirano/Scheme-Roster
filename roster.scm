;Samantha Puckett
;sep13b

(define ano-read-2-items
  (lambda (n x)
     (cond ((= n 0) (begin
                      (display "\tInput ID: ")
                      (ano-read-2-items 1 (list (read-line)))
                     ))
			((= n 1) (begin
                      (display "\tInput Name: ")
                      (ano-read-2-items 2 (list (car x) (read-line)))
                     ))		 
           ((= n 2) (begin
                      (display "\tInput Grade: ")
                      (list (car x) (car (cdr x)) (read-line))
                     ))
      )
  )
)

(define write-to-file
	(lambda (roster)
		(display "\tStore roster to file: ")
		(define f (open-output-file (read-line)))
		(write roster f)
		(close-port f)
	)
)

(define find-by-ID
	(lambda (ID roster original)
		(cond ((pair? roster)
						(if (equal? ID (car (car roster))) (print-record (car roster)) (find-by-ID ID (cdr roster) original))
						)
			  ((null? roster)
						(find-by-name ID original)
						)
		
		)	
	)
)

(define find-by-name
	(lambda (ID roster)
		(cond ((pair? roster)
						(if (equal? ID (cadr (car roster))) (print-record (car roster)) (find-by-name ID (cdr roster)))
						)
			  ((null? roster)
						(display "Entry not found")
						)
		
		)
	)
)

(define sort-by-ID
	(lambda (roster n)
		(cond ((pair? roster)
						(display "\n\tNo.")
						(display n)
						(display ": ")
						(print-record (smallest-ID roster (car roster)))
						(sort-by-ID (remove (car (smallest-ID roster (car roster))) roster roster) (+ n 1))
						)
			  ((null? roster))
		
		)
	)
)

(define smallest-ID
	(lambda (lst min)
		(cond ((null? lst) min)
			((< (string->number(car (car lst))) (string->number(car min))) (smallest-ID(cdr lst)(car lst)))
			(else (smallest-ID(cdr lst) min))
		)
	)
)

(define sort-by-name
	(lambda (roster n)
		(cond ((pair? roster)
						(display "\n\tNo.")
						(display n)
						(display ": ")
						(print-record (smallest-name roster (car roster)))
						(sort-by-name (remove (car (smallest-name roster (car roster))) roster roster) (+ n 1))
						)
			  ((null? roster))
		
		)
	)
)

(define smallest-name
	(lambda (lst min)
		(cond ((null? lst) min)
			((string<? (cadr (car lst)) (cadr min)) (smallest-name(cdr lst)(car lst)))
			(else (smallest-name(cdr lst) min))
		)
	)
)

(define sort-by-grade
	(lambda (roster n)
		(cond ((pair? roster)
						(display "\n\tNo.")
						(display n)
						(display ": ")
						(print-record (smallest-grade roster (car roster)))
						(sort-by-grade (remove (car (smallest-grade roster (car roster))) roster roster) (+ n 1))
						)
			  ((null? roster))	
		)
	)
)

(define smallest-grade
	(lambda (lst min)
		(cond ((null? lst) min)
			((< (string->number(car (cddr (car lst)))) (string->number(car (cddr min)))) (smallest-grade(cdr lst)(car lst)))
			(else (smallest-grade(cdr lst) min))
		)
	)
)

(define remove
	(lambda (ID roster original)
		(cond ((null? roster) (display "\tEntry not found\n"))
			  ((equal? ID (car (car roster))) (cdr roster))
			  (else (cons (car roster) (remove ID (cdr roster) original))) 
		)	
	)
)

(define remove-by-name
	(lambda (ID roster)
		(cond ((null? roster) (display "\tEntry not found\n"))
			  ((equal? ID (cadr (car roster))) (cdr roster))
			  (else (cons (car roster) (remove-by-name ID (cdr roster)))) 
		)
	)
)

(define print-record
	(lambda (record)
		(display "ID=")
		(display (car record))
		(display ", Name=")
		(display (cadr record))
		(display ", Grade=")
		(display (car (cddr record)))
		(newline)
	)
)

(define performtask
  (lambda (n roster) 
    (cond ((= n 0) (begin
                    (display "\tRoster reset (now empty).\n")
                    (menu '())
                    ))
          ((= n 1) (begin
					(display "\tLoad roster from file: ")
                    (menu (read (open-input-file (read-line))))
                    ))
          ((= n 2)	(begin
					(write-to-file roster)
					(menu roster)
					))
		  ((= n 3)	(begin
					(display "Displaying roster, sorted by ID")
					(sort-by-ID roster 1)
					(menu roster)
					))
		  ((= n 4)	(begin
					(display "Displaying roster, sorted by name")
					(sort-by-name roster 1)
					(menu roster)
					))
		  ((= n 5)	(begin
					(display "Displaying roster, sorted by grade")
					(sort-by-grade roster 1)
					(menu roster)
					))
		  ((= n 6)	(begin
					(display "\n\tEnter student ID or name: ")
					(display "\n\t")
					(find-by-ID (read-line) roster roster)
					(newline)
					(menu roster)
					))
		  ((= n 7)	(begin
					(display "\n\tAdd a student to the class roster:\n")
					(newline)
					(menu (cons (ano-read-2-items 0 '()) roster))
					))
		  ((= n 8)	(begin
					(display "\n\t Enter student ID: ")
					(menu (remove (read-line) roster roster))
					))
		  ((= n 9) (begin
					(display "\n\t Enter name: ")
					(menu (remove-by-name (read-line) roster))
					))
		  ((= n 10) (begin
                    (display "\n\tExiting...\n")
                    #t
                    ))
           (else (begin
                    (display "\n\tOption #")
                    (display n)
                    (display " does not exist.\n\n")
                    (menu roster)
                  )
            )
     )
   )
)

(define menu
  (lambda (roster)
     (begin
		(display "\t Class roster management system \n")
        (display "\t============================\n")
        (display "\t   MENU\n")
        (display "\t============================\n")
        (display "\t0. Reset roster\n")
        (display "\t1. Load roster from file\n")
		(display "\t2. Store roster to file\n")
		(display "\t3. Display roster sorted by ID\n")
		(display "\t4. Display roster sorted by name\n")
		(display "\t5. Display roster sorted by grade\n")
		(display "\t6. Display student info\n")
		(display "\t7. Add a student to roster\n")
		(display "\t8. Remove a student from roster by ID\n")
		(display "\t9. Remove a student from roster by name\n")
        (display "\t10. Exit\n\n")
        (display "\tEnter your choice: ")
        (performtask (read) roster)
      )
   )
)