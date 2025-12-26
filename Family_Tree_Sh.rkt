;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Comp 1811 Scheme Project|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;Your full name: Davier Udin Bagum
;;Student ID: 001393001-X
;;Date of birth (day/month/year): 11/06/2006

;;Your full name: Md Abdullah Al Mamun
;;Student ID: 001427708-5
;;Date of birth (day/month/year): 10/06/2001

;;Data format: Name, Mother, Father, Date of birth, Date of death.
;;An empty list means Unknown.


;; Maternal branch
(define Mb
  '(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
    ((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
    ((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
    ((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
    ((Ned Bloom) (() ()) ((23 4 2001) ()))
    ((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))
    ((Ada West) (() ()) ((22 8 1973) ()))
    ((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
    ((Mary Jones) (() ()) ((12 5 1967) (19 5 2024)))
    ((Tom Blake) (() ()) ((17 1 1964) ()))) )

;; Paternal branch
(define Pb
  '(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021)))
    ((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
    ((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
    ((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
    ((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
    ((John Doe) (() ()) ((18 2 1899) (7 7 1970)))
    ((Lisa Brown) (() ()) ((31 6 1904) (6 3 1980)))
    ((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
    ((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
    ((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))


;; This sections is used to separate the outputs which are the names so that the names are clearly understandable and
;; everything is not mushed toghether
(define (join lst sep)
  (cond
    ((null? lst) "") ;; this section is used for if the list is empty
    ((null? (cdr lst)) (car lst)) ;; and this section is for if in the list there is only one element which would not need a separator
    (else (string-append (car lst) sep (join (cdr lst) sep))))) ;; finally this section separates the elements if there are more than one element

;; This turns the names from list to string so the output can be shown properly
(define (format-name name-list)
  (string-append (symbol->string (car name-list)) ;; in here we tae the first name and turn it into a string
                 " " ;; this is to add the space between first name and last name
                 (symbol->string (cadr name-list)))) ;; same thing done for last name , finally string-append is used to put it all toghether

;; This section is to put a coma between names
(define (format-members lst) ;; this section uses the function above to get the name in string format
  (join (map format-name lst) ", ")) ;; using the join function we use this section to add a come and spasce in between each name

;;Coma between numbers
(define (format-numbers lst)
  (join (map number->string lst) ", ")) ;; same as above but for the age outputs so that they have comas inbetween them aswell

;; remove-duplicates: remove duplicate elements from a list.
(define (remove-duplicates lst)
  (if (null? lst) ;; if the list is empty it will return an empt list
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) (not (equal? x (car lst)))) (cdr lst)))))) ;; for some outputs i had few duplicate names, so using open ai i created this part
                                                                                              ;; which uses filter and makes a new list filtering out duplicates

;; C1, C2, C3

;; C1
(define (C1)
  (format-members (map car Mb))) ;; makes sure the outputs are separated and with comas using the functions above

;; C2
(define (C2)
  (format-members (map car Pb))) ;; same as above

;; C3
(define (C3)
  (string-append (C1) ", " (C2))) ;; uses string-append to link the formatted outputs of C1 and C2 with a come inbetween

;; A1-A6 function section

;; A1: Return a list of all parents 
(define (parents lst)
  (apply append
         (map (lambda (person)
                (filter (lambda (x) (not (null? x))) (cadr person)))
              lst))) ;; this section gets the parents that are in parenthesis in the Mb and Pb lists, using the lamba function, people who are not
                     ;; parents are filtered out, and finally the names are combined into one list and is displayed

;; A2: Return all living members of the branch.
(define (living-members lst)
  (filter (lambda (person) (alive? (cadr (caddr person)))) lst)) ;; using the third element of the lists which are the dates, using lambda, death date is checkd to see
                                                                 ;; if its empty and then filter is used to get rid of dead people

;; A3: Return a list of the current age of all living members

(define (calculate-age birth-date death-date)
  (if (null? death-date)
      (- 2025 (caddr birth-date))
      (- (caddr death-date) (caddr birth-date))))

;; Helper to format a living member with age.
(define (format-name-age person)
  (string-append (format-name (car person))
                 " " 
                 (number->string (calculate-age (car (caddr person))
                                                (cadr (caddr person))))))

;; A3 C: Combine living members from both branches and format.
(define (current-age-all)
  (join (map format-name-age (append (living-members Mb) (living-members Pb))) ", "))


;; A4: gives the members with same birth month
(define (same-birthday-month lst month)
  (filter (lambda (person) (= (get-month (car (caddr person))) month)) lst)) ;; this section get the birth month of each member and using filter and lambda
                                                                             ;; checks if the month which is in the format of number is the same as my one 

;; A5: sorted bylast name members of Mb
(define (sort-by-last lst)
  (sort-by-last-name lst)) ;; uses the function sort-by-last-name for the sort-by-last function

;; A6: any name containing john is changed to juan
(define (change-name-to-Juan lst)
  (change-name lst "John" "Juan")) ;; uses the change-name function to change any johj to juan

;; B1-B6 function section

;; B1: Return a list of all the children 
(define (children lst)
  (map car lst)) ;; using map car and list it extracts the first element of each list as that is the child of two people

;; B2: Return the oldest living member.
(define (oldest-living-member lst)
  (let ((living (living-members lst)))
    (let loop ((current (car living))
               (rest (cdr living)))
      (if (null? rest)
          current
          (let ((next (car rest)))
            (if (> (calculate-age (car (caddr next)) (cadr (caddr next)))
                   (calculate-age (car (caddr current)) (cadr (caddr current))))
                (loop next (cdr rest))
                (loop current (cdr rest)))))))) ;; in this section it first gets all living members and then takes each member and compares the birth date
                                                ;; of each person using a recursive loop and the oldest living member is stored in current when the loop is
                                                ;; finished and returns the oldest lving member



;; B3: Return the average age on death of Pb
(define (average-age-on-death lst)
  (let ((dead (filter (lambda (person) (not (alive? (cadr (caddr person))))) lst))) ;; filters out the dead members in Pb as livings members are no use here
    (if (null? dead)
        0 ;; prevention measure for if the list is empty
        (exact->inexact ;; the output first came as 299/4 but with the help of Ai i found how to turn it into decimal
         (/ (apply + (map (lambda (person) ; adds up the ages at death and divides it by members to find the average
                            (calculate-age (car (caddr person))
                                           (cadr (caddr person))))
                          dead)) ;; get the birth date and death date and fins out the age at death
            (length dead))))))


;; B4: gives the members with same birth month
(define (birthday-month-same lst month)
  (same-birthday-month lst month)) ;; same as A4

;; B5: sorted members by first name in Pb.
(define (sort-by-first lst)
  (sort-by-first-name lst)) ;; same as A5 but using the function for first name instead of last name

;; B6: change any name from mary to maria
(define (change-name-to-Maria lst)
  (change-name lst "Mary" "Maria")) ;; same as A6 but instead og jhon is mary with maria


;; this is to assist B1 as members with no parents lissted i didnt consider them children in the branch
(define (children-with-parents lst)
  (map car 
       (filter (lambda (person)
                 (not (equal? (cadr person) '(() ())))
               )
               lst))) ;; checks if the member has parents and uses filter and lambda so that only people with parents are outputed

;; Function to check if a person is alive.
(define (alive? death-date)
  (null? death-date))

;; Function to get the month from a date.
(define (get-month date)
  (cadr date)) ;; function to get the mont from the integer

;; Function to sort by last name.
(define (sort-by-last-name lst)
  (sort lst (lambda (x y)
              (string<? (symbol->string (cadr (car x)))
                         (symbol->string (cadr (car y))))))) ;; extracts the last name from each member and converts it to strings and then compares other members alphabetically
                                                             ;; to get the order

;; Function to sort by first name.
(define (sort-by-first-name lst)
  (sort lst (lambda (x y)
              (string<? (symbol->string (car (car x)))
                         (symbol->string (car (car y)))))))  ;; same as above but with first name

;; Function to change a name in the family tree.
(define (change-name lst old-name new-name)
  (map (lambda (person)
         (if (equal? (symbol->string (car (car person))) old-name)
             (list (cons (string->symbol new-name) (cdr (car person))) (cdr person))
             person))
       lst))

;; output display section (unfortunately i was not able to make it request output)

(begin
  (display "C1: All members in Maternal Branch: ")
  (display (C1))
  (newline) (newline)
  
  (display "C2: All members in Paternal Branch: ")
  (display (C2))
  (newline) (newline)
  
  (display "C3: All members in Both Branches: ")
  (display (C3))
  (newline) (newline)
  
  (display "A1: Parents in Both Branches: ")
  (display (format-members (remove-duplicates (append (parents Mb) (parents Pb)))))
  (newline) (newline)
  
  (display "A2: Living Members in Both Branches: ")
  (display (format-members (remove-duplicates (append (map car (living-members Mb))
                                                      (map car (living-members Pb))))))
  (newline) (newline)
  
  (display "A3: Current Age of Living Members in Both Branches (Name Age): ")
  (display (current-age-all))
  (newline) (newline)
  
  (display "A4: Members with Birthday in June: ")
(display (format-members (map car (append (same-birthday-month Mb 6)
                                           (same-birthday-month Pb 6)))))
(newline) (newline)

  
  (display "A5: Sorted by Last Name: ")
  (display (format-members (map car (sort-by-last Mb))))
  (newline) (newline)
  
  (display "A6: Change John to Juan (Both Branches): ")
(display (format-members (append (map car (change-name-to-Juan Mb))
                                  (map car (change-name-to-Juan Pb)))))
(newline) (newline)
  
  (display "B1: Children in Both Branches: ")
(display (format-members (append (children-with-parents Mb)
                                 (children-with-parents Pb))))
(newline) (newline)
  
  (display "B2: Oldest Living Member in Paternal Branch: ")
  (let ((old (oldest-living-member Pb)))
    (if (not (eq? old #f))
        (display (format-name (car old)))
        (display "None")))
  (newline) (newline)
  
  (display "B3: Average Age on Death in Paternal Branch: ")
  (display (average-age-on-death Pb))
  (newline) (newline)
  
  (display "B4: Members with Birthday in June : ")
(display (format-members (map car (append (birthday-month-same Mb 6)
                                          (birthday-month-same Pb 6)))))
(newline) (newline)

  
  (display "B5: Sorted by First Name: ")
  (display (format-members (map car (sort-by-first Pb))))
  (newline) (newline)
  
  (display "B6: Change Mary to Maria (Both Branches): ")
(display (format-members (append (map car (change-name-to-Maria Mb))
                                  (map car (change-name-to-Maria Pb)))))
(newline) (newline))
