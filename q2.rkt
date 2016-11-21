#lang racket
(provide (all-defined-out))
;(define inputFile(vector-ref(current-command-line-arguments)0))
;(define inputFile "/home/fduser/ppl/input.txt")
(define wordlist (file->list "test.txt"))      ; Finally use inputFile instead of test.txt

(display wordlist)

; setting up the counter for id purpose ;
(define count 0)
(define (id_counter)
  (set! count (+ 1 count))
  count
  )

(define (get_newnode member)
  (list (list (id_counter) (list member) '() )  )
  )

; implementing step 2 ;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define clusterList '() )                                                               ;
(define (create_clusterList wordlist ls)                                                 ;   
  (if (null? wordlist)                                                                   ;
      ls                                                                                 ; 
      (create_clusterList (cdr wordlist) (append ls (get_newnode (car wordlist))))       ;  
      )   
  )                                                                                      ; 
(define clusterList (create_clusterList wordlist '()))                                  ;
(define (step2)                      
  (print clusterList)                                                                   ;
  )
;(display (step2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          


;;;;;;;;;;;;;;;;;;;;;;;;;;; implementing step 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (WtoWdistance aa bb)
  (- 1 (/ (length(set-intersect (string->list (symbol->string aa)) (string->list(symbol->string bb))))  (length(set-union (string->list (symbol->string aa)) (string->list(symbol->string bb)))) ))
  )
(define (minim lst)
    (cond ((null? (cdr lst)) (car lst))
          ((< (car lst) (minim (cdr lst))) (car lst))
          (else (minim (cdr lst)))) )

(define (distance_point_list a l2_list acc_list)
  (if (null? l2_list) acc_list
      (distance_point_list a (cdr l2_list) (append acc_list  (list (WtoWdistance a (car l2_list))) )  )
      )
  )

(define (create_distList l1 l2 distList)
  (if (null? l1)
      distList
      (create_distList (cdr l1) l2 (append distList (distance_point_list (car l1) l2 '())) )
      )
  )

(define (CtoCdistance a b)
  (create_distList (list-ref a 1) (list-ref b 1) '() )
 )

(define (get_token c1 c2)
  (if (= (list-ref c1 0) (list-ref c2 0))
      (list (list (list-ref c2 0) 100) )
      (list (list (list-ref c2 0) (car(CtoCdistance c1 c2 ))))
      )
  )

(define (return_update_cluster cl ls)
  (list(list (list-ref cl 0) (list-ref cl 1) ls))
  )

(define (get_list cluster l2 acc_list2)
  (if (null? l2)
      (return_update_cluster cluster acc_list2)
      (get_list cluster (cdr l2) (append acc_list2 (get_token cluster (car l2))))
      )
  )

(define (update_distances l1 l2 acc_list)
  (if (null? l1)
      acc_list
      (update_distances (cdr l1) l2 (append acc_list (get_list (car l1) l2 '()))) 
      )
  )

(define updated_clusterList (update_distances clusterList clusterList '()) )

(define (step3)
   updated_clusterList
  )
(display(step3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;implementing step-4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; 4(i) ;;;;
;;;;;;;;;;;;;;;;
(define min_d 999)         ;define global variables
(define cluster1 -1)
(define cluster2 -1)
(define newest_cl_id 0)

(define (min_d_in_cluster ls id_cl)                                    ; to find the minimum distance in the distance-list of each cluster
  (cond
    ( (not(null?  ls)) (begin
                              (if (> min_d (list-ref (car ls) 1))      ; compare with the global variable to set the first occurence of min distance and the resp clusters
                                           (begin 
                                             (set! min_d (list-ref (car ls) 1))
                                             (set! cluster1 id_cl)
                                             (set! cluster2 (list-ref (car ls) 0))                                           
                                            )                                        
                                           (min_d_in_cluster (cdr ls) id_cl)
                               )                                                           
  ))))


(define (nearest_cluster updated_cL)
  (cond
    ( (not(null? updated_cL)) (begin
                                (min_d_in_cluster (car(cdr(cdr (car updated_cL)))) (car (car updated_cL)))  ; send the distance list of each cluster
                                (nearest_cluster (cdr updated_cL))                                          ; check for each cluster
                                )))
   )
  
(nearest_cluster updated_clusterList)

;;;;;;; 4(ii) ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;  
(define (get_words cl_id cl_list)
  (if (= cl_id (list-ref (car cl_list) 0) )
         (list-ref (car cl_list) 1)
         (get_words cl_id (cdr cl_list))
         ))

(define d1 -1)
(define d2 -1)
(define (mini a b)
  (if (> a b)
      a
      b))

(define (repair2 ls accls)
  (cond
    ( (if (null? ls) 
          (append accls (list newest_cl_id (min d1 d2))) 
          (if  (= (car(car ls)) cluster1) 
              (begin (set! d1 (car(cdr(car ls)))) (repair2 (cdr ls) accls))
              (begin (if (= (car(car ls)) cluster2)
                         (begin (set! d2 (car(cdr(car ls)))) (repair2 (cdr ls) accls) )
                         (repair2 (cdr ls) (append accls (list(car ls))))
                         )
              
             )
         )))))

;(repair2 (list '(1 3/4)'(2 5/6)'(3 100)'(4 2/3)) (list ))
(define (repair1 ls)    ;to return the repaired list with correct list of list distances
  (list (list-ref ls 0) (list-ref ls 1) (repair2 (list-ref ls 2) '() ) ) ) ; sends the list-of-list to repair2

(define (get_dlist_for_newcl cl)
  (
  
  

(define (get_new_cl cl_id)
  (list newest_cl_id (append (get_words cluster1 updated_clusterList) (get_words cluster2 updated_clusterList)) (get_dlist_for_newcl newest_cl_id) )
  )

(define (update cL accL idL)
 (cond
   ((if (null? cL)
       (append accL (list (get_new_cl newest_cl_id) ))
    (if  (= (car(car cL)) cluster1) 
              (update (cdr cL) accL idL) 
              (begin (if (= (car(car cL)) cluster2)
                         (update (cdr cL) accL idL)
                         (update (cdr cL) (append accL (list(repair1 (car cL)))) idL)
                         ))
              )
))))



(define (step4)
  (set! newest_cl_id (id_counter))       ; to set the id of the newest cluster
  (update updated_clusterList '() (list cluster1 cluster2))
  
  )

(step4)
