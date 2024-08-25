#lang htdp/asl

(require "quiz_lib.rkt")

;; See "*** TODO ***" below for the missing methods

;; a fun question is a ...
;; (make-fun-question string (listof string) (listof string))
;;   If a user answers with a particular choice, you can think of that as a vote
;;   for a particular outcome (the result of the quiz as a whole).
;; Note that corresponding-outcomes in each question could be ordered differently.
(define-struct fun-question [text choices corresponding-outcomes] 
  #:methods
  (define (show-text q)
    (local [(define (print-choices num-lbl choices)
              (when (not (empty? choices))
                (begin
                  (printf "  ~a. ~a\n" num-lbl (first choices))
                  (print-choices (+ num-lbl 1) (rest choices)))))]
      (begin
        (printf "Q: ~a\n" (fun-question-text q))
        (print-choices 1 (fun-question-choices q)))))
  
  ;; get-choice-from-number: fun-question number -> string
  ;;   Returns the choice associated with a particular number.
  (define (choice-ref q choice)
    ;; the list index starts at 0, so we need to subtract the choice 1
    (list-ref (fun-question-choices q) (- choice 1)))

  ;; get-corresponding-outcome-from-number: fun-question Number -> String
  ;;   Returns the outcome associated with a particular number.
  (define (corresponding-outcome-ref q choice)
    ;; the list index starts at 0, so we need to subtract the choice 1
    (list-ref (fun-question-corresponding-outcomes q) (- choice 1))))

;; Credit buzzfeed - https://www.buzzfeed.com/jenniferabidor/encanto-character-quiz 
(check-expect (choice-ref q1 5) "Shy")
(check-expect (corresponding-outcome-ref q1 5) "Bruno")
(define q1
  (make-fun-question
   "How would your friends describe you?"
   (list "Funny" "Strong" "Emotional" "Kind" "Shy" "Intense")
   (list "Dolores" "Luisa" "Pepa" "Mirabel" "Bruno" "Isabella")))
(define myq1
  (make-fun-question
   "What is your favorite pet?"
   (list "cat" "dog" "Birds" "rabbit" "snake" "Hamster")
   (list "Jerome" "Ciella" "Faith" "Tharcisse" "Mr.Johnson" "Christa")))
(check-expect (choice-ref q2 5) "No gift")
(check-expect (corresponding-outcome-ref q2 5) "Isabella")
(define q2
  (make-fun-question
   "Choose a gift you'd love to have."
   (list "Invisibility" "Mind Reading"
         "Ability to fly" "Knowing every language"
         "No gift" "Literally any gift")
   (list "Dolores" "Luisa" "Pepa" "Bruno" "Isabella" "Mirabel")))
(define myq2
  (make-fun-question
   "If you were to ask one thing from your parents what would be?"
   (list "How they met and decided to be partners" "Their favorite memories in childhood"
         "Visiting foreign countries" "Things that were fascinating in their emerging adulthood"
         "No thing" "Their parents' backgrounds")
   (list "Jerome" "Mr.Johnson" "Faith" "Christa" "Tharcisse" "Ciella")))

(check-expect (choice-ref q3 5) "What Else Can I Do?")
(check-expect (corresponding-outcome-ref q3 5) "Pepa")
(define q3
  (make-fun-question
   "Pick the Encanto song you can't stop listening to."
   (list "The Family Madrigal"
         "Waiting on a Miracle"
         "Surface Pressure"
         "We Don't Talk About Bruno"
         "What Else Can I Do?"
         "Impossible to pick just one")
   (list "Dolores" "Mirabel" "Luisa" "Bruno" "Pepa" "Isabella")))


(define myq3
  (make-fun-question
   "What do you do for fun?"
   (list "Making jokes"
         "Hanging out with friends"
         "biking and playing basketball"
         "Listening to music"
         "Reading posts on TikTok"
         "Coding in my free time")
   (list "Mr.Johnson" "Jerome" "Ciella" "Christa" "Faith" "Tharcisse")))
(define myq4
  (make-fun-question
   "Who is your favorite artist?"
   (list "Taylor Swift"
         "Hard to pick one"
         "Beyonce"
         "I don't have any"
         "The Weekend"
         "Justin Bieber")
   (list "Faith" "Ciella" "Christa" "Tharcisse" "Jerome" "Mr.Johnson")))



;; a quiz is a ...
;; (make-quiz string (listof fun-question) (listof string) (hash string number))
(define-struct quiz [title questions possible-outcomes scoring-of-outcomes]
  #:methods
  (define (reset-scoring-hash qz)
    (for-each (λ (o)
                 
                (hash-set! (quiz-scoring-of-outcomes qz)
                           o
                           0))
              (quiz-possible-outcomes qz )))
             
     
  (define (update-scoring-hash qz o)
    
    (hash-set! (quiz-scoring-of-outcomes qz)
               o
               (+ 1 (hash-ref (quiz-scoring-of-outcomes qz)
                              o))))
              
  (define (get-scoring-outcome qz)
    (local[(define mymax -1)
           (define max-outcome "")]
      (begin(for-each (λ(o)
                        (when (> (hash-ref (quiz-scoring-of-outcomes qz)
                                           o)
                                 mymax)
                          (begin(set! mymax (hash-ref (quiz-scoring-of-outcomes qz)
                                                      o))
                                (set! max-outcome o)))
                    
                        )
                      (quiz-possible-outcomes qz))
            max-outcome))))
    

;; *** TODO ***
;; reset-scoring-hash: quiz -> void
;;   For each outcome in possible-outcomes, sets the value associated it
;;   in the hash table in scoring-of-outcomes to 0.
;; Effect: scoring-of-outcomes attribute has changed, all values set to 0.
;;
;; See the first check-expect below for an example.


;; *** TODO ***
;; update-scoring-hash: quiz string -> void
;;   Increments the scoring-of-outcomes hash, adding one to the value
;;   associated with the given outcome.
;; Effect: scoring-of-outcomes attribute has changed, incrementing value
;;  associated with the given by 1.
;;
;; See the second check-expect below for an example.

  
;; *** TODO ***
;; get-scoring-outcome: quiz -> String
;;   Iterates over the scoring-of-outcomes hash to find and return the
;;   outcome with the highest associated value.
;;
;; See the third check-expect below for an example.

  

;; an example "quiz" (a list of questions)
(define encanto-quiz
  (make-quiz "Which Encanto character are you most like?"
             (list q1 q2 q3)
             (list "Dolores" "Luisa" "Pepa" "Mirabel" "Bruno" "Isabella")
             (make-hash)))
(define my-fun-quiz
  (make-quiz "Which Person are you most like?"
             (list myq1 myq2 myq3 myq4)
             (list "Jerome" "Faith" "Tharcisse" "Ciella2
" "Christa" "Mr.Johnson")
             (make-hash)))

;; A check-expect for reset-scoring-hash
(check-expect
 (local [(define testquiz
           (make-quiz "Title"
                      (list q1)
                      (list "Pepa" "Mirabel" "Bruno" "Isabella")
                      (make-hash)))]
   (begin
     (reset-scoring-hash testquiz)
     testquiz))
 (make-quiz "Title"
            (list q1)
            (list "Pepa" "Mirabel" "Bruno" "Isabella")
            (make-hash
             (list (list "Pepa" 0) (list "Mirabel" 0)
                   (list "Bruno" 0) (list "Isabella" 0)))))

;; A check-expect for update-scoring-hash
(check-expect
 (local [(define testquiz
           (make-quiz "Title"
                      (list q1)
                      (list "Pepa" "Mirabel" "Bruno" "Isabella")
                      (make-hash)))]
   (begin
     (reset-scoring-hash testquiz)
     (update-scoring-hash testquiz "Mirabel")
     (update-scoring-hash testquiz "Bruno")
     (update-scoring-hash testquiz "Bruno")
     (update-scoring-hash testquiz "Isabella")
     (quiz-scoring-of-outcomes testquiz)))
 (make-hash
  (list (list "Pepa" 0) (list "Mirabel" 1)
        (list "Bruno" 2) (list "Isabella" 1))))

;; A check-expect for get-scoring-outcome
(check-expect
 (local [(define testquiz
           (make-quiz "Title"
                      (list q1)
                      (list "Pepa" "Mirabel" "Bruno" "Isabella")
                      (make-hash)))]
   (begin
     (reset-scoring-hash testquiz)
     (update-scoring-hash testquiz "Mirabel")
     (update-scoring-hash testquiz "Bruno")
     (update-scoring-hash testquiz "Bruno")
     (update-scoring-hash testquiz "Isabella")
     (update-scoring-hash testquiz "Mirabel")
     (update-scoring-hash testquiz "Bruno")
     ;(printf "~a updating\n" testquiz)
     ;; Hint: insert (print testquiz) here to see the votes
     (get-scoring-outcome testquiz)))
 "Bruno")




;; runquiz: (listof question) -> void
;;   Takes a list of questions. In order, displays the question,
;;   gets a response from the user and checks the answer.
;; Effect: A quiz has been displayed and run.
(define (runquiz somequiz)
  (local [(define user-response "")
          (define user-responses (list))]
    (begin
      (printf "Welcome to my quiz!\n>>> ~a <<<\n" (quiz-title somequiz))
      (reset-scoring-hash somequiz)
      (for-each (lambda (q)
                  (begin (newline)
                         (show-text q)
                         (printf "> ")
                         (set! user-response (read))
                         (set! user-responses (append user-responses
                                                      (list (choice-ref q user-response))))
                         (update-scoring-hash somequiz
                                              (corresponding-outcome-ref q
                                                                         user-response))))
                (quiz-questions somequiz))
      (printf "\nYou answered...\n")
      (for-each (lambda (s) (printf "    - ~a\n" s)) user-responses)
      (printf "Your result is...")
      (get-scoring-outcome somequiz))))


;; run the quiz on our list of questions
;(runquiz encanto-quiz)
(runquiz my-fun-quiz)