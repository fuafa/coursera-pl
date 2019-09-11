;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number in the first two arguments")))]
        [(closure? e) e]
        [(fun? e) (println env) (closure env e)]
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)])
           (if (not (closure? funexp))
               (error "MUPL call applied to non-closure in the first argument")
               (let* ([funexp-fun (closure-fun funexp)]
                      [fun-name (fun-nameopt funexp-fun)]                    
                      [param-name (fun-formal funexp-fun)]
                      [param-value (eval-under-env (call-actual e) env)]
                      [cl-env (closure-env funexp)]
                      [funbody (fun-body funexp-fun)])
                 (if fun-name
                     (eval-under-env funbody (cons (cons param-name param-value) (cons (cons fun-name funexp) cl-env)))
                     (eval-under-env funbody (cons (cons param-name param-value) cl-env))))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([value (eval-under-env (snd-e e) env)])
           (if (apair? value)
               (apair-e2 value)
               (error "MUPL snd applied to non-apair")))]
        [(mlet? e)
         (let ([name (mlet-var e)]
               [value (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons name value) env)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (call (fun #f "flag" (ifgreater (isaunit (var "flag")) (int 0) e2 e3)) e1))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4

(define mupl-map
  (fun #f "processor"
       (fun "map-rec" "mupl-list"
            (ifaunit (var "mupl-list")
                     (aunit)
                     (apair (call (var "processor") (fst (var "mupl-list"))) (call (var "map-rec") (snd (var "mupl-list"))))))))
;;            (mlet "item" (fst (var "mupl-list"))
;;                  (ifaunit (var "item")
;;                           (aunit)
;;                           (apair (call (var "processor") (var "item")) (call (var "map-rec") (snd (var "mupl-list")))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
;;        "CHANGE (notice map is now in MUPL scope)"))
        (fun #f "n"
             (call (var "map") (fun #f "x" (add (var "n") (var "x")))))))



;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([helper (λ (x)
          ;;           (println x)
                     (cond [(var? x)
                            (cons x (set (var-string x)))]
                           [(int? x)
                            (cons x (set))]
                           [(add? x)
                            (let ([v1 (helper (add-e1 x))]
                                  [v2 (helper (add-e2 x))])
                              (cons (add (car v1) (car v2))
                                    (set-union (cdr v1) (cdr v2))))]
                           
                           [(aunit? x)
                            (cons x (set))]
                           [(isaunit? x)
                            (let ([e-pair (helper (isaunit-e x))])
                              (cons (isaunit (car e-pair))
                                    (cdr e-pair)))]
                           [(ifgreater? x)
                            (let ([v1-pair (helper (ifgreater-e1 x))]
                                  [v2-pair (helper (ifgreater-e2 x))]
                                  [v3-pair (helper (ifgreater-e3 x))]
                                  [v4-pair (helper (ifgreater-e4 x))])
                              (cons (ifgreater (car v1-pair)
                                               (car v2-pair)
                                               (car v3-pair)
                                               (car v4-pair))
                                    (set-union (cdr v1-pair)
                                               (cdr v2-pair)
                                               (cdr v3-pair)
                                               (cdr v4-pair))))]
                           [(apair? x)
                            (let ([v1-pair (helper (apair-e1 x))]
                                  [v2-pair (helper (apair-e2 x))])
                              (cons (apair (car v1-pair) (car v2-pair))
                                    (set-union (cdr v1-pair)
                                               (cdr v2-pair))))]
                           [(fst? x)
                            (let ([e-pair (helper (fst-e x))])
                              (cons (fst (car e-pair))
                                    (cdr e-pair)))]
                           [(snd? x)
                            (let ([e-pair (helper (snd-e x))])
                              (cons (snd (car e-pair))
                                    (cdr e-pair)))]
                           [(closure? x)
                            (let ([fun-pair (helper (closure-fun x))])
                              (cons (closure (closure-env x)
                                             (car fun-pair))
                                    (cdr fun-pair)))]
                           [(call? x)
                            (let ([funexp-pair (helper (call-funexp x))]
                                  [actual-pair (helper (call-actual x))])
                              (cons (call (car funexp-pair)
                                          (car actual-pair))
                                    (set-union (cdr funexp-pair)
                                               (cdr actual-pair))))]
                           [(mlet? x)
                            (let ([v (mlet-var x)]
                                  [exp-pair (helper (mlet-e x))]
                                  [body-pair (helper (mlet-body x))])
                              (cons (mlet v
                                          (car exp-pair)
                                          (car body-pair))
                                    ;; NOTE: remove var from body-set and then
                                    ;; union with the e-set, rather than remove
                                    ;; var from the union of these two sets.
                                    ;; This rule makes variables-shadowing works.
                                    ;; The order is IMPORTANT.
                                    (set-union (set-remove (cdr body-pair) v)
                                               (cdr exp-pair))))]
                           [(fun? x)
                            (let* ([nameopt (fun-nameopt x)]
                                   [formal (fun-formal x)]
                                   [body-pair (helper (fun-body x))]
                                   [body-env (cdr body-pair)]
                                   [free-vars (if nameopt
                                                  (set-remove (set-remove body-env nameopt) formal)
                                                  (set-remove body-env formal))])
;;                              (println body-env)
;;                              (println free-vars)
                              (cons (fun-challenge nameopt
                                                   formal
                                                   (car body-pair)
                                                   free-vars)
                                    free-vars))]
                           [#t (error "Invalid MUPL syntax")]))])
    (car (helper e))))


;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number in the first two arguments")))]
        [(closure? e) e]
        [(fun-challenge? e)
         (let* ([free-vars (fun-challenge-freevars e)]
                [cl-env (set->list (set-map free-vars
                                            (λ (v) (cons v (envlookup env v)))))])
           (println cl-env)
           (closure cl-env e))]
        [(call? e)
         (let ([funexp (eval-under-env-c (call-funexp e) env)])
           (if (not (closure? funexp))
               (error "MUPL call applied to non-closure in the first argument")
               (let* ([funexp-fun (closure-fun funexp)]
                      [fun-name (fun-challenge-nameopt funexp-fun)]                    
                      [param-name (fun-challenge-formal funexp-fun)]
                      [param-value (eval-under-env-c (call-actual e) env)]
                      [cl-env (closure-env funexp)]
                      [funbody (fun-challenge-body funexp-fun)])
                 (if fun-name
                     (eval-under-env-c funbody (cons (cons param-name param-value) (cons (cons fun-name funexp) cl-env)))
                     (eval-under-env-c funbody (cons (cons param-name param-value) cl-env))))))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([value (eval-under-env-c (snd-e e) env)])
           (if (apair? value)
               (apair-e2 value)
               (error "MUPL snd applied to non-apair")))]
        [(mlet? e)
         (let ([name (mlet-var e)]
               [value (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons name value) env)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))


;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

