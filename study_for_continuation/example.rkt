(app
 (fun
  '(mk-list)
  (app
   (fun
    '(list)
    (if0
     (app (id 'list) (list (num 0)))
     (app (id 'list) (list (num 1)))  ; true value
     (app                             ; false value
      (num 0)
      (list
       (app
        (app (id 'list) (list (num 2)))
        (list
         (app
          (app
           (app (id 'mk-list) (list (sub (app (id 'list) (list (num 0))) (num 1))))
           (list (add (app (id 'list) (list (num 1))) (num 2))))
          (list (app (id 'list) (list (num 2)))))))))))
   (list
    (withcc
     'k
     (app (app (app (id 'mk-list) (list (num 3))) (list (num 0))) (list (id 'k)))))))
 (list
  (fun
   '(a)
   (fun
    '(b)
    (fun
     '(c)
     (fun
      '(sel)
      (if0 (id 'sel) (id 'a) (if0 (sub (id 'sel) (num 1)) (id 'b) (id 'c)))))))))
