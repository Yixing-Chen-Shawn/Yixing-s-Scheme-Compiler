

(letrec ([run-long
          (lambda (it)
            (if (= it '0)
                '1
                (+

                 (run-long (- it '1))


             (cond [(< '1 '1) '0]
      [else (cond [(< '0 '1) (cond [(void? (cond)) (cond ['#t ((lambda (a) a) '11)]
                                                          [else '5])]
                                    [else '3])]
                  [else '2])])


             (car (cons
 (cond
  ('#t '1)
  ('20))
 (cond
  ('#f '10)
  ('30))))



             (let ([ff (lambda (ff) (lambda (a b c d e f g h i j)
                                      (if (<= '0 b)
                                          '1
                                          ((ff ff) (- b '1) c d e f g h i j a))))])
               ((ff ff) '1250 '1242 '683 '299 '351 '627 '990 '388 '717 '416))


             )))])
  (run-long '19000))


