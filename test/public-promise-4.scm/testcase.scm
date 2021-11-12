(foldl (lambda args (apply + args))
      '0
      (map (lambda (b) (if b '1 '2)) 
            (map (lambda arg (apply number? arg))
                  (list
                   '#f
                   '#t
                   '7
                   (list)
                   (list '() '())
                   (vector)
                   (vector '0 '1)
                   'yes))))
