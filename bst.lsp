(defun createNode (input)
    (list input nil nil)
)

(defun insertBST (bst input)
    (if bst
        (if (< input (car bst))
            (list (car bst) (insertBST (cadr bst) input) (caadr bst))
            (list (car bst) (cadr bst) (insertBST (caddr bst) input))
        )
        (createNode input)
    )
)

(defun createBST (input)
    (createBSTRunner input nil)
)

(defun createBSTRunner (input bst)
    (if input
        (createBSTRunner (cdr input) (insertBST bst (car input)))
        bst
    )
)

(defun removeElementBST (bst input)
    (if bst
        (cond
            ((< input (car bst))
                (list (car bst) (removeElementBST (cadr bst) input) (caddr bst))
            )
            ((> input (car bst))   
                (list (car bst) (cadr bst) (removeElementBST (caddr bst) input))
            )
            ((xor (cadr bst) (caddr bst))
                (list (findMinBST (caddr bst)) (cadr bst) (removeMinBST (caddr bst)))
                ; (print "bruh")
            )
            ((or (cadr bst) (caddr bst))
                (list (findMinBST (caddr bst)) (cadr bst) (removeMinBST (caddr bst)))
            )
        )
        bst
    )
)

(defun findMinBST (bst)
    (when bst
        (if (null (cadr bst))
            (car bst)
            (findMinBST (cadr bst))
        )
    )
)

(defun removeMinBST (bst)
    (when bst
        (if (cadr bst)
            (list (car bst) (removeMinBST (cadr bst)) (caddr bst))
            (caddr bst)
            ; nil
        )
    )
)

(defun findMaxBST (bst)
    (when bst
        (if (null (caddr bst))
            (car bst)
            (findMaxBST (caddr bst))
        )
    )
)

(defun removeMaxBST (bst)
    (when bst
        (if (caddr bst)
            (list (car bst) (cadr bst) (removeMaxBST (caddr bst)))
            (cadr bst)
        )
    )
)

(defun preOrderBST (bst)
    (when bst
        (append
            (list (car bst))
            (inOrderBST (cadr bst))
            (inOrderBST (caddr bst))
        )
    )
)

(defun inOrderBST (bst)
    (when bst
        (append
            (inOrderBST (cadr bst))
            (list (car bst))
            (inOrderBST (caddr bst))
        )
    )
)

(defun postOrderBST (bst)
    (when bst
        (append
            (inOrderBST (cadr bst))
            (inOrderBST (caddr bst))
            (list (car bst))
        )
    )
)

; example usage
(print (insertBST (insertBST (insertBST (insertBST nil 3) 2) 5) 6))
(print (createBST (list 3 2 5 6)))
(print (preOrderBST (createBST (list 3 2 5 6))))
(print (inOrderBST (createBST (list 3 2 5 6))))
(print (postOrderBST (createBST (list 3 2 5 6))))
(print (removeElementBST (createBST (list 3 2 5 6)) 3))
(print (removeElementBST (createBST (list 3 2 5 6)) 2))
(print (removeElementBST (createBST (list 3 2 5 6)) 5))
(print (removeElementBST (createBST (list 3 2 5 6)) 6))
