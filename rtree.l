#{

  https://github.com/mourner/rbush

}#

# Do this before math
(scl 7) # single-float precision

(load "@lib/math.l")

(de d* (D R) (*/ D R 1.0))
(de d/ (N D) (*/ 1.0 N D))
(de trunc (N) (* 1.0 (/ N 1.0)))
(de floor (N) (* 1.0 (*/ (- N 0.5) 1.0)))
(de ceil (N) (* 1.0 (*/ (+ N 0.5) 1.0)))

(class +RTree)
(dm T (Max-Entries)
  # max entries in a Node is 9 by default; min Node fill is 40% for best performance
  (default Max-Entries 9.0)
  (=: max-entries (max 4.0 Max-Entries))
  (=: min-entries (max 2.0 (ceil (* (: max-entries)) 0.4)))
  (clear> This))


(dm all> ()
  (-all> This (: data)))


(dm search> (BBox)
  #(println 'search> BBox)

  (let (Node (: data)
        Result ())

    #(println 'search> (getl Node))

    (when (xs BBox Node)

      (let (To-BBox 'to-bbox>
            Nodes-Search ()
            Ch (get Node 'children))

        (loop

          (for ((I . Ch) Ch Ch (cdr Ch))

            (let (Child (car Ch)
                  BBox-Child
                    (if (get Node 'rleaf)
                        (To-BBox This Child)
                        Child))

              (when (xs BBox BBox-Child)
                (cond
                  ((get Node 'rleaf) (push 'Result Child))
                  ((within BBox BBox-Child)
                    # push returned result to this result
                    (for C (-all> This Child)
                      (push 'Result C)))
                  (T
                    (push 'Nodes-Search Child))))))

          (NIL (setq Node (pop 'Nodes-Search))))))

      Result))


(dm collides> (BBox)

  (let Node (: data)

    (when (xs BBox Node)

      (let (Nodes-Search ()
            Ret NIL)

        (loop

          (let Ch (get Node 'children)

            (for ((I . Ch) Ch (and Ch (not Ret)) (cdr Ch))

              (let (Child (car Ch)
                    BBox-Child
                      (if (get Node 'rleaf)
                          (to-bbox> This Child)
                          Child))
                (when (xs BBox BBox-Child)
                  (if
                    (or
                      (get Node 'rleaf)
                      ((within BBox BBox-Child)))
                    (setq Ret T)
                    (push 'Nodes-Search Child))))))

          (NIL (setq Node (pop 'Nodes-Search))))

        Ret))))


(dm load> (Data)

  (when Data

    #(println 'load> Data
    #  (length Data)
    #  (: min-entries))

    (if (<
          (length Data)
          (: min-entries))

        (for ((I . Data) Data Data (cdr Data))
          (insert> This (car Data))))

        # recursively build the tree
        # with the given data from scratch
        # using OMT algorithm
        (let Node
              (-build> This
                  (copy Data)
                  0
                  (- (length Data) 1)
                  0)

          (cond

            ((not (get (: Data) 'children))
              # save as is if tree is empty
              (=: Data Node))

            ((= (get (: Data) 'height) (get Node 'height))
              # split root if trees have the same height
              (-split-root> This (: Data) Node))

            (T

              (when (< (get (: Data) 'height) (get Node 'height))
                # swap trees if inserted one is bigger
                (let Tmp-Node (: Data)
                  (=: Data Node)
                  (setq Node Tmp-Node)))

              # insert the small tree into the large tree at appropriate level
              (-insert> This
                  Node
                  (-
                    (get (: Data) 'height)
                    (get Node 'height)
                    1.0)
                  T))))))


(dm insert> (Item)
  #(println 'insert> (last Item))
  (when Item
    (-insert> This
      Item
      (- (get (: data) 'height) 1.0))))


(dm clear> ()
  (=: data (new '(+RTNode))))


(dm remove> (Item Equals-Fn)

  (println 'remove> Item)
  #(pp 'Item)
  #(println Equals-Fn)

  (when Item
    (let (Node (: data)
          BBox (to-bbox> This Item)
          Path ()
          Indices ()
          Index NIL
          I NIL
          Parent NIL
          Going-Up NIL
          Ret NIL)

      # depth-first iterative tree traversal
      (while
        (and
          (not Ret)
          (or Node Path))

        (unless Node # go up
            (setq
              Node (pop 'Path)
              Parent (get Path (- (length Path) 1))
              I (pop 'Indices)
              Going-Up T))

        (if (and
              (get Node 'rleaf) # check current Node
              (setq Index
                (find-item Item
                  (get Node 'children) Equals-Fn)))

          # Index found
          (prog
            # item found, remove the item
            # condense tree upwards
            # remove item at ix
            (put Node 'children
              (remove Index (get Node 'children)))
            (push 'Path Node)
            (-condense> This Path)
            (setq Ret T))

          # Else go down
          (cond

            ((and
              (not Going-Up)
              (not (get Node 'rleaf))
              (within Node BBox)) # go down

              (push 'Path Node)
              (push 'Indices I)
              (setq
                I 0
                Parent Node
                Node (car (get Node 'children))))

            (Parent # go right
              (inc 'I)
              (setq
                Node (car (get Node 'children))
                Going-Up NIL))

            (T
              # nothing found
              (print 'remove>) (prinl " Item not found!")
              (setq Node NIL)))))

      # post while
      # maybe flip Node to return it removed
      Node)))


# why...is this overloaded?
(dm to-bbox> (Item) Item)

(dm cmp-min-x> (A B) (- (get A 'min-x) (get B 'min-x)))
(dm cmp-min-y> (A B) (- (get A 'min-y) (get B 'min-x)))

(dm -all> (Node)
  # TODO: use deque?
  (let (Nodes-Search ()
        Res ())
    (loop
      (let Ch  (get Node 'children)
        (if (get Node 'rleaf)
            (for C Ch (push 'Res C))
            # does order matter here?
            (for C Ch (push 'Nodes-Search C))))
      (NIL (setq Node (pop 'Nodes-Search))))
    Res))

(dm -build> (Items Left Right Height)

  (let (N (- Right (+ Left 1))
        M (: max-entries)
        Node NIL)

    (if (<= N M)

        (prog
          # reached rleaf level, return rleaf
          # slice
          (setq Node
            (new '(+Node)
              (head Left (nth Items (+ Right 1.0)))))
          (calc-bbox Node 'to-bbox>))


      (unless Height
        # target height of the bulk-loaded tree
        (setq
          Height (ceil (d/ (log N) (log M)))
          # target number of root entries to maximize storage utilization
          M (ceil (d/ N (pow M (- Height 1.0))))))

      (setq Node (new '(+RTNode)))
      (put Node 'rleaf NIL)
      (put Node 'height Height)

      # split the items into M mostly square tiles

      (let (N2 (ceil (d/ N M))
            N1 (* (ceil (sqrt M)) N2))

      # modifies items?
      (multi-select Items Left Right N1 'cmp-min-x>)

      (for (I Left (<= I Right) (inc 'I N1))

        (let R2 (min (+ I (- N1 1.0)) Right)

          (multi-select Items I R2 N2 'cmp-min-y>)

          (for (J I (<= J R2) (inc 'J N2))

            (let R3 (min (- (+ J N2) -1) R2)

              # pack each entry recursively
              (push (get Node 'children)
                (-build> This
                  Items J R3 (- Height 1.0)))))))

      (calc-bbox Node 'to-bbox>)))

    Node))


(dm -choose-subtree> (BBox Node Level Path)

  #(println '-choose-subtree>)
  #(pp 'BBox)
  #(pp 'Node)
  #(println Level Path)

  (loop

    (push 'Path Node)

    (T
      (or
        (get Node 'rleaf)
        (= (- (length Path) 1) Level)))

    (let (Min-Area        T # Infinity
          Min-Enlargement T # Infinity
          Target-Node     NIL)

      (for (I 0 (< I (length (get Node 'children))) (inc I))

        (let (Child (get Node 'children I)
              Area (bbox-area Child)
              Enlargement (- (enlarged-area BBox Child) Area))

          (cond
            # choose entry with the least area enlargement
            ((< Enlargement Min-Enlargement)
              (setq
                Min-Enlargement Enlargement
                Min-Area
                  (if (< Area Min-Area) Area Min-Area)
                Target-Node Child))

            ((= Enlargement Min-Enlargement)
              # otherwise choose one with the smallest area
              (when (< Area Min-Area)
                (setq
                  Min-Area Area
                  Target-Node Child))))))

        (setq Node
          (or
            Target-Node
            (car (get Node 'children))))))
  Node)


(dm -insert> (Item Level Is-Node)

  #(println '-insert> (last Item) Level Is-Node)

  # find the best Node for accommodating the item,
  # saving all Nodes along the path too
  (let (BBox Item #(if Is-Node Item (to-bbox> This Item))
        Insert-Path ()
        Node (-choose-subtree> This
                BBox (: data) Level Insert-Path))

    (let Ch (get Node 'children)
      # put the item into the Node
      (put Node 'children
        (if Ch
            (prog
              (push 'Ch Item)
              Ch)
            (list Item))))

    (ext-bbox Node BBox)

    # split on Node overflow
    # propagate upwards if necessary
    (loop
      (T
        (or
          (not (ge0 Level))
          (<=
            (length (get Insert-Path 'level 'children))
            (: max-entries))))
      (-split> This Insert-Path Level)
      (dec 'Level))

    # adjust bboxes along the insertion path
    (-adj-parent-bboxes> This BBox Insert-Path Level)))


# split overflowed Node into two
(dm -split> (Insert-Path Level)
  #(println '-split> Insert-Path Level)

  (let (Node (get Insert-Path 'level)
        M (length (get Node 'children))
        m (: min-entries))

    (-choose-split-axis Node m M)

    (let (Split-Index (-choose-split-ix Node m M)
          New-Node
            (new '(+RTNode)
              (let (Children (get Node 'children)
                    L (length Children)
                    A (nth Children)
                    B (nth A
                        (- L Split-Index Split-Index)))
                # want everything before A
                # then delete N items after
                # aka con A to B cdr
                (con A (cdr B))
                (put Node 'children
                  Children)
                Children)))

      (put New-Node 'height (get Node 'height))
      (put New-Node 'rleaf (get Node 'rleaf))

      (calc-bbox Node 'to-bbox)
      (calc-bbox New-Node 'to-bbox)

      (if Level

        #insertPath[level - 1].children.push(newNode);
        # ensure this is destructive
        (push (get Insert-Path (- Level 1) 'children)
          New-Node)

        (-split-root Node New-Node)))))


(dm -split-root> (Node New-Node)
  # split root Node
  (=: data (new '(+RTNode) (list Node New-Node)))
  (with (: data)
    (=: height (+ (get Node 'height) 1.0))
    (=: rleaf NIL))
  (calc-bbox (: data) 'to-bbox>))


(dm -choose-split-ix> (Node m M)
  (let (Index NIL
        Min-Overlap T  # +Inf or 'MAX (any sym)
        Min-Area    T) # +Inf or 'MAX (any sym)

    (for (I m (<= I (- M m)) (inc I))

      (let (BBox1 (dist-bbox Node 0 I 'to-bbox>)
            BBox2 (dist-bbox Node I M 'to-bbox>)
            Overlap (xs-area BBox1 BBox2)
            Area (+ (bbox-area BBox1) (bbox-area BBox2)))

        (cond

          # choose distribution with minimum overlap
          ((< Overlap Min-Overlap)
            (setq
              Min-Overlap Overlap
              Index I
              minArea
                (if (< Area Min-Area) Area Min-Area)))

          ((= Overlap Min-Overlap)
            # otherwise choose distribution with minimum area
            (when (< Area Min-Area)
              (setq
                Min-Area Area
                Index I))))))

    # post for
    (or
      Index
      (- M m))))


# sorts Node children by the best axis for split
(dm -choose-split-axis> (Node m M)
  (let (Cmp-Min-X (if (get Node 'rleaf) 'cmp-min-x> 'cmp-node-min-x)
        Cmp-Min-Y (if (get Node 'rleaf) 'cmp-min-y> 'cmp-node-min-y)
        xMargin (-all-dist-margin Node m M Cmp-Min-X)
        yMargin (-all-dist-margin Node m M Cmp-Min-Y))

    # if total distributions margin value is minimal for x, sort by minX,
    # otherwise it's already sorted by minY
    (when (< xMargin yMargin)
      # verify sort
      # write back?
      (put Node 'children
        (sort (get Node 'children)
          'Cmp-Min-X)))))


# total margin of all possible split distributions where each Node is at least m full
(dm -all-dist-margin> (Node m M Cmp)

  (put Node 'children
    (sort (get Node 'children)
      Cmp))

  (let (To-BBox 'to-bbox>
        Left-BBox (dist-bbox Node 0 m To-BBox)
        Right-BBox (dist-bbox Node (- M m) M To-BBox)
        Margin (+ (bbox-margin Left-BBox) (bbox-margin Right-BBox)))

    (for (I m (< I (- M m)) (inc I))
      (let Child (get Node 'children I)
        (ext-bbox
          Left-BBox
          (if (get Node 'rleaf) (To-BBox Child) Child))
        (inc 'Margin
          (bbox-margin Left-BBox))))

    (for (I (- M m 1) (>= I m) (dec I))
      (let Child (get Node 'children I)
        (ext-bbox
          Right-BBox
          (if (get Node 'rleaf) (To-BBox Child) Child))
        (inc 'Margin
          (bbox-margin Right-BBox))))

    Margin))


(dm -adj-parent-bboxes> (BBox Path Level)
  #(println 'adjust-parent-bboxes>)
  #(pp 'BBox) (println Path Level)
  # adjust bboxes along the given tree path
  (for (I Level (>= I 1) (dec I))
    (ext-bbox (get Path I) BBox)))


(dm -condense> (Path)
  #(println '-condense> Path)
  # go through the path, removing empty Nodes and updating bboxes
  (let Siblings NIL
    (for (I (length Path) (>= I 1) (dec I))
      (if (=0 (length (get Path I 'children)))
          (if (> I 1)
              (prog
                # modify children paths
                (setq Siblings (get Path (- I 1) 'children))
                (put Path (- I 1) 'children
                  (remove
                    (index (get Path I)
                      Siblings)
                    Siblings)))
              (clear> This))
          (calc-bbox (get Path I) 'to-bbox>)))))


# NORMAL

(de find-item (Item Items Equals-Fn)
  (println 'find-item Item (length Items))
  # (pp 'Item) (println Items Equals-Fn)
  (if Equals-Fn
      (let R NIL
        (for
          ((I . Items)
          Items
          # stop when R is true
          # but loop stops when this ret NIL
          # so when T retun NIL
          (not (setq R (Equals-Fn (car Items) Item)))
          (cdr Items)))
        R)
      (index Item Items)))

(de find-item-slow (Item Items Equals-Fn)
  (println 'find-item Item Items)
  # (pp 'Item) (println Items Equals-Fn)
  (if Equals-Fn
      (let R
        # TODO fix this!...stop immediately
        (for (I 1 (<= I (length Items)) (inc I))
          (when (Equals-Fn Item (get Items I))
            (setq R I)))
        R)
      (index Item Items)))

# calculate Node's bbox from bboxes of its children
(de calc-bbox (Node To-BBox)
  # TODO: Fix To-BBox...returns the first arg
  (dist-bbox Node 0 (length (get Node 'children)) To-BBox Node))


# min bounding rectangle of Node children from k to p-1
(de dist-bbox (Node K P To-BBox Dest-Node)

  (unless Dest-Node
    (setq Dest-Node (new '(+RTNode))))

  (with Dest-Node
    (=: min-x 'MAX)
    (=: min-y 'MIN)
    (=: max-x NIL)
    (=: max-y NIL))

  (for (I K (< I P) (inc I))
    (let Child (get Node 'children I)
      (ext-bbox Dest-Node
        (if (get Node 'rleaf)
            Child
            Child))))

  Dest-Node)


(de ext-bbox (A B)
  #(println 'ext-bbox)
  (with A
    (=: min-x (min (: min-x) (get B 'min-x)))
    (=: min-y (min (: min-y) (get B 'min-y)))
    (=: max-x (max (: max-x) (get B 'max-x)))
    (=: max-y (max (: max-y) (get B 'max-y)))))


(de cmp-node-min-x (A B) (- (get A 'min-x) (get B 'min-x)))
(de cmp-node-min-y (A B) (- (get A 'min-y) (get B 'min-y)))

(de bbox-area (A)
  (*
    (- (get A 'max-x) (get A 'min-x))
    (- (get A 'max-y) (get A 'min-y))))

(de bbox-margin (A)
  (+
    (- (get A 'max-x) (get A 'min-x))
    (- (get A 'max-y) (get A 'min-y))))

(de enlarged-area (A B)
  (*
    (- (max (get B 'max-x) (get A 'max-x)) (min (get B 'min-x) (get A 'min-x)))
    (- (max (get B 'max-y) (get A 'max-y)) (min (get B 'min-y) (get A 'min-y)))))


(de xs-area (A B)
  (let (Min-X (max (get A 'min-x) (get B 'min-x))
        Min-Y (max (get A 'min-y) (get B 'min-y))
        Max-X (min (get A 'max-x) (get B 'max-x))
        Max-Y (min (get A 'max-y) (get B 'max-y)))
    (* (max 0.0 (- Max-X Min-X))
       (max 0.0 (- Max-Y Min-Y)))))


(de within (A B)
  (and
    (<= (get A 'min-x) (get B 'min-x))
    (<= (get A 'min-y) (get B 'min-y))
    (>= (get B 'max-x) (get A 'max-x))
    (>= (get B 'max-y) (get A 'max-y))))


(de xs (A B)
  (and
    (<= (get B 'min-x) (get A 'max-x))
    (<= (get B 'min-y) (get A 'max-y))
    (>= (get B 'max-x) (get A 'min-x))
    (>= (get B 'max-y) (get A 'min-y))))


(class +RTNode)
(dm T (Children Height RLeaf Min-X Min-Y Max-X Max-Y Any)
  (default
    Height 1.0
    RLeaf 'rleaf
    Min-X 'MAX # T
    Min-Y 'MAX # T
    Max-X NIL
    Max-Y NIL)
  (=: children Children)
  (=: height   Height)
  (=: rleaf    'rleaf)
  (=: min-x    Min-X) # +Infinity
  (=: min-y    Min-Y) # +Infinity
  (=: max-x    Max-X) # -Infinity
  (=: max-y    Max-Y) # -Infinity
  (=: any      Any))


# sort an array so that items come in groups of n unsorted items,
# with groups sorted between each other;
# combines selection algorithm with binary divide & conquer approach

(de multi-select (Arr Left Right N Cmp)
  (println 'multi-select)
  (let Stack (cons Left Right)
    (while (length Stack)
      (let (Right (pop 'Stack)
            Left (pop 'Stack))
        (unless (<= (- Right Left) N)
          (let Mid (+ Left (d* (ceil (d/ (- Right Left) N 2.0)) N))
            (quickselect Arr Mid Left Right Cmp))
          (push 'Stack Left Mid Mid Right))))))

#{

  https://github.com/mourner/quickselect

  quickselect(array, k[, left, right, compareFn]);

  Rearranges items so that all items in the [left, k] are the smallest. The k-th element will have the (k - left + 1)-th smallest value in [left, right].

    array: the array to partially sort (in place)
    k: middle ix for partial sorting (as defined above)
    left: left ix of the range to sort (0 by default)
    right: right ix (last ix of the array by default)
    compareFn: compare function


}#

(de quickselect (Arr K Left Right Cmp)
  (quickselect-step
    Arr
    K
    (or Left 0)
    (or Right (- (length Arr) 1))
    (or Cmp default-compare)))


(de quickselect-step (Arr K Left Right Cmp)

  (while (> Right Left)

    (when (> (- Right Left) 600.0)
      (let (n (- Right (+ Left 1.0))
            m (- K (+ Left 1.0))
            z (log N)
            s (d* 0.5 (exp (d* 2.0 (d/ z 3.0))))
            sd (d* 0.5
                (d*
                  (sqrt
                    (d*
                      (d* z s)
                      (d/ (- n s) n)))
                  (if (< (d/ (- m n) 2.0) 0.0) -1.0 1.0)))
            New-Left (max
                       Left
                       (floor
                          (+
                            (-
                              K
                              (d/
                                (d* m s)
                                n))
                            sd)))
            New-Right (min
                        Right
                        (floor
                          (+
                            K
                            (d/
                              (d*
                                (- n m)
                                s)
                              n)
                            sd))))

        (quickselect-step Arr K New-Left New-Right Cmp)))

    (let (t (get Arr K)
          I Left
          J Right)

      (swap-arr Arr Left K)

      (when (> (Cmp (get Arr Right) t) 0.0)
        (swap-arr Arr Left Right))

      (while (< I J)
        (swap-arr Arr I J)
        (inc 'I)
        (dec 'J)
        (while (< (Cmp (get Arr I) t) 0.0) (inc 'I))
        (while (> (Cmp (get Arr J) t) 0.0) (dec 'J)))

      (if (= (Cmp (get Arr Left) t) 0.0)
          (swap-arr Arr Left J)
          (inc 'J)
          (swap-arr Arr J Right))

      (when (<= J K) (setq Left (+ J 1.0)))
      (when (<= K J) (setq Right (- J 1.0))))))


(de swap-arr (Arr I J)
  (let Tmp (get Arr I)
    (put Arr I (get Arr J))
    (put Arr J Tmp)))


(de default-compare (A B)
  (if (< A B)
      -1.0
      (if (> A B) 1.0 0.0)))


(de main ()

  #{

    Test

    * insert 1, N
    * delete 1, N
    * alt ins/del
    * bulk load
    * bulk delete (serial del, no native fn)
    * search (items in bbox
    * collision (any in bbox)


    9 +         +-------9,9
    8 +         |        |
    7 +         |        |
    6 +         |        |
    5 +         +--------+
    4 +   +---+
    3 +   |   |
    2 +   2,2-+
    1 +
    0 +-+-+-+-+-+-+-+-+-+
      0 1 2 3 4 5 6 7 8 9

    * 3 interaction states:
      * inside completely
      * outside completely
      * intersect (overlap)

  }#

  (let (Rt (new '(+RTree) 16.0)
        Item-1 (new '(+RTNode)
                NIL 1.0 T
                2 2 4 4
                'Item-1)
        Item-2 (new '(+RTNode)
                  NIL 1.0 T
                  5 5 9 9
                  'Item-2)
        Search-T (new '(+RTNode)
                    NIL 1.0 T
                    0 0 10 10
                    'search)
        Search-Nil (new '(+RTNode)
                      NIL 1.0 T
                      2 5 4 9
                      'search)
        Search-1 (new '(+RTNode)
                      NIL 1.0 T
                      2 2 4 4
                      'search)
        Search-2 (new '(+RTNode)
                      NIL 1.0 T
                      5 5 9 9
                      'search)
        Collide-T (new '(+RTNode)
                      NIL 1.0 T
                      3 3 7 7
                      'search)
        Collide-Nil Search-Nil)

    (println 'Root (get Rt 'data))
    (println 'Item-1 Item-1)
    (println 'Item-2 Item-2)
    (do 1 (prinl))

    (load> Rt (list Item-1 Item-2))

    (prin "Search (T): ")     (println (search> Rt Search-T))
    (prin "Search (Nil): ")   (println (search> Rt Search-Nil))
    (prin "Search (1): ")     (println (car (search> Rt Search-1)))
    (prin "Search (2): ")     (println (car (search> Rt Search-2)))
    (prin "Collides (T): ")   (println (collides> Rt Collide-T))
    (prin "Collides (Nil): ") (println (collides> Rt Collide-Nil))

    (do 1 (prinl))
    (remove> Rt Item-1)
    (do 1 (prinl))
    (remove> Rt Item-2)
    (do 1 (prinl))

    # all NIL
    (prin "Search (T): ")     (println (search> Rt Search-T))
    (prin "Search (Nil): ")   (println (search> Rt Search-Nil))
    (prin "Search (1): ")     (println (car (search> Rt Search-1)))
    (prin "Search (2): ")     (println (car (search> Rt Search-2)))
    (prin "Collides (T): ")   (println (collides> Rt Collide-T))
    (prin "Collides (Nil): ") (println (collides> Rt Collide-Nil))

    (bye)

    (prinl)))


(de main-2 ()

  #{

    # Randomize within 0,0 1000,1000

    9 +         +-------9,9
    8 +         |        |
    7 +         |        |
    6 +         |        |
    5 +         +--------+
    4 +   +---+
    3 +   |   |
    2 +   2,2-+
    1 +
    0 +-+-+-+-+-+-+-+-+-+
      0 1 2 3 4 5 6 7 8 9

  }#

  (seed (in "/dev/urandom" (rd 8)))

  (let (Rt (new '(+RTree) 16.0)
        Search-T (new '(+RTNode)
                    NIL 1.0 T
                    0 0 100000 100000
                    'search)
        Search-Hf (new '(+RTNode)
                    NIL 1.0 T
                    0 0 50000 50000
                    'search))

    (println 'Root (get Rt 'data))

    (gc 16)
    (prinl "GC alloc done")

    # -/+ 2147483648
    (load> Rt
      (make
        (do 100000
          (link
            (new '(+RTNode)
              NIL 1.0 T
              (rand 1 99999)
              (rand 1 99999)
              (rand 1 99999)
              (rand 1 99999))))))

    (prin "Search (T): ") (println (length (search> Rt Search-T))) (prinl)
    (prin "Search (Hf): ") (println (length (search> Rt Search-Hf))) (prinl)

    (bye)

    (prinl)))
