(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (> (count v) 2) (+ (get v 0) (get v 2)) '?)
)

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1, y1], [x2, y2]]]
  (- x2 x1))

(defn height [[[x1, y1], [x2, y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [[[x1, y1], [x2, y2]] [px, py]]
  (and (<= x1 px x2) (<= y1 py y2)))

(defn contains-rectangle? [outer [p1, p2]]
  (and (contains-point? outer p1) (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [all-books (conj (:authors book) new-author)]
  (assoc book :authors all-books)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [s (fn [e] (get e 1))]
  (map s collection))
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)
  )
)

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth (:birth-year author "")
        death (:death-year author "")
        birth-and-death (if (empty? (str birth death)) "" (str" (" birth " - " death ")"))
       ]
  (str author-name birth-and-death)
))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn book-count->string [books]
  (cond
    (empty? books) "No books"
    (== (count books) 1) "1 book"
    :else (str (count books) " books"))
)

(defn books->string [books]
  (let [the-books (apply str (interpose ", " (map book->string books)))
        the-books-dot (if (empty? the-books) "" (str " " the-books "."))
       ]
   (str (book-count->string books) "." the-books-dot)
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
