(defglobal ?*alphabet* = "")
(defglobal ?*alphabet-size* = 0)
(defglobal ?*numerical-series* = (create$))
(defglobal ?*sorted-series* = (create$))
(defglobal ?*linguistic-series* = (create$))
(defglobal ?*precedence-matrix* = (create$))

(deffunction read-params-file ()
  (bind ?params-file (open "params.txt" "r"))
  (if (not ?params-file) then
    (printout t "Failed to open file with alphabet options." crlf)
    (exit)
  )
  (bind ?*alphabet-size* (read ?params-file))
  (bind ?*alphabet* (create$))
  (loop-for-count ?i ?*alphabet-size* do
    (bind ?char (read ?params-file))
    (bind ?*alphabet* (insert$ ?*alphabet* (length$ ?*alphabet*) ?char))
  )
  (close ?params-file)
)

(deffunction read-numerical-series ()
  (bind ?input-file (open "number series.txt" "r"))
  (if (not ?input-file) then
    (printout t "Could not open source file." crlf)
    (exit)
  )
  (bind ?*numerical-series* (create$))
  (while (bind ?num (read ?input-file))
    (bind ?*numerical-series* (insert$ ?*numerical-series* (length$ ?*numerical-series*) ?num))
  )
  (close ?input-file)
)

(deffunction merge-sort (?array)
  (if (> (length$ ?array) 1) then
    (bind ?mid (/ (length$ ?array) 2))
    (bind ?left (subseq$ ?array 0 ?mid))
    (bind ?right (subseq$ ?array ?mid (length$ ?array)))
    (bind ?left (merge-sort ?left))
    (bind ?right (merge-sort ?right))
    (return (merge ?left ?right))
  else
    (return ?array)
  )
)

(deffunction merge (?left ?right)
  (bind ?result (create$))
  (while (or (length$ ?left) (length$ ?right))
    (if (or (not (length$ ?right)) (and (length$ ?left) (<= (nth$ 1 ?left) (nth$ 1 ?right))))
      then
        (bind ?result (insert$ ?result (length$ ?result) (nth$ 1 ?left)))
        (bind ?left (subseq$ ?left 1 (length$ ?left)))
    else
      (bind ?result (insert$ ?result (length$ ?result) (nth$ 1 ?right)))
      (bind ?right (subseq$ ?right 1 (length$ ?right)))
    )
  )
  (return ?result)
)

(deffunction uniform-cdf (?x ?min ?max)
  (return (/ (- ?x ?min) (- ?max ?min)))
)

(deffunction numerical-to-linguistic (?numerical-series ?sorted-series)
  (bind ?linguistic-series (create$))
  (bind ?min (nth$ 1 ?sorted-series))
  (bind ?max (nth$ (length$ ?sorted-series) ?sorted-series))
  (bind ?interval-size (/ 1.0 ?*alphabet-size*))
  (loop-for-count ?i (length$ ?numerical-series) do
    (bind ?value (nth$ ?i ?numerical-series))
    (bind ?cdf-value (uniform-cdf ?value ?min ?max))
    (bind ?interval-index (if (>= ?cdf-value 1.0) then
                             (- ?*alphabet-size* 1)
                           else
                             (floor (/ ?cdf-value ?interval-size))))
    (bind ?char (nth$ (+ ?interval-index 1) ?*alphabet*))
    (bind ?linguistic-series (insert$ ?linguistic-series (length$ ?linguistic-series) ?char))
  )
  (return ?linguistic-series)
)

(deffunction build-precedence-matrix (?linguistic-series)
  (bind ?matrix (create$))
  (loop-for-count ?i ?*alphabet-size* do
    (bind ?row (create$))
    (loop-for-count ?j ?*alphabet-size* do
      (bind ?row (insert$ ?row (length$ ?row) 0))
    )
    (bind ?matrix (insert$ ?matrix (length$ ?matrix) ?row))
  )
  (loop-for-count ?i (- (length$ ?linguistic-series) 1) do
    (bind ?char1 (nth$ ?i ?linguistic-series))
    (bind ?char2 (nth$ (+ ?i 1) ?linguistic-series))
    (bind ?row-index (index$ ?char1 ?*alphabet*))
    (bind ?col-index (index$ ?char2 ?*alphabet*))
    (bind ?current (nth$ ?col-index (nth$ ?row-index ?matrix)))
    (bind ?new-row (subseq$ (nth$ ?row-index ?matrix) 0 ?col-index))
    (bind ?new-row (insert$ ?new-row ?col-index (+ ?current 1)))
    (bind ?new-row (insert$ ?new-row (length$ ?new-row) (subseq$ (nth$ ?row-index ?matrix) (+ ?col-index 1))))
    (bind ?matrix (insert$ (subseq$ ?matrix 0 ?row-index) ?row-index ?new-row))
    (bind ?matrix (insert$ ?matrix (length$ ?matrix) (subseq$ ?matrix (+ ?row-index 1))))
  )
  (return ?matrix)
)

(deffunction write-output-file (?linguistic-series ?matrix)
  (printout "Linguistic series: " crlf)
  (loop-for-count ?i (length$ ?linguistic-series) do
    (printout (nth$ ?i ?linguistic-series) " ")
  )
  (printout t crlf)
  (printout "Precedence matrix:" crlf "  ")
  (loop-for-count ?i ?*alphabet-size* do
    (printout (nth$ ?i ?*alphabet*) " ")
  )
  (printout t crlf)
  (loop-for-count ?i ?*alphabet-size* do
    (printout (nth$ ?i ?*alphabet*) " ")
    (loop-for-count ?j ?*alphabet-size* do
      (printout (nth$ ?j (nth$ ?i ?matrix)) " ")
    )
    (printout t crlf)
  )

  (bind ?output-file (open "output.txt" "w"))
  (if (not ?output-file) then
    (printout t "Could not open output file." crlf)
    (exit)
  )
  (printout ?output-file "Linguistic series: " crlf)
  (loop-for-count ?i (length$ ?linguistic-series) do
    (printout ?output-file (nth$ ?i ?linguistic-series) " ")
  )
  (printout ?output-file crlf crlf)
  (printout ?output-file "Precedence matrix:" crlf "  ")
  (loop-for-count ?i ?*alphabet-size* do
    (printout ?output-file (nth$ ?i ?*alphabet*) " ")
  )
  (printout ?output-file crlf)
  (loop-for-count ?i ?*alphabet-size* do
    (printout ?output-file (nth$ ?i ?*alphabet*) " ")
    (loop-for-count ?j ?*alphabet-size* do
      (printout ?output-file (nth$ ?j (nth$ ?i ?matrix)) " ")
    )
    (printout ?output-file crlf)
  )
  (close ?output-file)
)

(deffunction index$ (?char ?list)
  (loop-for-count ?i (length$ ?list) do
    (if (eq ?char (nth$ ?i ?list)) then
      (return ?i)
    )
  )
  (return -1)
)

(defrule main
  =>
  (read-params-file)
  (read-numerical-series)
  (bind ?sorted-series (merge-sort ?*numerical-series*))
  (bind ?linguistic-series (numerical-to-linguistic ?*numerical-series* ?sorted-series))
  (bind ?matrix (build-precedence-matrix ?linguistic-series))
  (write-output-file ?linguistic-series ?matrix)
  (printout t "Data written to output.txt" crlf)
)

(reset)
(run)
