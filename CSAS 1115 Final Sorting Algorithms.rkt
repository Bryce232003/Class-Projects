;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |CSAS 1115 Final Sorting Algorithms|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction) 
 
;; PURPOSE: To check if a vector is empty
(define (empty-VINT? low high) (> low high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUESTION 7
 
;; vector -> number
;; Purpose: finds the number of digits in the longest integer of (vectorof integer)
;; Assumption: there's no empty intervals, any given vector has 2 or more integers
(define (maxdigits V)
  (local [;; vector-large: vector -> number
          ;; purpose: finds the largest element of vector V
          (define (vector-large V)
            (local [;; [int int] num -> num
                    ;; To find the largest given element of the vector such that V(i) >= V(low)
                    ;; accum inv: max is the biggest number of the processed part of the vector
                    (define (large? low high max)
                      (cond [(empty-VINT? low high)max]
                            [(>(vector-ref V low) max)
                             (large? (add1 low) high (vector-ref V low))]
                            [else (large? (add1 low) high max)]))]
              (begin (abs-val V)
                     (large? 0 (sub1 (vector-length V))(vector-ref V 0)))))
          
          ;; CONTRACT: v -> (void)
          ;; PURPOSE: To find the absolute value
          (define (abs-val v)
            (local [;; num num -> num
                    ;; To convert the values of a vector into absolute values
                    (define (av-helper low high)
                      (if (empty-VINT? low high)
                          (void)
                          (begin
                            (vector-set! v low (abs (vector-ref v low)))
                            (av-helper (add1 low) high))))]
              (av-helper 0 (sub1 (vector-length v)))))

          ;; CONTRACT: num num -> num
          ;; PURPOSE: To find the amount of digits in the given number
          ;; accum inv: digits is the amount of times num has been divided by 10 so far
          (define (digit-count num digits)
            (if (< num 10)
                (add1 digits)
                (digit-count (/ num 10) (add1 digits))))]
    (digit-count (vector-large V) 0)))




 
(check-expect (maxdigits (vector 1 3 54 6)) 2)
(check-expect (maxdigits (vector 1 3 10 6)) 2)
(check-expect (maxdigits (vector 1 3 10 100)) 3)
(check-expect (maxdigits (vector 1 3347 10 6)) 4)
(check-expect (maxdigits (vector 1 99 10 6)) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUESTION 8
;; CONTRACT: num -> (void) 
;; PURPOSE: To create an interface that can add, dump, return the size, or return the elements of the given vector
(define (bucket-builder num)
  (local [;; PURPOSE: To build a bucket that holds the vector that can be manipulated
          (define bucket (build-vector num (λ (i) (void))))
  
          ;; PURPOSE: i is the index of the next available spot in the bucket
          (define i 0)
          
          ;; CONTRACT: int -> (void)
          ;; PURPOSE: To mutate a bucket to add a given integer
          (define (add! int) 
            (begin
              (vector-set! bucket i int)
              (set! i (add1 i))
              ))

          ;; CONTRACT: bucket index -> (void)
          ;; PURPOSE: To dump the bucket elements into the given vector starting
          ;; at the given index and mutates the bucket to become empty
          ;; EFFECTS: To empty the bucket into the original vector in the same order 
          (define (dump! vect index) 
            (local [;; num num num num -> (void)
                    ;; PURPOSE: To dump the bucket elements into the given vector starting
                    ;; at the given index and mutates the bucket to become empty
                    (define (dump-helper vlow vhigh blow bhigh)
                      (cond [(empty-VINT? blow bhigh)
                             (void)]
                            [(empty-VINT? vlow vhigh)
                             (error "Vector is too small for dumping")]
                            (else
                             (begin
                               (vector-set! vect vlow
                                            (vector-ref bucket blow))
                               (vector-set! bucket blow (void))
                               (dump-helper (add1 vlow) vhigh (add1 blow) bhigh)))))]
              (begin
                (dump-helper index (sub1 (vector-length vect))
                             0 (sub1 i))
                (set! i 0))))


          ;; PURPOSE: to manage the adding, dumping, inquiry of size,
          ;; and inquiry of elements of a bucket
          (define (bucket-interface m)
            (cond [(eq? m 'add) add!]
                  [(eq? m 'dump) dump!]
                  [(eq? m 'size) i]
                  [(eq? m 'elems) bucket]
                  (else "There is no function with that name")))]
    bucket-interface))


(define B1 (bucket-builder 10))
(define B2 (bucket-builder 3))

;;;;; WRAPPER FUNCTIONS

;; CONTRACT: bucket number -> (void)
;; PURPOSE: To add the number to the bucket
(define (bucket-add buckets number)
  ((buckets 'add) number))

;; CONTRACT: bucket vect index -> (void)
;; PURPOSE: To dump the number in the bucket to the vector
(define (bucket-dump buckets vect index)
  ((buckets 'dump) vect index))

;; CONTRACT: bucket  -> num
;; PURPOSE: To return the size of the given bucket
(define (bucket-size buckets)
  (buckets 'size))

;; CONTRACT: bucket  -> vector
;; PURPOSE: To return every element in the vector
(define (bucket-elems buckets)
  (buckets 'elems))
 
(check-expect (begin
                (bucket-add B1 34)
                (bucket-add B1 2)
                (bucket-size B1))
              2)

(check-expect (begin
                (bucket-add B1 34)
                (bucket-add B1 2)
                (bucket-elems B1))
              (vector 34 2 34 2 (void) (void) (void) (void) (void) (void)))

(check-expect (begin
                (bucket-add B2 45)
                (bucket-add B2 3)
                (bucket-dump B2 (build-vector 10 (λ (i) 0)) 2)
                (bucket-size B2))
              0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUESTION 9

;; CONTRACT: vect -> (void)
;; PURPOSE: To sort the vector in ascending order
;; EFFECT: It re-arranges the elements of the given vector in ascending order
(define (radix-sort! v)
  (local [;; This is the vector of buckets, its a vector that holds 10 buckets, one for each digit, each bucket is
          ;; (vector-length) long because at worst, the vector elements all have the same digit in the same
          ;; place
          (define vob (build-vector 10 (λ (i) (bucket-builder (vector-length v)))))
          
          ;; CONTRACT: low high num(digit) -> Void
          ;; PURPOSE: Placing vector elements into the appropriate buckets
          (define (bucketize! low high digit)
            (if (empty-VINT? low high) (void)
                (begin
                  (bucket-add (vector-ref vob (modulo (floor (/ (vector-ref v low)
                                                                digit))10))
                              (vector-ref v low))
                  (bucketize! (add1 low) high digit))))
 
          ;; CONTRACT: low high dindex-> (void)
          ;; PURPOSE: Dumps the buckets back out into the vector in order
          ;; EFFECT: It rearanges the vector elements to be sorted by a certain digit
          (define (debucketize! low high dindex)
            (if (empty-VINT? low high) (void) 
                (local [;;PURPOSE: To record the bucket size before it is dumped
                        (define temp-bucket (bucket-size (vector-ref vob low)))]
                  (begin
                    (bucket-dump (vector-ref vob low) v dindex)
                    (debucketize! (add1 low) high (+ temp-bucket dindex)))))) 
 
          ;; CONTRACT: num num num -> (void)
          ;; PURPOSE: To sort v
          (define (sort! digit calls-needed count)
            (if (< calls-needed count) (void)
                (begin 
                  (bucketize! 0 (sub1 (vector-length v)) digit)
                  (debucketize! 0 (sub1 (vector-length vob)) 0)
                  (sort! (* 10 digit) calls-needed (add1 count))))) ]
     
    (sort! 1 (maxdigits v) 1))) 
                  
(define UV (vector 45 21 3 565 1111 0))
 
(check-expect
 (begin (radix-sort! UV)
        UV)
 (vector 0 3 21 45 565 1111))

(define UV2 (vector 4 2 3 5 1 0))
 
(check-expect
 (begin (radix-sort! UV2)
        UV2)
 (vector 0 1 2 3 4 5 ))

(define UV4 (vector -1 3 45 399 -234))

#;(check-expect
   (begin (radix-sort! UV4)
          UV4)
   (vector -234 -1 3 45 399))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUESTION 10
;; Complexity Of Radix Sort?
 
;; What is time complexity?
;; Time complexity is the amount of time taken by an algorithm to run, as a function of
; the length of the input

;; T(n) = O(d*(n+b)), where d is the number of digits in the given list, n is the number
;; of elements in the list, and b is the base or bucket size used


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUESTION 11
;; INSERTION SORTING

;; CONTRACT: von -> (void)
;; PURPOSE: To sort the given vector via insertion sort
(define (ins-sort! v)
  (local [;; CONTRACT: Num num -> (void)
          ;; PURPOSE: To swap I and J in a vector
          (define (swap! i j)
            (local [(define temp (vector-ref v i))]
              (begin
                (vector-set! v i (vector-ref v j))
                (vector-set! v j temp))))

          ;; CONTRACT: low high -> (void)
          ;; PURPOSE: To insert low into (add1 low)
          (define (insert low high)
            (cond [(empty-VINT? low high)
                   (void)]
                  [(<= (vector-ref v low) (vector-ref v (add1 low)))
                   (void)]
                  [else (begin
                          (swap! low (add1 low))
                          (insert (add1 low) high)
                          )]))

          ;; CONTRACT: low high -> (void)
          ;; PURPOSE: To sort the given vector
          (define (sort! low high)
            (cond [(empty-VINT? low high)
                   (void)]
                  [else (begin 
                          (sort! (add1 low) high)
                          (insert low (sub1 high)))]))]
    (sort! 0 (sub1 (vector-length v)))))

;; TERMINATION ARGUMENT: low starts at 0, high starts at sub1 (vector-length v), with each recursive call
;; high decreases by 1, it will eventually reach 0 and that's when the function terminates
(define U3 (vector 827 1927 33 12))

(check-expect
 (begin (ins-sort! U3)
        U3)
 (vector 12 33 827 1927))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CODE GIVEN BY MARCO BELOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEAP SORT
;; (vectorof number) → (void)
;; Purpose: Sort the given vector in nondecreasing order
;; Effect: The given vector’s elements are rearranged in
;; nondecreasing order
(define (heap-sort-in-place! V)
  (local [;; trickle-down!: [int int] → (void)
          ;; Purpose: For the given VINTV, reestablish a heap
          ;; rooted at low
          ;; Effect: Vector elements are rearranged to have
          ;; a heap rooted at low
          ;; Assumption: V[low+1..high] are all heap roots
          (define (trickle-down! low high)
            (local
              [(define rc-index (right-heap-root low))
               (define lc-index (left-heap-root low))]
              (cond [(> lc-index high) (void)];; root has no children
                    [(> rc-index high) ;; root only has a left child
                     (if (<= (vector-ref V lc-index)
                             (vector-ref V low))
                         (void)
                         (begin 
                           (swap! low lc-index)
                           (trickle-down! lc-index high)))]
                    [else ;; root has two children
                     (local
                       [(define mc-index (if (>= (vector-ref V lc-index)
                                                 (vector-ref V rc-index))
                                             lc-index
                                             rc-index))]
                       (cond [(>= (vector-ref V low)
                                  (vector-ref V mc-index))
                              (void)]
                             [else (begin
                                     (swap! low mc-index)
                                     (trickle-down! mc-index high))]))])))
          ;; natnum natnum → (void) Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))
          ;; natnum → natnum
          ;; Purpose: Return the index for the right subheap’s root
          (define (right-heap-root parent-index)
            (+ (* 2 parent-index) 2))
          ;; natnum → natnum
          ;; Purpose: Return the index for the left subheap’s root
          (define (left-heap-root parent-index)
            (add1 (* 2 parent-index)))
          ;; natnum → natnum Purpose: Return the parent index in the heap
          ;; Assumption: the given index is in [1..(sub1 (vector-length V))]
          (define (parent i)
            (if (even? i)
                (quotient (sub1 i) 2)
                (quotient i 2)))
          ;; [int int] → (void)
          ;; Purpose: For the given VINTV, sort the vector elements
          ;; Effect: V’s elements in the given VINTV are rearranged
          ;; in nondecreasing order
          ;; Assumption: V[low..high] is a heap
          (define (sorter! low high)
            (cond [(> low high) (void)]
                  [else (begin
                          (swap! low high)
                          (trickle-down! low (sub1 high))
                          (sorter! low (sub1 high)))]))
          ;; . . . Purpose:
          ;; heapify!: [int int] → (void)
          ;; Purpose: Transform the elements in the given VINTV
          ;; into a heap
          ;; Effect: Rearrange the vector elements to form a heap
          ;; rooted at low
          ;; Assumptions:
          ;; low > 0 Given VINTV is valid for V
          ;;
          ;; Given VINTV is valid for V
          ;;
          ;; Elements indexed in [high+1..(sub1 (vector-length V))]
          ;; are heap roots
          (define (heapify! low high)
            (cond [(> low high) (void)]
                  [else
                   (local [(define parent-index (parent high))]
                     (cond [(>= (vector-ref V parent-index)
                                (vector-ref V high))
                            (heapify! low (sub1 high))]
                           [else
                            (begin
                              (swap! parent-index high)
                              (trickle-down! high
                                             (sub1 (vector-length V)))
                              (heapify! low (sub1 high)))]))]))]
    (begin
      (heapify! 1 (sub1 (vector-length V)))
      (sorter! 0 (sub1 (vector-length V))))
    ))

;; TERMINATION ARGUMENT: low starts at 0, high starts at sub1 (vector-length v), with each recursive call
;; high decreases by 1, it will eventually reach 0 and that's when the function terminates


(check-expect (begin
                (heap-sort-in-place! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (heap-sort-in-place! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46
                      60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (heap-sort-in-place! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (heap-sort-in-place! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUICK SORT
;; (vectorof number) → (void)
;; Purpose: Sort the given vector in nondecreasing order
;; Effect: Vector elements are rearranged in place in
;; nondecreasing order
(define (qs-in-place! V)
  (local [;; natnum natnum → (void) Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))
          ;; number [int int] → natnum
          ;; Purpose: If it exists, find smallest index ≥ low,
          ;; i, such that V[i] > given pivot if it exists.
          ;; Otherwise, return a value greater than high.
          (define (find> pivot low high)
            (if (or (> low high)
                    (> (vector-ref V low) pivot))
                low
                (find> pivot (add1 low) high)))
          ;; number [int int] → natnum
          ;; Purpose: Find the largest index, i, such that
          ;; V[i] ≤ to the given pivot
          ;; Assumption: V[low] = pivot
          (define (find<= pivot low high)
            (if (or (> low high)
                    (<= (vector-ref V high) pivot))
                high
                (find<= pivot low (sub1 high))));; number [int int] → natnum
          ;; Purpose: Return the position of the pivot in the
          ;; sorted V
          ;; How: The smallest index of a vector element > pivot
          ;; and the largest index of an element <= to the
          ;; pivot are found. If they form an empty vector
          ;; interval the largest index of an element <= to
          ;; the pivot is returned. Otherwise, the two
          ;; indexed values are swapped and the partitioning
          ;; process continues with the vector interval
          ;; formed by the two indices.
          ;; Effect: V’s elements are rearranged so that all
          ;; elements <= to the pivot are at the beginning
          ;; of the vector interval and all elements > the
          ;; pivot are at the end of the vector interval.
          (define (partition! pivot low high) 
            (local [(define first>pivot (find> pivot low high))
                    (define first<=pivot (find<= pivot low high))]
              (if (> first>pivot first<=pivot)
                  first<=pivot
                  (begin
                    (swap! first>pivot first<=pivot)
                    (partition! pivot
                                first>pivot
                                first<=pivot)))))
          ;; [int int] → (void)
          ;; Purpose: Sort V’s elements in the given vector interval
          ;; in nondecreasing order
          ;; How: The vector is partitioned in two. The first element
          ;; is placed in the vector position between the
          ;; elements ≤ to it and the elements > than it. The
          ;; The vector intervals for the two parts of the
          ;; partition are recursively sorted
          ;; Effect: Vector elements in the given vector interval are
          ;; rearranged in nondecreasing order.
          (define (qs-aux! low high)
            (if (> low high)
                (void)
                (local [(define pivot (vector-ref V low))
                        (define pivot-pos (partition! pivot low high))]
                  (begin
                    (swap! low pivot-pos)
                    (qs-aux! low (sub1 pivot-pos))
                    (qs-aux! (add1 pivot-pos) high)))))]
    (qs-aux! 0 (sub1 (vector-length V)))))

;; Termination Argument: recursivelly calling qs on each partition, increasing the high of one partition and decreasing the low
;; of the other, at one point their low and high will be equal, which is when the function terminates
;; A given nonempty vector interval is divided into two
;; smaller vector intervals and these are recursively
;; processed. Eventually, the given vector interval
;; becomes empty and the function halts.

(define V1 (vector 10 17 3 11 7))
(define V2 (vector 12 20 22 22 23 27 31 37 44 46
                   60 60 62 67 74 75 77 80 85 86))
(define V3 (vector 9 1 3 5 7 6 2 8 4 0))
(define V4 (vector 1 2 3 4 0 9 8 7 6 5))


(check-expect (begin
                (qs-in-place! V1)
                V1)
              (vector 3 7 10 11 17))
(check-expect (begin
                (qs-in-place! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46
                      60 60 62 67 74 75 77 80 85 86))
(check-expect (begin
                (qs-in-place! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))
(check-expect (begin
                (qs-in-place! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))



(define (time-sorts x)
  (cond [(> x 20000) (void)]
        [else (local [ (define V (build-vector x (lambda (i) (random 1000000))))
                       (define V1 (build-vector x (lambda (i) (random 1000000))))
                       (define V2 (build-vector x (lambda (i) (random 1000000))))
                       (define V3 (build-vector x (lambda (i) (random 1000000))))]
                (begin
                  (display (format "Quick Sort ~a" x))
                  (newline)
                  (time (qs-in-place! V))
                  (newline)
                  (display (format "Insertion Sort ~a" x))
                  (newline)
                  (time (ins-sort! V1))
                  (display (format "Heap Sort ~a" x))
                  (newline)
                  (time (heap-sort-in-place! V2))
                  (display (format "Radix Sort ~a" x))
                  (newline)
                  (time (radix-sort! V3))
                  (time-sorts (+ x 500))))]))
