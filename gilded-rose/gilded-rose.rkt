#lang racket/base

(require racket/contract
         racket/match)

(define normal-item-deterioration-rate 1)
(define expired-item-deterioration-rate 2)

;
; item
;
(struct item (name sell-in quality) #:transparent)

(define/contract (update-item the-item)
  (-> item? item?)
  (match the-item
    [(item name sell-in quality)
     #:when (> sell-in 0)
     (item name
           (sub1 sell-in)
           (max 0 (min 50 (- quality 1))))]

    [(item name
           sell-in
           quality)
     #:when (<= sell-in 0)
     (item name
           (sub1 sell-in)
           (max 0 (min 50 (- quality 2))))]))

;
; legendary-item
;
(define/contract (legendary-item? the-item)
  (-> item? boolean?)
  ; TODO: find a way to identify legendary items that doesn't require maintaining a list
  (define legendary-items (list "Sulfuras, Hand of Ragnaros"))
  (not (not (member (item-name the-item) legendary-items))))

(define/contract (update-legendary-item the-item)
  (-> item? item?)
  ; Nothing to do here, legendary items don't change in quality and don't need to be sold
  the-item)

;
; conjured-item
;
(define/contract (conjured-item? the-item)
  (-> item? boolean?)
  ; TODO: find a way to identify conjured items that doesn't require maintaining a list
  (define conjured-items (list "Conjured Mana Cake"))
  (not (not (member (item-name the-item) conjured-items))))

(define/contract (update-conjured-item the-item)
  (-> item? item?)
  (match the-item
    [(item name sell-in quality)
     #:when (> sell-in 0)
     (item name
           (sub1 sell-in)
           (max 0 (min 50 (- quality (* normal-item-deterioration-rate 2)))))]

    [(item name sell-in quality)
     #:when (<= sell-in 0)
     (item name
           (sub1 sell-in)
           (max 0 (min 50 (- quality (* expired-item-deterioration-rate 2)))))]))

;
; forever-appreciating-item
;
(define/contract (forever-appreciating-item? the-item)
  (-> item? boolean?)
  ; TODO: find a way to identify forever-appreciating items that doesn't require maintaining a list
  (define forever-appreciating-items (list "Aged Brie"))
  (not (not (member (item-name the-item) forever-appreciating-items))))

(define/contract (update-forever-appreciating-item the-item)
  (-> item? item?)
  (match the-item
    [(item name sell-in quality)
     #:when (> sell-in 0)
     (item name (sub1 sell-in) (min 50 (+ quality 1)))]

    [(item name sell-in quality)
     #:when (<= sell-in 0)
     (item name (sub1 sell-in) (min 50 (+ quality 2)))]))

;
; temporarily-appreciating-item
;
(define/contract (temporarily-appreciating-item? the-item)
  (-> item? boolean?)
  ; TODO: find a way to identify temporarily-appreciating items that doesn't require maintaining a list
  (define temporarily-appreciating-items (list "Backstage passes to a TAFKAL80ETC concert"))
  (not (not (member (item-name the-item) temporarily-appreciating-items))))

(define/contract (update-temporarily-appreciating-item the-item)
  (-> item? item?)
  (match the-item
    [(item name sell-in quality)
     #:when (> sell-in 10)
     (item name (sub1 sell-in) (min 50 (add1 quality)))]

    [(item name sell-in quality)
     #:when (and (> sell-in 5) (<= sell-in 10))
     (item name (sub1 sell-in) (min 50 (+ quality 2)))]

    [(item name sell-in quality)
     #:when (and (> sell-in 0) (<= sell-in 5))
     (item name (sub1 sell-in) (min 50 (+ quality 3)))]

    [(item name sell-in quality)
     #:when (<= sell-in 0)
     (item name (sub1 sell-in) 0)]))

;
; update-quality
;
(define (update-quality items) 
  (map (lambda (the-item)
         (match the-item
           [(? legendary-item?) (update-legendary-item the-item)]
           [(? conjured-item?) (update-conjured-item the-item)]
           [(? forever-appreciating-item?) (update-forever-appreciating-item the-item)]
           [(? temporarily-appreciating-item?) (update-temporarily-appreciating-item the-item)]
           [(? item?) (update-item the-item)]))
       items))

;
; exports
;
(provide
 (struct-out item)
 update-quality)

;
; tests - as they live in a submodule, running the app shouldn't incur any performance overhead
;
(module+ test
  (require rackunit rackunit/text-ui)

  (define
    item-tests
    (test-suite
     "Normal items"
     (test-equal?
      "Not expired"
      (update-quality (list (item "Foo" 5 50))) (list (item "Foo" 4 49)))
     (test-equal?
      "Expired"
      (update-quality (list (item "Foo" 0 50))) (list (item "Foo" -1 48)))
     (test-equal?
      "Ridiculously high-quality item"
      (update-quality (list (item "Foo" 0 55))) (list (item "Foo" -1 50)))
     ))

  (define
    conjured-item-tests
    (test-suite
     "Conjured items"
     (test-equal?
      "Not expired"
      (update-quality (list (item "Conjured Mana Cake" 5 50))) (list (item "Conjured Mana Cake" 4 48)))
     (test-equal?
      "Expired"
      (update-quality (list (item "Conjured Mana Cake" 0 50))) (list (item "Conjured Mana Cake" -1 46)))
     (test-equal?
      "Ridiculously high-quality item"
      (update-quality (list (item "Conjured Mana Cake" 0 55))) (list (item "Conjured Mana Cake" -1 50)))
     ))

  (define
    legendary-item-tests
    (test-suite
     "Legendary items"
     (test-equal?
      "Not expired"
      (update-quality (list (item "Sulfuras, Hand of Ragnaros" 5 80)))
      (list (item "Sulfuras, Hand of Ragnaros" 5 80)))
     (test-equal?
      "Expired"
      (update-quality (list (item "Sulfuras, Hand of Ragnaros" -200 80)))
      (list (item "Sulfuras, Hand of Ragnaros" -200 80)))
     ))

  (define
    forever-appreciating-item-tests
    (test-suite
     "Forever appreciating items"
     (test-equal?
      "Not expired"
      (update-quality (list (item "Aged Brie" 5 20))) (list (item "Aged Brie" 4 21)))
     (test-equal?
      "Expired"
      (update-quality (list (item "Aged Brie" -5 20))) (list (item "Aged Brie" -6 22)))
     (test-equal?
      "Ridiculously high-quality"
      (update-quality (list (item "Aged Brie" -5 55))) (list (item "Aged Brie" -6 50)))
     ))

  (define
    temporarily-appreciating-item-tests
    (test-suite
     "Temporarily appreciating items"
     (test-equal?
      "More than 10 days before concert"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 20 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" 19 21)))
     (test-equal?
      "10 days before concert"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 10 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" 9 22)))
     (test-equal?
      "6 days before concert"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 6 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" 5 22)))
     (test-equal?
      "5 days before concert"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 5 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" 4 23)))
     (test-equal?
      "1 day before concert"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 1 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" 0 23)))
     (test-equal?
      "1 day before concert - ridiculously valuable to begin with"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 1 55)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" 0 50)))
     (test-equal?
      "Concert day"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" 0 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" -1 0)))
     (test-equal?
      "1 day after concert"
      (update-quality (list (item "Backstage passes to a TAFKAL80ETC concert" -1 20)))
      (list (item "Backstage passes to a TAFKAL80ETC concert" -2 0)))
     ))

  (run-tests
   (test-suite
    "all tests"
    item-tests
    conjured-item-tests
    legendary-item-tests
    forever-appreciating-item-tests
    temporarily-appreciating-item-tests)))
