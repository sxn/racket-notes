#lang racket

(define example "Fred:Corwill;Wilfred:Corwill;Barney:Tornbull;Betty:Tornbull;Bjon:Tornbull;Raphael:Corwill;Alfred:Corwill")

(define (meeting s)
  (define people
    (for/list ([person (in-list (string-split (string-upcase s) ";" #:trim? #t #:repeat? #t))])
      (string-split person ":")))

  (define (sorter a b)
    (string<=? (string-join (reverse a) " ")
               (string-join (reverse b) " ")))

  (apply
   string-append
   (for/list ([person (in-list (sort people sorter))])
     (string-append "(" (string-join (reverse person) ", ") ")"))))


(module+ test
  (require rackunit
           rackunit/text-ui)

  (define tests
    (test-suite
     "Meetings"
     (test-case "Example"
       (check-equal?
        (meeting "Fred:Corwill;Wilfred:Corwill;Barney:Tornbull;Betty:Tornbull;Bjon:Tornbull;Raphael:Corwill;Alfred:Corwill")
        "(CORWILL, ALFRED)(CORWILL, FRED)(CORWILL, RAPHAEL)(CORWILL, WILFRED)(TORNBULL, BARNEY)(TORNBULL, BETTY)(TORNBULL, BJON)"))
     (test-case "Simple case 1"
       (check-equal?
        (meeting "Alexis:Wahl;John:Bell;Victoria:Schwarz;Abba:Dorny;Grace:Meta;Ann:Arno;Madison:STAN;Alex:Cornwell;Lewis:Kern;Megan:Stan;Alex:Korn" )
        "(ARNO, ANN)(BELL, JOHN)(CORNWELL, ALEX)(DORNY, ABBA)(KERN, LEWIS)(KORN, ALEX)(META, GRACE)(SCHWARZ, VICTORIA)(STAN, MADISON)(STAN, MEGAN)(WAHL, ALEXIS)"))
     (test-case "Simple case 2"
       (check-equal?
        (meeting "John:Gates;Michael:Wahl;Megan:Bell;Paul:Dorries;James:Dorny;Lewis:Steve;Alex:Meta;Elizabeth:Russel;Anna:Korn;Ann:Kern;Amber:Cornwell")
        "(BELL, MEGAN)(CORNWELL, AMBER)(DORNY, JAMES)(DORRIES, PAUL)(GATES, JOHN)(KERN, ANN)(KORN, ANNA)(META, ALEX)(RUSSEL, ELIZABETH)(STEVE, LEWIS)(WAHL, MICHAEL)"))
     (test-case "Failed test"
       (check-equal?
        (meeting "James:Wahl;Ann:Tolkien;Paul:Kern;Michael:Russel;Grace:Kern;Emily:Dorny;Anna:Russell;Alex:Cornwell;Alexis:Meta;James:Thorensen;Amandy:Tolkien;Andrew:Schwarz;James:Wahl;Sophia:Bell;Sarah:Cornwell")
        "(BELL, SOPHIA)(CORNWELL, ALEX)(CORNWELL, SARAH)(DORNY, EMILY)(KERN, GRACE)(KERN, PAUL)(META, ALEXIS)(RUSSEL, MICHAEL)(RUSSELL, ANNA)(SCHWARZ, ANDREW)(THORENSEN, JAMES)(TOLKIEN, AMANDY)(TOLKIEN, ANN)(WAHL, JAMES)(WAHL, JAMES)"))))

  (run-tests tests))
