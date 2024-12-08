#lang racket
(require data-science
         plot
         srfi/19)

;; Example tweet data
(define tweets
  '(("Happy to be in Uganda!" "Uganda" "2024-01-10")
    ("Such a beautiful day!" "Uganda" "2024-02-15")
    ("I hate waiting in traffic." "Uganda" "2024-03-05")
    ("Feeling sad about the news." "Uganda" "2024-04-25")
    ("The weather in Kampala is amazing." "Uganda" "2024-05-10")
    ("Traffic jam is unbearable today." "Uganda" "2024-03-15")
    ("Proud to be Ugandan on this special day!" "Uganda" "2024-07-20")
    ("This new policy is a disaster." "Uganda" "2024-01-25")
    ("Excited for the upcoming elections!" "Uganda" "2024-09-15")))

;; Convert timestamps to Racket date objects
(define (convert-timestamp str)
  (string->date str "~Y-~m-~d"))

;; Group tweets by month
(define (group-tweets-by-month tweets)
  (let ([grouped (make-hash)])
    (for-each
     (λ (tweet)
       (let* ([date (convert-timestamp (list-ref tweet 2))]
              [month (date->string date "~Y-~m")]
              [existing (hash-ref grouped month '())])
         (hash-set! grouped month (cons tweet existing))))
     tweets)
    grouped))

;; Combine all tweet texts into a single document
(define (combine-tweet-texts tweets)
  (string-join (map (λ (tweet) (list-ref tweet 0)) tweets) " "))

;; Tokenize combined text and calculate word frequencies
(define (tokenize-and-count-with-doc tweets)
  (document->tokens (combine-tweet-texts tweets) #:sort? #t))

;; Perform sentiment analysis with tokenized and counted data
(define (analyze-sentiments tokens)
  (filter
   (λ (entry) 
     (and (list? entry)
          (= (length entry) 3)
          (number? (list-ref entry 2)))) ; Ensure freq is numeric
   (list->sentiment tokens #:lexicon 'nrc)))

;; Aggregate sentiments by type
(define (aggregate-sentiments sentiments)
  (let ([aggregated (make-hash)])
    (for-each
     (λ (sentiment)
       (let* ([sentiment-type (list-ref sentiment 1)]
              [freq (if (number? (list-ref sentiment 2))
                        (list-ref sentiment 2)
                        (string->number (list-ref sentiment 2)))] ; Handle both number and string
              [existing (hash-ref aggregated sentiment-type 0)])
         (hash-set! aggregated sentiment-type (+ existing freq))))
     sentiments)
    aggregated))

;; Group, tokenize, count, and analyze
(define grouped-tweets (group-tweets-by-month tweets))
(define sentiments-by-month
  (for/hash ([month (hash-keys grouped-tweets)])
    (values month 
            (aggregate-sentiments
             (analyze-sentiments
              (tokenize-and-count-with-doc (hash-ref grouped-tweets month)))))))

;; Debug Outputs
(displayln "Sentiments by Month:")
(for ([month (hash-keys sentiments-by-month)])
  (printf "Month: ~a\nSentiments: ~a\n\n" month (hash-ref sentiments-by-month month)))

(define (visualize-sentiments sentiments-by-month)
  (parameterize ([plot-width 800]
                 [plot-height 600]
                 [plot-x-label "Sentiment"]
                 [plot-y-label "Frequency"])
    (plot
     (list
      (tick-grid)
      (for/list ([month (sort (hash-keys sentiments-by-month) string<?)])
        (let* ([month-sentiments (hash-ref sentiments-by-month month)]
               [data (hash->list month-sentiments)] ; Convert hash to list
               [formatted-data (map (λ (pair) (list (car pair) (cdr pair))) data)]) ; Format data
          (discrete-histogram
           formatted-data
           #:color "MediumSlateBlue"
           #:line-color "MediumSlateBlue"
           #:label month)))))))



;; Call Visualization
(visualize-sentiments sentiments-by-month)