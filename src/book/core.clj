(ns book.core)
(ns book.core
  (:require [clojure.test :refer [is run-tests with-test]])
  )

;TODO Write some tests

(let [books '({:title "Romeo and Juliet" :author-id 890}
              {:title "Moby Dick" :author-id 123}
              {:title "The Idiot" :author-id 567}
              {:title "Bartleby, the Scrivener" :author-id 123}
              {:title "Crime and Punishment" :author-id 567}
              {:title "Of Mice and Men" :author-id 248}
              {:title "Harry Potter and the Chamber of Secrets" :author-id 135}
              )

      authors {123 "Herman Melville" 567 "Fyodor Dostoevsky"
               890 "William Shakespeare" 135 "J.K. Rowling"}
      ]
  (def catalog {:books books :authors authors})
  )

(with-test (defn search [in-title catalog]
             "   - Parameter query is a substring of the title;
                 - Parameter catalog is a map {:books list-of-books :authors map-of-author-ids-to-author-name
                   A book in the list in the catalog takes the form {:title \"Moby Dick\" :author-id 123}.
                   The map of authors in the catalog takes the form  {123 \"Herman Melville\" 567 \"Fyodor Dostoevsky\" }
                 - Returns list of books whose title contain the query.
                 - An example of a book in that return-value list  is
                   {:title \"Moby Dick\" :author \"Herman Melville\"}
                 "
             (let [books (filter (fn [bk] (clojure.string/includes? (:title bk) in-title))
                                 (:books catalog))
                   books-with-or-without-author (for [bk books] {:title (:title bk) :author (get (:authors catalog) (:author-id bk))})
                   books-with-author (for [bk books-with-or-without-author :when (:author bk)] bk)]
               books-with-author
               )
             )
           (is (some #(= "William Shakespeare" (:author %)) (search "and" catalog)))
           (is (not-any? #(= "Herman Melville" (:author %)) (search "and" catalog)))
           (is (not-any? #(= "Of Mice and Men" (:title %)) (search "and" catalog)) "Exclude books with missing author")
           )


(run-tests 'book.core)
