# stylometry_opt_arabic

Stylometric test for optimal parameters with Arabic novels. The main goal of the test: take 2 slices of text from the same books and try to match them to each other using a large variety of parameters in ordr to establish the optimal parameters for Arabic texts.

# Final results structure:

Results are aggregared into a TSV file with the following columns.

- `author_book` :: the `stylo` format ID of a book
- `clustering_method` :: clustering/linkage pmethod
- `book_cluster_match` :: was the clustering on the book level successful? 1 is yes, the book was matched to itself; 0 is no, the book was not matched to itself 
- `author_cluster_match` :: was the clustering on the author level successful? 1 is yes, the book was matched to the correct author; 0 is no, the book was not matched to the correct author 
- `distance_method` :: distance method used
- `dist_value` :: the distance value
- `book_dist_matched` (`book_match` in temp files) :: the book to which the main book was matched --- through the shortest distance
- `book_dist_match` :: was the matching through distance on the book level successful? 1 is yes, the book was matched to itself; 0 is no, the book was not matched to itself
- `author_dist_match` :: was the matching through distance on the author level successful? 1 is yes, the book was matched to the correct author; 0 is no, the book was not matched to the correct author
- `feature` :: features, "w" --- word/token; "c" -- character; the number that follows indicates the length of the ngram used, i.e. "w1" --- single tokens/words; "w2" --- token/word bigrams
- `mff` :: current limit of the *most frequent features* used
- `culling` :: culling parameter, in %, i.e. 50 means that only *mff* that occur in 50% of tested slices were used in the stylometric test
- `slices_compared` :: the number of slices used in comparison (default is 2); slices are consecutive
- `slice_len` :: the length of the slice