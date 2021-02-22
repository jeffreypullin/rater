# rater errors correctly

    Code
      rater(data.frame(item = 0, rater = 0, rating = 0), dawid_skene())
    Error <simpleError>
      
      * Some item indexes are 0. All indexes must be in 1:I where I is the number of items.
      * Some rater indexes are 0. All indexes must be in 1:J where J is the number of raters.
      * Some ratings are 0. All ratings must be in 1:K where K is the number of classes.

---

    Code
      rater(data.frame(thing = 0, n = 0), dawid_skene(), data_format = "grouped")
    Error <simpleError>
      
      * All elements of the column `n` must be > 0.
      * Some ratings are 0. All ratings must be in 1:K where K is the number of classes.

