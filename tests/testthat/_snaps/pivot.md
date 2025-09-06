# pivot

    Code
      pivot_crosstable(ct)
    Output
      # A tibble: 3 x 6
        .id   label A           B           C          `NA` 
        <chr> <chr> <chr>       <chr>       <chr>      <chr>
      1 x     the X 11 (44.00%) 7 (28.00%)  7 (28.00%) 5    
      2 y     the Y 6 (20.00%)  17 (56.67%) 7 (23.33%) 0    
      3 z     the Z 10 (47.62%) 3 (14.29%)  8 (38.10%) 9    
    Code
      pivot_crosstable(ct) %>% af(T) %>% {
        .$header$dataset
      }
    Output
        .id label        A        B        C       NA
      1 .id label Variable Variable Variable Variable
      2 .id label        A        B        C       NA
    Code
      pivot_crosstable(ct) %>% af(by_header = "foobar")
    Output
      a flextable object.
      col_keys: `label`, `A`, `B`, `C`, `NA` 
      header has 2 row(s) 
      body has 3 row(s) 
      original dataset sample: 
      'data.frame':	3 obs. of  6 variables:
       $ .id  : chr  "x" "y" "z"
       $ label: chr  "the X" "the Y" "the Z"
       $ A    : chr  "11 (44.00%)" "6 (20.00%)" "10 (47.62%)"
       $ B    : chr  "7 (28.00%)" "17 (56.67%)" "3 (14.29%)"
       $ C    : chr  "7 (28.00%)" "7 (23.33%)" "8 (38.10%)"
       $ NA   : chr  "5" "0" "9"
       - attr(*, "debug")=List of 3
        ..$ interface: chr "quosure"
        ..$ x_class  : Named chr [1:3] "character" "character" "character"
        .. ..- attr(*, "names")= chr [1:3] "x" "y" "z"
        ..$ y_class  : chr(0) 
       - attr(*, "N")= int 30
       - attr(*, "showNA")= chr "ifany"
       - attr(*, "variables")= chr [1:3] "x" "y" "z"
       - attr(*, "has_test")= logi FALSE
       - attr(*, "has_effect")= logi FALSE
       - attr(*, "has_total")= num 0
       - attr(*, "has_label")= logi TRUE
       - attr(*, "by_levels")=List of 1
        ..$ Variable: chr [1:4] "A" "B" "C" "NA"
       - attr(*, "by")= chr "variable"
       - attr(*, "by_label")= chr "Variable"

