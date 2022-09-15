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
        .id label           A           B          C NA
      1   x the X 11 (44.00%)  7 (28.00%) 7 (28.00%)  5
      2   y the Y  6 (20.00%) 17 (56.67%) 7 (23.33%)  0
      3   z the Z 10 (47.62%)  3 (14.29%) 8 (38.10%)  9

