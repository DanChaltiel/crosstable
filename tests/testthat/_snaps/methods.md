# transpose works

    Code
      ct2 = t(ct)
      class(ct2)
    Output
      [1] "transposed_crosstable" "crosstable"            "tbl_df"               
      [4] "tbl"                   "data.frame"           
    Code
      ct2
    Output
      # A tibble: 40 x 6
         .id   letter variable   `the X`          `the Y`          `the Z`         
         <chr> <chr>  <chr>      <chr>            <chr>            <chr>           
       1 a     a      Min / Max  -1.1 / -0.5      -0.5 / 2.5       -0.6 / 0.3      
       2 a     a      Med [IQR]  -0.6 [-0.8;-0.5] -0.2 [-0.3;1.2]  -0.5 [-0.6;-0.1]
       3 a     a      Mean (std) -0.7 (0.3)       0.6 (1.7)        -0.3 (0.5)      
       4 a     a      N (NA)     3 (0)            3 (0)            3 (0)           
       5 b     b      Min / Max  -1.1 / -0.7      -1.4 / -0.03     -1.1 / 1.7      
       6 b     b      Med [IQR]  -0.9 [-1.0;-0.8] -0.4 [-0.9;-0.2] 0.1 [-0.5;0.9]  
       7 b     b      Mean (std) -0.9 (0.2)       -0.6 (0.7)       0.2 (1.4)       
       8 b     b      N (NA)     3 (0)            3 (0)            3 (0)           
       9 c     c      Min / Max  -1.0 / -0.3      -0.7 / 0.6       -0.5 / 1.0      
      10 c     c      Med [IQR]  -0.5 [-0.8;-0.4] -0.2 [-0.4;0.2]  0.8 [0.2;0.9]   
      # ... with 30 more rows
    Code
      af(ct2)$header$dataset
    Output
        letter variable the X the Y the Z
      1 letter variable label label label
      2 letter variable the X the Y the Z

