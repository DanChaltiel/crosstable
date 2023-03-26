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
       1 a     a      Min / Max  -0.6 / 1.5       -0.2 / 1.4       -0.6 / 2.4      
       2 a     a      Med [IQR]  0.9 [0.1;1.2]    0.4 [0.1;0.9]    0.5 [-0.05;1.4] 
       3 a     a      Mean (std) 0.6 (1.1)        0.5 (0.8)        0.8 (1.5)       
       4 a     a      N (NA)     3 (0)            3 (0)            3 (0)           
       5 b     b      Min / Max  0.2 / 0.8        -0.6 / -0.1      -0.7 / -0.04    
       6 b     b      Med [IQR]  0.4 [0.3;0.6]    -0.3 [-0.4;-0.2] -0.1 [-0.4;-0.1]
       7 b     b      Mean (std) 0.5 (0.3)        -0.3 (0.3)       -0.3 (0.4)      
       8 b     b      N (NA)     3 (0)            3 (0)            3 (0)           
       9 c     c      Min / Max  -0.8 / 0.1       0.3 / 0.7        0.6 / 1.2       
      10 c     c      Med [IQR]  -0.6 [-0.7;-0.3] 0.4 [0.4;0.5]    0.7 [0.7;0.9]   
      # i 30 more rows
    Code
      af(ct2)$header$dataset
    Output
        letter variable the X the Y the Z
      1 letter variable label label label
      2 letter variable the X the Y the Z

