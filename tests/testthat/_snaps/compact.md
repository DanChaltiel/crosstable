# Compact method OK with data.frame

    Code
      ct_compact(df, name_from = "Species")
    Output
            Species   variable Sepal.Length Sepal.Width Petal.Length Petal.Width
      1      setosa     setosa           NA          NA           NA          NA
      2      setosa                     5.1         3.5          1.4         0.2
      3      setosa                     4.9         3.0          1.4         0.2
      4      setosa                     4.7         3.2          1.3         0.2
      5      setosa                     4.6         3.1          1.5         0.2
      6      setosa                     5.0         3.6          1.4         0.2
      7  versicolor versicolor           NA          NA           NA          NA
      8  versicolor                     7.0         3.2          4.7         1.4
      9  versicolor                     6.4         3.2          4.5         1.5
      10 versicolor                     6.9         3.1          4.9         1.5
      11 versicolor                     5.5         2.3          4.0         1.3
      12 versicolor                     6.5         2.8          4.6         1.5
      13  virginica  virginica           NA          NA           NA          NA
      14  virginica                     6.3         3.3          6.0         2.5
      15  virginica                     5.8         2.7          5.1         1.9
      16  virginica                     7.1         3.0          5.9         2.1
      17  virginica                     6.3         2.9          5.6         1.8
      18  virginica                     6.5         3.0          5.8         2.2
    Code
      ct_compact(df, name_from = "Species", name_to = "Petal.Length")
    Output
            Species Petal.Length Sepal.Length Sepal.Width Petal.Width
      1      setosa       setosa           NA          NA          NA
      2      setosa          1.4          5.1         3.5         0.2
      3      setosa          1.4          4.9         3.0         0.2
      4      setosa          1.3          4.7         3.2         0.2
      5      setosa          1.5          4.6         3.1         0.2
      6      setosa          1.4          5.0         3.6         0.2
      7  versicolor   versicolor           NA          NA          NA
      8  versicolor          4.7          7.0         3.2         1.4
      9  versicolor          4.5          6.4         3.2         1.5
      10 versicolor          4.9          6.9         3.1         1.5
      11 versicolor            4          5.5         2.3         1.3
      12 versicolor          4.6          6.5         2.8         1.5
      13  virginica    virginica           NA          NA          NA
      14  virginica            6          6.3         3.3         2.5
      15  virginica          5.1          5.8         2.7         1.9
      16  virginica          5.9          7.1         3.0         2.1
      17  virginica          5.6          6.3         2.9         1.8
      18  virginica          5.8          6.5         3.0         2.2
    Code
      df$Species2 = substr(df$Species, 1, 1)
      ct_compact(df, name_from = "Species", name_to = "Petal.Length", wrap_cols = "Species2")
    Output
            Species Petal.Length Sepal.Length Sepal.Width Petal.Width Species2
      1      setosa       setosa           NA          NA          NA     <NA>
      2      setosa          1.4          5.1         3.5         0.2        s
      3      setosa          1.4          4.9         3.0         0.2        s
      4      setosa          1.3          4.7         3.2         0.2        s
      5      setosa          1.5          4.6         3.1         0.2        s
      6      setosa          1.4          5.0         3.6         0.2        s
      7  versicolor   versicolor           NA          NA          NA     <NA>
      8  versicolor          4.7          7.0         3.2         1.4        v
      9  versicolor          4.5          6.4         3.2         1.5        v
      10 versicolor          4.9          6.9         3.1         1.5        v
      11 versicolor            4          5.5         2.3         1.3        v
      12 versicolor          4.6          6.5         2.8         1.5        v
      13  virginica    virginica           NA          NA          NA     <NA>
      14  virginica            6          6.3         3.3         2.5        v
      15  virginica          5.1          5.8         2.7         1.9        v
      16  virginica          5.9          7.1         3.0         2.1        v
      17  virginica          5.6          6.3         2.9         1.8        v
      18  virginica          5.8          6.5         3.0         2.2        v

# Compact method OK with crosstable

    Code
      ct_compact(ct)
    Output
      # A tibble: 17 x 4
         .id   variable              straight           vshaped            
       * <chr> <chr>                 <chr>              <chr>              
       1 disp  Displacement (cu.in.) <NA>               <NA>               
       2 disp  Min / Max             71.1 / 258.0       120.3 / 472.0      
       3 disp  Med [IQR]             120.5 [83.0;162.4] 311.0 [275.8;360.0]
       4 disp  Mean (std)            132.5 (56.9)       307.1 (106.8)      
       5 disp  N (NA)                14 (0)             18 (0)             
       6 hp    Gross horsepower      <NA>               <NA>               
       7 hp    Min / Max             52.0 / 123.0       91.0 / 335.0       
       8 hp    Med [IQR]             96.0 [66.0;109.8]  180.0 [156.2;226.2]
       9 hp    Mean (std)            91.4 (24.4)        189.7 (60.3)       
      10 hp    N (NA)                14 (0)             18 (0)             
      11 cyl   Number of cylinders   <NA>               <NA>               
      12 cyl   4                     10 (90.91%)        1 (9.09%)          
      13 cyl   6                     4 (57.14%)         3 (42.86%)         
      14 cyl   8                     0 (0%)             14 (100.00%)       
      15 am    Transmission          <NA>               <NA>               
      16 am    auto                  7 (36.84%)         12 (63.16%)        
      17 am    manual                7 (53.85%)         6 (46.15%)         
    Code
      ct_compact(ct, name_from = ".id")
    Output
      # A tibble: 17 x 5
         .id   variable   label                 straight           vshaped            
       * <chr> <chr>      <chr>                 <chr>              <chr>              
       1 disp  disp       <NA>                  <NA>               <NA>               
       2 disp  Min / Max  Displacement (cu.in.) 71.1 / 258.0       120.3 / 472.0      
       3 disp  Med [IQR]  Displacement (cu.in.) 120.5 [83.0;162.4] 311.0 [275.8;360.0]
       4 disp  Mean (std) Displacement (cu.in.) 132.5 (56.9)       307.1 (106.8)      
       5 disp  N (NA)     Displacement (cu.in.) 14 (0)             18 (0)             
       6 hp    hp         <NA>                  <NA>               <NA>               
       7 hp    Min / Max  Gross horsepower      52.0 / 123.0       91.0 / 335.0       
       8 hp    Med [IQR]  Gross horsepower      96.0 [66.0;109.8]  180.0 [156.2;226.2]
       9 hp    Mean (std) Gross horsepower      91.4 (24.4)        189.7 (60.3)       
      10 hp    N (NA)     Gross horsepower      14 (0)             18 (0)             
      11 cyl   cyl        <NA>                  <NA>               <NA>               
      12 cyl   4          Number of cylinders   10 (90.91%)        1 (9.09%)          
      13 cyl   6          Number of cylinders   4 (57.14%)         3 (42.86%)         
      14 cyl   8          Number of cylinders   0 (0%)             14 (100.00%)       
      15 am    am         <NA>                  <NA>               <NA>               
      16 am    auto       Transmission          7 (36.84%)         12 (63.16%)        
      17 am    manual     Transmission          7 (53.85%)         6 (46.15%)         

