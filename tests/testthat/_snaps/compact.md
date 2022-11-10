# Compact method OK with data.frame

    Code
      ct_compact(df, name_from = "Species")
    Output
           variable Sepal.Length Sepal.Width Petal.Length Petal.Width
      1      setosa                                                  
      2                      5.1         3.5          1.4         0.2
      3                      4.9           3          1.4         0.2
      4                      4.7         3.2          1.3         0.2
      5                      4.6         3.1          1.5         0.2
      6                        5         3.6          1.4         0.2
      7  versicolor                                                  
      8                        7         3.2          4.7         1.4
      9                      6.4         3.2          4.5         1.5
      10                     6.9         3.1          4.9         1.5
      11                     5.5         2.3            4         1.3
      12                     6.5         2.8          4.6         1.5
      13  virginica                                                  
      14                     6.3         3.3            6         2.5
      15                     5.8         2.7          5.1         1.9
      16                     7.1           3          5.9         2.1
      17                     6.3         2.9          5.6         1.8
      18                     6.5           3          5.8         2.2
    Code
      ct_compact(df, name_from = "Species", name_to = "Petal.Length")
    Output
         Petal.Length Sepal.Length Sepal.Width Petal.Width
      1        setosa                                     
      2           1.4          5.1         3.5         0.2
      3           1.4          4.9           3         0.2
      4           1.3          4.7         3.2         0.2
      5           1.5          4.6         3.1         0.2
      6           1.4            5         3.6         0.2
      7    versicolor                                     
      8           4.7            7         3.2         1.4
      9           4.5          6.4         3.2         1.5
      10          4.9          6.9         3.1         1.5
      11            4          5.5         2.3         1.3
      12          4.6          6.5         2.8         1.5
      13    virginica                                     
      14            6          6.3         3.3         2.5
      15          5.1          5.8         2.7         1.9
      16          5.9          7.1           3         2.1
      17          5.6          6.3         2.9         1.8
      18          5.8          6.5           3         2.2
    Code
      df$Species2 = substr(df$Species, 1, 1)
      ct_compact(df, name_from = "Species", name_to = "Petal.Length", wrap_cols = "Species2")
    Output
         Petal.Length Sepal.Length Sepal.Width Petal.Width Species2
      1        setosa                                             s
      2           1.4          5.1         3.5         0.2         
      3           1.4          4.9           3         0.2         
      4           1.3          4.7         3.2         0.2         
      5           1.5          4.6         3.1         0.2         
      6           1.4            5         3.6         0.2         
      7    versicolor                                             v
      8           4.7            7         3.2         1.4         
      9           4.5          6.4         3.2         1.5         
      10          4.9          6.9         3.1         1.5         
      11            4          5.5         2.3         1.3         
      12          4.6          6.5         2.8         1.5         
      13    virginica                                             v
      14            6          6.3         3.3         2.5         
      15          5.1          5.8         2.7         1.9         
      16          5.9          7.1           3         2.1         
      17          5.6          6.3         2.9         1.8         
      18          5.8          6.5           3         2.2         

# Compact method OK with crosstable

    Code
      ct_compact(ct)
    Output
                      variable           straight             vshaped
      1  Displacement (cu.in.)                                       
      2              Min / Max       71.1 / 258.0       120.3 / 472.0
      3              Med [IQR] 120.5 [83.0;162.4] 311.0 [275.8;360.0]
      4             Mean (std)       132.5 (56.9)       307.1 (106.8)
      5                 N (NA)             14 (0)              18 (0)
      6       Gross horsepower                                       
      7              Min / Max       52.0 / 123.0        91.0 / 335.0
      8              Med [IQR]  96.0 [66.0;109.8] 180.0 [156.2;226.2]
      9             Mean (std)        91.4 (24.4)        189.7 (60.3)
      10                N (NA)             14 (0)              18 (0)
      11   Number of cylinders                                       
      12                     4        10 (90.91%)           1 (9.09%)
      13                     6         4 (57.14%)          3 (42.86%)
      14                     8             0 (0%)        14 (100.00%)
      15          Transmission                                       
      16                  auto         7 (36.84%)         12 (63.16%)
      17                manual         7 (53.85%)          6 (46.15%)
    Code
      ct_compact(ct, name_from = ".id")
    Output
           variable           straight             vshaped
      1        disp                                       
      2   Min / Max       71.1 / 258.0       120.3 / 472.0
      3   Med [IQR] 120.5 [83.0;162.4] 311.0 [275.8;360.0]
      4  Mean (std)       132.5 (56.9)       307.1 (106.8)
      5      N (NA)             14 (0)              18 (0)
      6          hp                                       
      7   Min / Max       52.0 / 123.0        91.0 / 335.0
      8   Med [IQR]  96.0 [66.0;109.8] 180.0 [156.2;226.2]
      9  Mean (std)        91.4 (24.4)        189.7 (60.3)
      10     N (NA)             14 (0)              18 (0)
      11        cyl                                       
      12          4        10 (90.91%)           1 (9.09%)
      13          6         4 (57.14%)          3 (42.86%)
      14          8             0 (0%)        14 (100.00%)
      15         am                                       
      16       auto         7 (36.84%)         12 (63.16%)
      17     manual         7 (53.85%)          6 (46.15%)

