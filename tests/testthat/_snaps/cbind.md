# cbind() works

    Code
      x = ct_bind_cols(ct1, ct2)
      attributes(x)
    Output
      $names
      [1] ".id"                     "label"                  
      [3] "variable"                "am=auto & vs=straight"  
      [5] "am=manual & vs=straight" "am=auto & vs=vshaped"   
      [7] "am=manual & vs=vshaped"  "am=auto & vs=Total"     
      [9] "am=manual & vs=Total"   
      
      $row.names
      [1] 1 2 3 4 5 6 7
      
      $class
      [1] "crosstable_multiby" "crosstable"         "tbl_df"            
      [4] "tbl"                "data.frame"        
      
      $debug
      $debug$interface
      [1] "quosure"
      
      $debug$x_class
           cyl 
      "factor" 
      
      $debug$y_class
               am          vs 
      "character" "character" 
      
      
      $N
      [1] 32
      
      $showNA
      [1] "ifany"
      
      $variables
      [1] "cyl"
      
      $has_test
      [1] FALSE
      
      $has_effect
      [1] FALSE
      
      $has_total
      [1] FALSE
      
      $has_label
      [1] TRUE
      
      $by
      [1] "am" "vs"
      
      $by_label
                  am             vs 
      "Transmission"       "Engine" 
      
      $by_table
        am=auto & vs=straight am=manual & vs=straight    am=auto & vs=vshaped 
                            7                       7                      12 
       am=manual & vs=vshaped      am=auto & vs=Total    am=manual & vs=Total 
                            6                      19                      13 
      
      $by_levels
      $by_levels$am
      [1] "auto"   "manual"
      
      $by_levels$vs
      [1] "straight" "vshaped" 
      
      
    Code
      y = cbind(ct1, ct2)
      identical(x, y)
    Output
      [1] TRUE

