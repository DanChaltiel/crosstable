# Labelling dataframes

    Code
      get_label(xx)
    Output
                          model                       mpg                       cyl 
                        "Model"       "Miles/(US) gallon"                     "cyl" 
                           disp                        hp                      drat 
        "Displacement (cu.in.)"        "Gross horsepower"         "Rear axle ratio" 
                             wt                      qsec                        vs 
            "Weight (1000 lbs)"           "1/4 mile time"                  "Engine" 
                             am                      gear                      carb 
                 "Transmission" "Number of forward gears"   "Number of carburetors" 
                        hp_date                qsec_posix 
           "Some nonsense date"               "Date+time" 

# Labelling nested lists (get)

    Code
      get_label(x)
    Output
        l1   l2   l3 
      "l1" "l2" "l3" 

---

    Code
      get_label(x, recursive = TRUE)
    Output
                            l11                       l12                       l13 
                             NA                        NA                        NA 
           l2.iris.Sepal.Length       l2.iris.Sepal.Width      l2.iris.Petal.Length 
              "Length of Sepal"          "Width of Sepal"         "Length of Petal" 
            l2.iris.Petal.Width           l2.iris.Species                  l2.model 
               "Width of Petal"                  "Specie"                   "Model" 
                         l2.mpg                    l2.cyl                   l2.disp 
            "Miles/(US) gallon"     "Number of cylinders"   "Displacement (cu.in.)" 
                          l2.hp                   l2.drat                     l2.wt 
             "Gross horsepower"         "Rear axle ratio"       "Weight (1000 lbs)" 
                        l2.qsec                     l2.vs                     l2.am 
                "1/4 mile time"                  "Engine"            "Transmission" 
                        l2.gear                   l2.carb                l2.hp_date 
      "Number of forward gears"   "Number of carburetors"      "Some nonsense date" 
                  l2.qsec_posix                    l3.foo                   l3.ffoo 
                    "Date+time"                     "foo"                    "ffoo" 

---

    Code
      get_label(x, recursive = TRUE, simplify = FALSE)
    Output
      $l1
      [1] NA NA NA
      
      $l2
              iris.Sepal.Length          iris.Sepal.Width         iris.Petal.Length 
              "Length of Sepal"          "Width of Sepal"         "Length of Petal" 
               iris.Petal.Width              iris.Species                     model 
               "Width of Petal"                  "Specie"                   "Model" 
                            mpg                       cyl                      disp 
            "Miles/(US) gallon"     "Number of cylinders"   "Displacement (cu.in.)" 
                             hp                      drat                        wt 
             "Gross horsepower"         "Rear axle ratio"       "Weight (1000 lbs)" 
                           qsec                        vs                        am 
                "1/4 mile time"                  "Engine"            "Transmission" 
                           gear                      carb                   hp_date 
      "Number of forward gears"   "Number of carburetors"      "Some nonsense date" 
                     qsec_posix 
                    "Date+time" 
      
      $l3
         foo   ffoo 
       "foo" "ffoo" 
      

# clean_names_with_labels

    Code
      x = data.frame(`name with space` = 1, TwoWords = 1, `total $ (2009)` = 1,
        àccénts = 1, check.names = FALSE)
      cleaned = clean_names_with_labels(x)
      names(cleaned)
    Output
      [1] "name_with_space" "twowords"        "total_2009"      "accents"        
    Code
      get_label(cleaned)
    Output
        name_with_space          twowords        total_2009           accents 
      "name with space"        "TwoWords"  "total $ (2009)"         "àccénts" 
    Code
      cleaned = clean_names_with_labels(x, except = "name with space")
      names(cleaned)
    Output
      [1] "name with space" "twowords"        "total_2009"      "accents"        
    Code
      get_label(cleaned)
    Output
        name with space          twowords        total_2009           accents 
      "name with space"        "TwoWords"  "total $ (2009)"         "àccénts" 

# rename_dataframe_with_labels 

    Code
      rename_with_labels(dat) %>% names()
    Output
      [1] "Model"                 "Miles/(US) gallon"     "Number of cylinders"  
      [4] "Displacement (cu.in.)" "Gross horsepower"     
    Code
      rename_with_labels(dat, except = 5) %>% names()
    Output
      [1] "Model"                 "Miles/(US) gallon"     "Number of cylinders"  
      [4] "Displacement (cu.in.)" "hp"                   
    Code
      rename_with_labels(dat, cols = 1:4) %>% names()
    Output
      [1] "Model"                 "Miles/(US) gallon"     "Number of cylinders"  
      [4] "Displacement (cu.in.)" "hp"                   
    Code
      rename_with_labels(dat, contains("p"), except = "hp") %>% names()
    Output
      [1] "model"                 "Miles/(US) gallon"     "cyl"                  
      [4] "Displacement (cu.in.)" "hp"                   
    Code
      rename_with_labels(dat, everything(), except = starts_with("h")) %>% names()
    Output
      [1] "Model"                 "Miles/(US) gallon"     "Number of cylinders"  
      [4] "Displacement (cu.in.)" "hp"                   
    Code
      rename_with_labels(dat, except = 99) %>% names()
    Output
      [1] "Model"                 "Miles/(US) gallon"     "Number of cylinders"  
      [4] "Displacement (cu.in.)" "Gross horsepower"     

