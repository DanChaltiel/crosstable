# is there a problem?

    Code
      a = mtcars %>% filter(vs == 0) %>% pull(mpg)
      m = median(a)
      m
    Output
      [1] 15.65
    Code
      format_fixed(m)
    Output
      [1] "15.6"

