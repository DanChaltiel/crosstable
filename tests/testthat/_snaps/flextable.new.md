# flextable header

    Code
      ct %>% af() %>% {
        .$header$dataset
      }
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable           vs=straight             vs=straight
      2 label variable               am=auto               am=manual
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1           vs=vshaped             vs=vshaped           vs=NA             vs=NA
      2              am=auto              am=manual         am=auto         am=manual
    Code
      ct %>% af(remove_header_keys = T) %>% {
        .$header$dataset
      }
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable              straight                straight
      2 label variable                  auto                  manual
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1              vshaped                vshaped              NA                NA
      2                 auto                 manual            auto            manual
    Code
      ct %>% af(header_show_n = TRUE) %>% {
        .$header$dataset
      }
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable           vs=straight             vs=straight
      2 label variable         am=auto (n=2)         am=manual (n=7)
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1           vs=vshaped             vs=vshaped           vs=NA             vs=NA
      2        am=auto (n=9)        am=manual (n=6)   am=auto (n=8)   am=manual (n=0)
    Code
      ct %>% af(header_show_n = 1:2) %>% {
        .$header$dataset
      }
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable     vs=straight (n=9)       vs=straight (n=9)
      2 label variable         am=auto (n=2)         am=manual (n=7)
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1    vs=vshaped (n=15)      vs=vshaped (n=15)     vs=NA (n=8)       vs=NA (n=8)
      2        am=auto (n=9)        am=manual (n=6)   am=auto (n=8)   am=manual (n=0)

