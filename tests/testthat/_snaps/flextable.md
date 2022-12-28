# header default

    Code
      ct1 %>% as_flextable() %>% get_header_df()
    Output
        label variable         auto       manual
      1 label variable Transmission Transmission
      2 label variable         auto       manual
    Code
      ct2 %>% as_flextable() %>% get_header_df()
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable           vs=straight             vs=straight
      2 label variable               am=auto               am=manual
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1           vs=vshaped             vs=vshaped           vs=NA             vs=NA
      2              am=auto              am=manual         am=auto         am=manual
    Code
      ct3 %>% as_flextable() %>% get_header_df()
    Output
        label variable vs=straight & cyl3=FALSE & am=auto
      1 label variable                            am=auto
      2 label variable                         cyl3=FALSE
      3 label variable                        vs=straight
        vs=vshaped & cyl3=FALSE & am=auto vs=NA & cyl3=FALSE & am=auto
      1                           am=auto                      am=auto
      2                        cyl3=FALSE                   cyl3=FALSE
      3                        vs=vshaped                        vs=NA
        vs=straight & cyl3=NA & am=auto vs=vshaped & cyl3=NA & am=auto
      1                         am=auto                        am=auto
      2                         cyl3=NA                        cyl3=NA
      3                     vs=straight                     vs=vshaped
        vs=NA & cyl3=NA & am=auto vs=straight & cyl3=FALSE & am=manual
      1                   am=auto                            am=manual
      2                   cyl3=NA                           cyl3=FALSE
      3                     vs=NA                          vs=straight
        vs=vshaped & cyl3=FALSE & am=manual vs=NA & cyl3=FALSE & am=manual
      1                           am=manual                      am=manual
      2                          cyl3=FALSE                     cyl3=FALSE
      3                          vs=vshaped                          vs=NA
        vs=straight & cyl3=NA & am=manual vs=vshaped & cyl3=NA & am=manual
      1                         am=manual                        am=manual
      2                           cyl3=NA                          cyl3=NA
      3                       vs=straight                       vs=vshaped
        vs=NA & cyl3=NA & am=manual
      1                   am=manual
      2                     cyl3=NA
      3                       vs=NA

# header by_header (monoby)

    Code
      ct1 %>% as_flextable(by_header = NULL) %>% get_header_df()
    Output
        label variable         auto       manual
      1 label variable Transmission Transmission
      2 label variable         auto       manual
    Code
      ct1 %>% as_flextable(by_header = FALSE) %>% get_header_df()
    Output
        label variable auto manual
      1 label variable auto manual

# generic_labels

    Code
      crosstable(mtcars2, am, by = vs, total = "both", test = TRUE, effect = TRUE) %>%
        rename(ID = .id, math = variable, Tot = Total, lab = label, pval = test, fx = effect) %>%
        as_flextable(by_header = "Engine shape", generic_labels = list(id = "ID",
          variable = "math", total = "Tot", label = "lab", test = "pval", effect = "fx")) %>%
        get_header_df()
    Output
        lab math     straight      vshaped Tot fx pval
      1 lab math Engine shape Engine shape Tot fx pval
      2 lab math     straight      vshaped Tot fx pval

# get_show_n_pattern

    Code
      get_show_n_pattern()
    Output
      $cell
      [1] "{.col} (N={.n})"
      
      $total
      NULL
      
    Code
      get_show_n_pattern("a")
    Output
      $cell
      [1] "a"
      
      $total
      NULL
      
    Code
      get_show_n_pattern(list(cell = "a"))
    Output
      $cell
      [1] "a"
      
      $total
      NULL
      
    Code
      get_show_n_pattern(list(total = "b"))
    Output
      $cell
      [1] "{.col} (N={.n})"
      
      $total
      [1] "b"
      
    Code
      get_show_n_pattern(list(cell = "a", total = "b"))
    Output
      $cell
      [1] "a"
      
      $total
      [1] "b"
      

# header header_show_n+pattern

    Code
      ct2 %>% as_flextable(header_show_n = TRUE, header_show_n_pattern = "{.col_key}:\n{.col_val}\n(N={.n})",
        remove_header_keys = T) %>% get_header_df()
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable  vs:\nstraight\n(N=9)    vs:\nstraight\n(N=9)
      2 label variable      am:\nauto\n(N=2)      am:\nmanual\n(N=7)
        am=auto & vs=vshaped am=manual & vs=vshaped  am=auto & vs=NA
      1 vs:\nvshaped\n(N=15)   vs:\nvshaped\n(N=15)   vs:\nNA\n(N=8)
      2     am:\nauto\n(N=9)     am:\nmanual\n(N=6) am:\nauto\n(N=8)
         am=manual & vs=NA
      1     vs:\nNA\n(N=8)
      2 am:\nmanual\n(N=0)

# header header_show_n+remove_header_keys

    Code
      ct2 %>% as_flextable(header_show_n = TRUE, remove_header_keys = TRUE) %>%
        get_header_df()
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable        straight (N=9)          straight (N=9)
      2 label variable            auto (N=2)            manual (N=7)
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1       vshaped (N=15)         vshaped (N=15)        NA (N=8)          NA (N=8)
      2           auto (N=9)           manual (N=6)      auto (N=8)      manual (N=0)

# header remove_header_keys

    Code
      ct1 %>% as_flextable(remove_header_keys = T) %>% get_header_df()
    Output
        label variable         auto       manual
      1 label variable Transmission Transmission
      2 label variable         auto       manual
    Code
      ct3 %>% as_flextable(remove_header_keys = T) %>% get_header_df()
    Output
        label variable vs=straight & cyl3=FALSE & am=auto
      1 label variable                               auto
      2 label variable                              FALSE
      3 label variable                           straight
        vs=vshaped & cyl3=FALSE & am=auto vs=NA & cyl3=FALSE & am=auto
      1                              auto                         auto
      2                             FALSE                        FALSE
      3                           vshaped                           NA
        vs=straight & cyl3=NA & am=auto vs=vshaped & cyl3=NA & am=auto
      1                            auto                           auto
      2                              NA                             NA
      3                        straight                        vshaped
        vs=NA & cyl3=NA & am=auto vs=straight & cyl3=FALSE & am=manual
      1                      auto                               manual
      2                        NA                                FALSE
      3                        NA                             straight
        vs=vshaped & cyl3=FALSE & am=manual vs=NA & cyl3=FALSE & am=manual
      1                              manual                         manual
      2                               FALSE                          FALSE
      3                             vshaped                             NA
        vs=straight & cyl3=NA & am=manual vs=vshaped & cyl3=NA & am=manual
      1                            manual                           manual
      2                                NA                               NA
      3                          straight                          vshaped
        vs=NA & cyl3=NA & am=manual
      1                      manual
      2                          NA
      3                          NA

# header header_show_n

    Code
      ct1 %>% as_flextable(header_show_n = TRUE) %>% get_header_df()
    Output
        label variable         auto        manual
      1 label variable Transmission  Transmission
      2 label variable  auto (N=19) manual (N=13)
    Code
      ct3 %>% as_flextable(header_show_n = TRUE) %>% get_header_df()
    Output
        label variable vs=straight & cyl3=FALSE & am=auto
      1 label variable                     am=auto (N=19)
      2 label variable                  cyl3=FALSE (N=17)
      3 label variable                  vs=straight (N=1)
        vs=vshaped & cyl3=FALSE & am=auto vs=NA & cyl3=FALSE & am=auto
      1                    am=auto (N=19)               am=auto (N=19)
      2                 cyl3=FALSE (N=17)            cyl3=FALSE (N=17)
      3                  vs=vshaped (N=9)                  vs=NA (N=7)
        vs=straight & cyl3=NA & am=auto vs=vshaped & cyl3=NA & am=auto
      1                  am=auto (N=19)                 am=auto (N=19)
      2                   cyl3=NA (N=2)                  cyl3=NA (N=2)
      3               vs=straight (N=1)               vs=vshaped (N=0)
        vs=NA & cyl3=NA & am=auto vs=straight & cyl3=FALSE & am=manual
      1            am=auto (N=19)                     am=manual (N=13)
      2             cyl3=NA (N=2)                    cyl3=FALSE (N=10)
      3               vs=NA (N=1)                    vs=straight (N=6)
        vs=vshaped & cyl3=FALSE & am=manual vs=NA & cyl3=FALSE & am=manual
      1                    am=manual (N=13)               am=manual (N=13)
      2                   cyl3=FALSE (N=10)              cyl3=FALSE (N=10)
      3                    vs=vshaped (N=4)                    vs=NA (N=0)
        vs=straight & cyl3=NA & am=manual vs=vshaped & cyl3=NA & am=manual
      1                  am=manual (N=13)                 am=manual (N=13)
      2                     cyl3=NA (N=3)                    cyl3=NA (N=3)
      3                 vs=straight (N=1)                 vs=vshaped (N=2)
        vs=NA & cyl3=NA & am=manual
      1            am=manual (N=13)
      2               cyl3=NA (N=3)
      3                 vs=NA (N=0)

