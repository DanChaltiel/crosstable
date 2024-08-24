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
               label     variable am=auto & vs=straight am=manual & vs=straight
      1       Engine       Engine              straight                straight
      2 Transmission Transmission                  auto                  manual
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1              vshaped                vshaped              NA                NA
      2                 auto                 manual            auto            manual
    Code
      ct3 %>% as_flextable() %>% get_header_df()
    Output
               label     variable vs=straight & cyl3=FALSE & am=auto
      1 Transmission Transmission                               auto
      2         cyl3         cyl3                              FALSE
      3       Engine       Engine                           straight
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
        remove_header_keys = FALSE) %>% get_header_df()
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
      ct2 %>% as_flextable(header_show_n = TRUE, remove_header_keys = FALSE) %>%
        get_header_df()
    Output
        label variable am=auto & vs=straight am=manual & vs=straight
      1 label variable     vs=straight (N=9)       vs=straight (N=9)
      2 label variable         am=auto (N=2)         am=manual (N=7)
        am=auto & vs=vshaped am=manual & vs=vshaped am=auto & vs=NA am=manual & vs=NA
      1    vs=vshaped (N=15)      vs=vshaped (N=15)     vs=NA (N=8)       vs=NA (N=8)
      2        am=auto (N=9)        am=manual (N=6)   am=auto (N=8)   am=manual (N=0)

# header remove_header_keys

    Code
      ct1 %>% as_flextable(remove_header_keys = FALSE) %>% get_header_df()
    Output
        label variable         auto       manual
      1 label variable Transmission Transmission
      2 label variable         auto       manual
    Code
      ct3 %>% as_flextable(remove_header_keys = FALSE) %>% get_header_df()
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

# header header_show_n

    Code
      ct0 %>% af(header_show_n = TRUE) %>% get_header_df()
    Output
        label variable value
      1 label variable value
    Code
      ct0 %>% af(header_show_n = TRUE, by_header = "foobar") %>% get_header_df()
    Output
        label variable value
      1 label variable value
    Code
      ct1 %>% af(header_show_n = TRUE) %>% get_header_df()
    Output
        label variable         auto        manual
      1 label variable Transmission  Transmission
      2 label variable  auto (N=19) manual (N=13)
    Code
      ct1 %>% af(by_header = "foobar", header_show_n = TRUE) %>% get_header_df()
    Output
        label variable        auto        manual
      1 label variable      foobar        foobar
      2 label variable auto (N=19) manual (N=13)
    Code
      ct1 %>% af(by_header = FALSE, header_show_n = TRUE) %>% get_header_df()
    Output
        label variable        auto        manual
      1 label variable auto (N=19) manual (N=13)
    Code
      ct3 %>% af(header_show_n = TRUE) %>% get_header_df()
    Output
               label     variable vs=straight & cyl3=FALSE & am=auto
      1 Transmission Transmission                        auto (N=19)
      2         cyl3         cyl3                       FALSE (N=17)
      3       Engine       Engine                     straight (N=1)
        vs=vshaped & cyl3=FALSE & am=auto vs=NA & cyl3=FALSE & am=auto
      1                       auto (N=19)                  auto (N=19)
      2                      FALSE (N=17)                 FALSE (N=17)
      3                     vshaped (N=9)                     NA (N=7)
        vs=straight & cyl3=NA & am=auto vs=vshaped & cyl3=NA & am=auto
      1                     auto (N=19)                    auto (N=19)
      2                        NA (N=2)                       NA (N=2)
      3                  straight (N=1)                  vshaped (N=0)
        vs=NA & cyl3=NA & am=auto vs=straight & cyl3=FALSE & am=manual
      1               auto (N=19)                        manual (N=13)
      2                  NA (N=2)                         FALSE (N=10)
      3                  NA (N=1)                       straight (N=6)
        vs=vshaped & cyl3=FALSE & am=manual vs=NA & cyl3=FALSE & am=manual
      1                       manual (N=13)                  manual (N=13)
      2                        FALSE (N=10)                   FALSE (N=10)
      3                       vshaped (N=4)                       NA (N=0)
        vs=straight & cyl3=NA & am=manual vs=vshaped & cyl3=NA & am=manual
      1                     manual (N=13)                    manual (N=13)
      2                          NA (N=3)                         NA (N=3)
      3                    straight (N=1)                    vshaped (N=2)
        vs=NA & cyl3=NA & am=manual
      1               manual (N=13)
      2                    NA (N=3)
      3                    NA (N=0)

