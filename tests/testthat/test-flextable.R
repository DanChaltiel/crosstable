
ct1 = crosstable(mtcars3, c(cyl, vs), by=c(am))
ct2 = crosstable(mtcars3, c(cyl, gear), by=c(am, vs))

test_that("flextable header", {
    expect_snapshot({
        ct2 %>% as_flextable() %>% {.$header$dataset}
        ct2 %>% as_flextable(remove_header_keys=T) %>% {.$header$dataset}
        ct2 %>% as_flextable(header_show_n=TRUE) %>% {.$header$dataset}
        ct2 %>% as_flextable(header_show_n=1:2) %>% {.$header$dataset}
    })
})

