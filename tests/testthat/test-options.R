

t <- data.frame(x=c('a', 'a', 'b'))
ct <- crosstable(t, cols=c('x'))
gt <- as_gt(ct, generic_labels=list(value="count"))



test_that("Options work", {
    local({
        crosstable_reset_options(verbose=FALSE)
        x=crosstable_peek_options()
        expect_length(x, 0)
        
        #normal func
        crosstable_options(zero_percent=TRUE)
        #legacy
        crosstable_options(crosstable_wrap_id = TRUE)
        #duplicates
        expect_warning(crosstable_options(units="cm", crosstable_units="error", 
                                          percent_pattern="{n}", crosstable_percent_pattern="error"), 
                       class="crosstable_dupl_option_warning")
        #unknown options
        expect_warning(crosstable_options(foo="bar", bar="foo"), 
                       class="crosstable_unknown_option_warning")
        #reset deprecated
        expect_warning(crosstable_options(reset=TRUE), 
                       class="lifecycle_warning_deprecated")
        
        x=crosstable_peek_options()
        expect_identical(x, list(crosstable_zero_percent = TRUE, crosstable_wrap_id = TRUE, 
                                 crosstable_units = "cm", crosstable_percent_pattern = "{n}"))
        
        #default in testing env
        crosstable_options(crosstable_verbosity_autotesting="quiet")
    })
})
