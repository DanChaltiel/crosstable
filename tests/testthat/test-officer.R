
crosstables = list(
    simple_test = cross(cbind(...) ~ ., esoph, test = TRUE),
    double_test = cross(cbind(mpg,cyl,disp) ~ factor(am), mtcars, test = TRUE),
    triple_test = cross(cbind(...) ~ Species, iris, test = TRUE),
    simple_no_test = cross(cbind(...) ~ ., esoph, test = F),
    double_no_test = cross(cbind(mpg,cyl,disp) ~ factor(am), mtcars, test = F),
    triple_no_test = cross(cbind(...) ~ Species, iris, test = F)
    #des warnings sur wilcoxon mais OSEF
)

#TODO body_add_crosstable when compacted before function

test_that("crosstables don't throw errors in officer", {
    doc <- read_docx()
    #test with compact=F et compact=T
    for (i in names(crosstables)) {
        print(i)
        crosstable = crosstables[[i]]
        doc = doc %>% 
            body_add_title(i, 1) %>%
            body_add_title("Not compacted", 2) %>%
            body_add_crosstable(crosstable, show.test.name = F, auto.fit = T) %>%
            body_add_break %>%
            body_add_title("Compacted in function", 2) %>%
            body_add_crosstable(crosstable, TRUE, show.test.name = F, auto.fit = T) %>% 
            body_add_break %>% 
            body_add_title("Compacted before function", 2) %>%
            body_add_normal("TODO") %>% 
            # body_add_crosstable(compact(crosstable), show.test.name = F, auto.fit = T) %>% 
            body_add_break 
    }
    print(doc, "test_cross_officer.docx")
})
