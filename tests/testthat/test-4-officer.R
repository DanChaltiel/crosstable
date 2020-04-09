

# Init --------------------------------------------------------------------

Sys.setenv(LANG = "en")
options(warn = 1)
options(stringsAsFactors = FALSE)
options(tidyselect_verbosity = "verbose")

library(officer)
library(survival)
mtcars3 = mtcars2
mtcars3$cyl[1:5] = NA
mtcars3$vs[5:12] = NA
mtcars3$cyl3 = mtcars3$cyl==3
mtcars3$cyl6 = mtcars3$cyl==6
mtcars3$surv = Surv(mtcars3$disp, mtcars3$am=="manual")
Hmisc::label(mtcars3$surv) = "Dummy survival"



# crosstables don't throw errors in officer -------------------------------
crosstables = suppressWarnings(
    list(
        simple = crosstable(esoph, test=TRUE),
        # double_test = crosstable(mtcars, mpg,cyl,disp, by=am, test=TRUE),
        # triple_test = crosstable(iris, by=Species, test=TRUE),
        # double_effect = crosstable(mtcars, mpg,cyl,disp, by=am, effect=TRUE),
        triple_effect = crosstable(iris, by=Species, effect=TRUE)
    )
)
test_that("crosstables don't throw errors in officer", {
    doc = read_docx()
    for (i in names(crosstables)) {
        crosstable = crosstables[[i]]
        expect_is(crosstable, c("crosstable"))
        doc = doc %>% 
            body_add_title(i, 1) %>%
            body_add_title("Not compacted", 2) %>%
            body_add_crosstable(crosstable, show.test.name=FALSE) %>%
            body_add_break %>%
            body_add_title("Compacted in function", 2) %>%
            body_add_crosstable(crosstable, compact=TRUE) %>% 
            body_add_break %>% 
            body_add_title("Compacted before function", 2) %>%
            body_add_crosstable(compact(crosstable), show.test.name=FALSE) %>%
            body_add_break 
    }
    if(!is_testing())
        print(doc, "../../examples/result_test_crosstable_officer.docx")
})













