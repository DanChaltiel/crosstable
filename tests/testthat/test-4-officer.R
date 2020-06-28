

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
mtcars3$surv = Surv(mtcars3$disp, mtcars3$am=="manual") %>% set_label("Dummy survival")



# crosstables don't throw errors in officer -------------------------------
crosstables = suppressWarnings(
    list(
        Simple = crosstable(esoph, test=TRUE),
        Double_effect = crosstable(mtcars, mpg,cyl,disp, by=am, effect=TRUE),
        Triple = crosstable(iris, by=Species, showNA="always", total="both")
    )
)
test_that("crosstables don't throw errors in officer", {
    doc = read_docx()
    for (i in names(crosstables)) {
        crosstable = crosstables[[i]]
        expect_is(crosstable, c("crosstable"))
        doc = doc %>% 
            body_add_title(i, 1) %>%
            body_add_glued("This dataset has {nrow(crosstable)} rows and {x} columns.", 
                           x=ncol(crosstable)) %>%
            body_add_title("Not compacted", 2) %>%
            body_add_crosstable(crosstable, show_test_name=FALSE) %>%
            body_add_table_legend(paste0(i, ", not compacted")) %>% 
            body_add_break %>%
            body_add_title("Compacted in function", 2) %>%
            body_add_crosstable(crosstable, compact=TRUE) %>% 
            body_add_table_legend(paste0(i, ", compacted inside function")) %>% 
            body_add_break %>% 
            body_add_normal("Look, there are labels!") %>%
            body_add_title("Compacted before function", 2) %>%
            body_add_crosstable(compact(crosstable), show_test_name=FALSE) %>%
            body_add_table_legend(paste0(i, ", compacted before function")) %>% 
            body_add_break 
    }
    if(!is_testing()){
        print(doc, "tests/testthat/docx/result_test_crosstable_officer.docx")
        
    }
})













