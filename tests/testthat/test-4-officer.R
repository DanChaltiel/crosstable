

# Init --------------------------------------------------------------------

Sys.setenv(LANG = "en")
options(warn = 1)
options(stringsAsFactors = FALSE)
# options(tidyselect_verbosity = "verbose")

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
        Double_effect = crosstable(mtcars, c(mpg, cyl, disp), by=am, effect=TRUE),
        Triple = crosstable(iris, by=Species, showNA="always", total="both")
    )
)
test_that("crosstables don't throw errors in officer", {
    doc = read_docx()
    for (i in names(crosstables)) {
        ct = crosstables[[i]]
        expect_is(ct, c("crosstable"))
        doc = doc %>% 
            body_add_title(i, 1) %>%
            body_add_normal("This dataset has {nrow(ct)} rows and {x} columns.", 
                            x=ncol(ct)) %>%
            body_add_title("Not compacted", 2) %>%
            body_add_crosstable(ct, show_test_name=FALSE) %>%
            body_add_table_legend(paste0(i, ", not compacted")) %>% 
            body_add_break() %>%
            body_add_title("Compacted in function", 2) %>%
            body_add_crosstable(ct, compact=TRUE) %>% 
            body_add_table_legend(paste0(i, ", compacted inside function")) %>% 
            body_add_break() %>% 
            body_add_normal("Look, there are labels!") %>%
            body_add_title("Compacted before function", 2) %>%
            body_add_crosstable(compact(ct), show_test_name=FALSE) %>%
            body_add_table_legend(paste0(i, ", compacted before function")) %>% 
            body_add_break()
    }
    if(!is_testing()){
        print(doc, "tests/testthat/docx/result_test_crosstable_officer.docx")
    }
})

test_that("crosstables helpers", {
    withr::local_options(crosstable_style_list_ordered="toc 1", 
                         crosstable_style_list_unordered="toc 2",
                         crosstable_style_image="centered",
                         crosstable_units="cm")
    
    img.file = file.path( R.home("doc"), "html", "logo.jpg" )
    p = ggplot2::ggplot(data = iris ) +
        ggplot2::geom_point(mapping =  ggplot2::aes(Sepal.Length, Petal.Length))
    doc = read_docx() %>% 
        body_add_normal("Iris has {nrow(iris)} rows and {x} columns.", 
                        x=ncol(iris)) %>%
        body_add_normal("I can write multiple {x}.", "Just like {y}.", 
                        x="paragraphs", y="this") %>%
        body_add_list(c("Numbered item 1", "Numbered item 2"), ordered = TRUE) %>%
        body_add_list_item("Numbered item 3", ordered = TRUE) %>%
        body_add_list(c("Bullet item 1", "Bullet item 2"), ordered = FALSE) %>%
        body_add_list_item("Bullet item 3", ordered = FALSE) %>%
        body_add_img2(img.file, h=7.6, w=10, style="Normal") %>% 
        body_add_img2(img.file, h=76, w=100, units="mm") %>% 
        body_add_gg2(p, w=14, h=10, scale=1.5, style="Normal") %>% 
        body_add_gg2(p, w=140, h=100, scale=1.5, units="mm") %>% 
        body_add_crosstable_footnote() %>% 
        body_add_break()
    if(!is_testing()){
        print(doc, "tests/testthat/docx/result_test_crosstable_officer2.docx")
    }
    expect_equal(TRUE, TRUE)
})




test_that("Officers warnings and errors", {
    pars1 = c("Paragraphe 1.1", "Paragraphe 1.2")
    pars2 = c("Paragraphe 2.1", "Paragraphe 2.2")
    expect_error(body_add_normal(read_docx(), pars1, pars2), 
                 "body_add_normal\\(\\) only accepts either one vector of any length or several vectors of length 1")
    
    lifecycle::expect_deprecated(body_add_glued(read_docx(), "Paragraphe"))
})




test_that("Utils functions are OK too", {
    withr::local_options(crosstable_units="cm")
    
    info_rows = c("Also, table iris has {nrow(iris)} rows.", "And table mtcars has {nrow(mtcars)} rows.")
    img.file = file.path( R.home("doc"), "html", "logo.jpg" )
    p = ggplot2::ggplot(data = iris ) +
        ggplot2::geom_point(mapping = ggplot2::aes(Sepal.Length, Petal.Length))
    
    doc = read_docx() %>% 
        body_add_title("Tests", 1)  %>%
        body_add_normal("Table iris has", ncol(iris), "columns.", .sep=" ") %>% 
        body_add_normal("However, table mtcars has {ncol(mtcars)} columns") %>% 
        body_add_normal(info_rows) %>% 
        body_add_normal("As you can see in Table \\@ref(tab1) and in Figure \\@ref(fig1), ",
                        "the iris dataset is about flowers.") %>%
        body_add_table_legend("This is a crosstable", bookmark="tab1") %>%
        body_add_crosstable(crosstables[[2]], show_test_name=FALSE, 
                            body_fontsize = 8, header_fontsize = 10) %>%
        body_add_break() %>% 
        body_add_img2(img.file, h=7.6, w=10, style="centered") %>%
        body_add_img2(img.file, h=7.6/2.5, w=10/2.5, units="in") %>%
        body_add_figure_legend("Twice the R logo", bookmark="fig1") %>% 
        body_add_gg2(p, w=14, h=10, scale=1.5) %>%
        body_add_gg2(p, w=14/2.5, h=10/2.5, scale=1.5, units="in") %>%
        identity()
    expect_true(TRUE)
})






# openxlsx ----------------------------------------------------------------


test_that("openxlsx is working", {
    x=crosstable(mtcars2, c(mpg, vs, gear), total=T, test=T)
    wb1=as_workbook(x, keep_id=FALSE)
    wb2=as_workbook(x, keep_id=TRUE)
    expect_true(TRUE)
    
    x=crosstable(mtcars2, c(mpg, vs, gear), by=cyl, total=T, test=T)
    wb3=as_workbook(x, keep_id=FALSE)
    wb4=as_workbook(x, keep_id=TRUE)
    
    if(!is_testing()){
        openxlsx::saveWorkbook(wb1, file = "tests/testthat/xlsx/test_openxlsx1.xlsx", overwrite = TRUE)
        openxlsx::saveWorkbook(wb2, file = "tests/testthat/xlsx/test_openxlsx2.xlsx", overwrite = TRUE)
        openxlsx::saveWorkbook(wb3, file = "tests/testthat/xlsx/test_openxlsx3.xlsx", overwrite = TRUE)
        openxlsx::saveWorkbook(wb4, file = "tests/testthat/xlsx/test_openxlsx4.xlsx", overwrite = TRUE)
    }
})






