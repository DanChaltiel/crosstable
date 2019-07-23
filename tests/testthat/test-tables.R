

#il faudrait faire un test en installant le package, trop relou...
# test_that("legacy is still OK", {
#     mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
#     doc <- docx()
#     doc <- addCrossTable(doc, mytable)
#     doc <- addPageBreak(doc)
#     doc <- addCrossTable(doc, mytable, TRUE)
#     dfile <- "test_doc_old.docx"
#     writeDoc(doc, dfile)
#     shell.exec(dfile)
#     
#     expect_equal(2 * 2, 4)
# })

test_that("crosstables don't throw errors in officer", {
    mytable <- cross(cbind(...) ~ tobgp, esoph, test = TRUE)
    
    doc <- read_docx() %>% 
        body_add_crosstable(mytable) %>%
        body_add_break %>%
        body_add_crosstable(mytable, TRUE) %>% 
        body_add_break %>% 
        body_add_crosstable(compact(mytable))
    #just checking there is no error.
    #maybe dig into doc object to check other things...
    expect_equal(2 * 2, 4) 
})
