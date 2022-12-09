
# Init --------------------------------------------------------------------

compare_snapshot_doc = function(name){
  doc1 = read_docx(paste0('tests/testthat/docx/4-officer/snap_',name,'.docx'))
  doc2 = read_docx(paste0('tests/testthat/docx/4-officer/snap_',name,'_new.docx'))

  x1 = doc1$doc_obj$get() %>% xml2::as_list() %>% jsonlite::serializeJSON(pretty = TRUE)
  x2 = doc2$doc_obj$get() %>% xml2::as_list() %>% jsonlite::serializeJSON(pretty = TRUE)

  waldo::compare(x1,x2)
}

expect_snapshot_doc = function(doc){
  skip_on_os(c("mac", "linux", "solaris"))
  doc_xml = xml2::as_list(doc$doc_obj$get())

  sp = testthat:::get_snapshotter()
  if(is.null(sp)) cli_abort("Can't compare snapshot to reference when testing interactively")
  test_file = sp$file
  test_name = sp$test %>% tolower() %>%
    str_replace_all("\\W+", "_") %>%
    stringi::stri_trans_general(id = "Latin-ASCII")

  x = expect_snapshot_value(digest::digest(doc_xml))

  folder=paste0("docx/", test_file)
  filename=paste0(folder, "/snap_", test_name, ".docx")
  if(!file.exists(filename)){
    dir.create(folder, recursive=TRUE, showWarnings=FALSE)
    print(doc, filename)
  }

  filename_new=paste0(folder, "/snap_", test_name, "_new.docx")
  if(!inherits(x, "expectation_success")){
    print(doc, filename_new)
    #TODO cli
    cli_warn(glue("Word document snapshot has changed. Explore changes by running:\n",
                  "   compare_snapshot_doc('{test_name}')\n",
                  "   browseURL('tests/testthat/{filename}')\n",
                  "   browseURL('tests/testthat/{filename_new}')\n\n"))
  } else {
    if(file.exists(filename_new)) file.remove(filename_new)
    print(doc, filename)
  }
}


# crosstables don't throw errors in officer -------------------------------
crosstables = suppressWarnings({
  set.seed(12345)
  list(
    Simple = crosstable(esoph, test=TRUE),
    Double_effect = crosstable(mtcars, c(mpg, cyl, disp), by=am, effect=TRUE),
    Triple = crosstable(iris, by=Species, showNA="always", total="both")
  )
})


test_that("crosstables: Simple", {
  # skip_on_os(c("mac", "linux", "solaris"))
  i="Simple"
  ct = crosstables[[i]]
  expect_s3_class(ct, c("crosstable"))
  doc = read_docx() %>%
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
    body_add_crosstable(ct_compact(ct), show_test_name=FALSE) %>%
    body_add_table_legend(paste0(i, ", compacted before function")) %>%
    body_add_break()

  # expect_snapshot_doc(doc)
  expect_true(TRUE)
})
test_that("crosstables: Double with effects", {
  # skip_on_os(c("mac", "linux", "solaris"))
  i="Double_effect"
  ct = crosstables[[i]]
  expect_s3_class(ct, c("crosstable"))
  doc = read_docx() %>%
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
    body_add_crosstable(ct_compact(ct), show_test_name=FALSE) %>%
    body_add_table_legend(paste0(i, ", compacted before function")) %>%
    body_add_break()

  # expect_snapshot_doc(doc)
  expect_true(TRUE)
})
test_that("crosstables: Triple", {
  # skip_on_os(c("mac", "linux", "solaris"))
  i="Triple"
  ct = crosstables[[i]]
  expect_s3_class(ct, c("crosstable"))
  doc = read_docx() %>%
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
    body_add_crosstable(ct_compact(ct), show_test_name=FALSE) %>%
    body_add_table_legend(paste0(i, ", compacted before function")) %>%
    body_add_break()

  # expect_snapshot_doc(doc)
  expect_true(TRUE)
})



# Helpers -----------------------------------------------------------------


test_that("crosstables helpers", {
  # skip_on_os(c("mac", "linux", "solaris"))
  skip("Run `test-officer > crosstables helpers` manually one in a while!")
  rlang::local_options(crosstable_style_list_ordered="toc 1",
                       crosstable_style_list_unordered="toc 2",
                       crosstable_style_image="centered",
                       crosstable_units="cm")

  img.file = file.path( R.home("doc"), "html", "logo.jpg" )
  p = ggplot2::ggplot(data = iris ) +
    ggplot2::geom_point(mapping =  ggplot2::aes(Sepal.Length, Petal.Length))
  doc = read_docx() %>%
    body_add_normal("Iris has {nrow(iris)} rows and {x} columns.",
                    x=ncol(iris)) %>%
    body_add_normal("I can write multiple {x}. ", "Just like {y}.",
                    x="paragraphs", y="this") %>%
    body_add_normal("You can format in **bold**, *italic*, _underlined_, and `code`, and reference the figure \\@ref(fig1).") %>%
    body_add_normal("You can ignore refs: formats (**bold**, *italic*, _underlined_), code (`mean(x)+5`), and  refs (figure \\@ref(fig1))", parse=c("format", "code")) %>%
    body_add_normal("You can ignore formats: formats (**bold**, *italic*, _underlined_), code (`mean(x)+5`), and  refs (figure \\@ref(fig1))", parse=c("ref", "code")) %>%
    body_add_normal("You can ignore code: formats (**bold**, *italic*, _underlined_), code (`mean(x)+5`), and  refs (figure \\@ref(fig1))", parse=c("ref", "format")) %>%
    body_add_normal("You can ignore all: formats (**bold**, *italic*, _underlined_), code (`mean(x)+5`), and  refs (figure \\@ref(fig1))", parse=NULL) %>%
    body_add_list(c("Numbered item 1", "Numbered item 2"), ordered = TRUE) %>%
    body_add_list_item("Numbered item 3", ordered = TRUE) %>%
    body_add_list(c("Bullet item 1", "Bullet item 2"), ordered = FALSE) %>%
    body_add_list_item("Bullet item 3", ordered = FALSE) %>%
    body_add_img2(img.file, h=3, w=5, style="Normal") %>%
    body_add_img2(img.file, h=7, w=10, units="mm") %>%
    body_add_figure_legend("legend", bookmark="fig1") %>%
    body_add_gg2(p, w=14, h=10, scale=1.5, style="Normal") %>%
    body_add_gg2(p, w=14, h=10, scale=1.5, units="mm") %>%
    body_add_crosstable_footnote() %>%
    body_add_break()
  write_and_open(doc)
  # expect_snapshot_doc(doc)
  expect_true(TRUE)
})


test_that("Utils functions", {
  skip_on_os(c("mac", "linux", "solaris"))
  rlang::local_options(crosstable_units="cm")

  info_rows = c("Also, table iris has {nrow(iris)} rows.", "And table mtcars has {nrow(mtcars)} rows.")
  img.file = file.path( R.home("doc"), "html", "logo.jpg" )
  p = ggplot2::ggplot(data = iris ) +
    ggplot2::geom_point(mapping = ggplot2::aes(Sepal.Length, Petal.Length))

  doc = read_docx() %>%
    body_add_title("Tests", 1)  %>%
    body_add_normal("Table iris has", ncol(iris), "columns.", .sep=" ") %>%
    body_add_normal("However, table mtcars has {ncol(mtcars)} columns") %>%
    body_add_normal(info_rows) %>%
    body_add_crosstable(crosstables[[2]], show_test_name=FALSE,
                        body_fontsize = 8, header_fontsize = 10) %>%
    body_add_break() %>%
    body_add_img2(img.file, h=7.6, w=10, style="centered") %>%
    body_add_img2(img.file, h=7.6/2.5, w=10/2.5, units="in") %>%
    body_add_gg2(p, w=14, h=10, scale=1.5) %>%
    body_add_gg2(p, w=14/2.5, h=10/2.5, scale=1.5, units="in") %>%
    identity()

  # expect_snapshot_doc(doc)
  expect_true(TRUE)
})


test_that("Legend fields", {
  # skip_on_os(c("mac", "linux", "solaris"))
  #cannot use snapshot as fields are identified with uuid
  fp = fp_text_lite(bold=FALSE, italic=FALSE, underlined=TRUE, font.size=15)
  fp2 = fp_text_lite(font.size=9)
  doc = read_docx() %>%
    body_add_normal("As you can see in Table \\@ref(tab1) and in Figure \\@ref(fig1), ",
                    "the iris dataset is about flowers.") %>%
    body_add_table_legend("This is a crosstable", bookmark="tab1") %>%
    body_add_table_legend("This is a crosstable with bold legend",
                          name_format=fp, bookmark="tab2") %>%
    body_add_table_legend("This is a crosstable with bold legend",
                          name_format=fp2, legend_style="Normal",
                          bookmark="tab2") %>%
    body_add_figure_legend("This is a figure", bookmark="fig1") %>%
    identity()
  # write_and_open(doc)
  expect_true(TRUE)
})


# Warnings and errors -----------------------------------------------------


test_that("Officers warnings and errors", {
  # skip_on_os(c("mac", "linux", "solaris"))
  pars1 = c("Paragraphe 1.1", "Paragraphe 1.2")
  pars2 = c("Paragraphe 2.1", "Paragraphe 2.2")
  expect_error(body_add_normal(read_docx(), pars1, pars2),
               class="crosstable_officer_wrong_vector_error")

  expect_snapshot_error(body_add_table_legend(read_docx(), "xxx", foo=1, fun=mean, 5)) #rlib_error_dots_nonempty

  lifecycle::expect_deprecated(body_add_glued(read_docx(), "Paragraphe"))

  ct = crosstable(mtcars3, vs, by=model)
  expect_error(body_add_crosstable(read_docx(), ct),
               class="crosstable_body_add_large_error")

  ll = list("a"=crosstable(iris), crosstable(mtcars))
  expect_error(body_add_table_list(read_docx(), ll),
               class="body_add_table_list_named")
  ll = list("a"=crosstable(iris), "b"=5, "c"=lm(am~vs, data=mtcars))
  expect_error(body_add_table_list(read_docx(), ll),
               class="body_add_table_list_class")
  ll = list(iris=crosstable(iris), mtcars=crosstable(mtcars))
  expect_error(body_add_table_list(read_docx(), ll, fun="foobar"),
               class="body_add_table_list_fun_name")
  expect_error(body_add_table_list(read_docx(), ll, fun=function(x, y) x),
               class="body_add_table_list_fun_args")
  expect_error(body_add_table_list(read_docx(), ll,
                                   fun=function(doc, .name) .name),
               class="body_add_table_list_return")
  expect_error(body_add_table_list(read_docx(), ll,
                                   fun_after=function(doc, .name) .name),
               class="body_add_table_list_return2")
})


# Other reporting functions -----------------------------------------------


## openxlsx workbooks ------------------------------------------------------



test_that("openxlsx is working", {
  set.seed(1234)

  #by=NULL
  x1=crosstable(mtcars2, c(mpg, vs, gear), total=T, test=T)
  wb1=as_workbook(x1, keep_id=FALSE)
  wb2=as_workbook(x1, keep_id=TRUE)
  expect_true(TRUE)

  #by=cyl
  x2=crosstable(mtcars2, c(mpg, vs, gear), by=cyl, total=T, test=T)
  wb3=as_workbook(x2, keep_id=FALSE)
  wb4=as_workbook(x2, keep_id=TRUE)

  #by=c(cyl, am)
  x3=crosstable(mtcars2, c(mpg, vs, gear), by=c(cyl, am), total=T)
  wb5=as_workbook(x3, keep_id=FALSE)

  xl=list("with by"=x2, noby=x1, x3)
  wb6=as_workbook(xl)

  if(!is_testing()){
    openxlsx::saveWorkbook(wb1, file = "tests/testthat/xlsx/test_openxlsx1.xlsx", overwrite = TRUE)
    openxlsx::saveWorkbook(wb2, file = "tests/testthat/xlsx/test_openxlsx2.xlsx", overwrite = TRUE)
    openxlsx::saveWorkbook(wb3, file = "tests/testthat/xlsx/test_openxlsx3.xlsx", overwrite = TRUE)
    openxlsx::saveWorkbook(wb4, file = "tests/testthat/xlsx/test_openxlsx4.xlsx", overwrite = TRUE)
    openxlsx::saveWorkbook(wb5, file = "tests/testthat/xlsx/test_openxlsx5.xlsx", overwrite = TRUE)
    openxlsx::saveWorkbook(wb6, file = "tests/testthat/xlsx/test_openxlsx6.xlsx", overwrite = TRUE)
  }
})


## gt ----------------------------------------------------------------------


test_that("gt is working", {
  rlang::local_options(tidyselect_verbosity = "verbose") #oddly needed for as_gt(x2)
  #by=NULL
  x1=crosstable(mtcars2, c(mpg, vs, gear), total=T, test=T)
  as_gt(x1)
  as_gt(x1, keep_id=TRUE)
  expect_true(TRUE)

  #by=cyl
  x2=crosstable(mtcars2, c(mpg, vs, gear), by=cyl, total=T, test=T)
  as_gt(x2)
  as_gt(x2, keep_id=TRUE, show_test_name=FALSE, by_header="Cylinders")

  #by=c(cyl, am) --> error pour l'instant
  x3=crosstable(mtcars2, c(mpg, vs, gear), by=c(cyl, am), total=T)
  expect_snapshot_error(as_gt(x3))
})
