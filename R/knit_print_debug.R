

#' For markdown compatibility, cf https://github.com/davidgohel/flextable/issues/216
#' View(flextable:::knit_print.flextable)
#' @keywords internal
#' @noRd
flextable_to_rmd = function (x, ft.align = opts_current$get("ft.align"), ft.split = opts_current$get("ft.split"),
                             ft.tabcolsep = opts_current$get("ft.tabcolsep"), ft.arraystretch = opts_current$get("ft.arraystretch"),
                             ft.left = opts_current$get("ft.left"), ft.top = opts_current$get("ft.top"),
                             webshot = opts_current$get("webshot"), bookdown = FALSE,
                             pandoc2 = TRUE, print = TRUE) {
  library(knitr)
  library(htmltools)
  library(graphics)
  html_value=flextable:::html_value
  pandoc_version=rmarkdown::pandoc_version

  str <- ""
  is_xaringan <- !is.null(getOption("xaringan.page_number.offset"))
  if (is.null(opts_knit$get("rmarkdown.pandoc.to"))) {
    str <- html_value(x, ft.align = ft.align, bookdown = FALSE,
                      pandoc2 = FALSE, ft.shadow = FALSE)
  }
  else if (is_xaringan) {
    str <- html_value(x, ft.align = ft.align, bookdown = FALSE,
                      pandoc2 = FALSE, ft.shadow = TRUE)
  }
  else if (grepl("(html|slidy|gfm|markdown_strict)", opts_knit$get("rmarkdown.pandoc.to"))) {
    str <- html_value(x, ft.align = ft.align, bookdown = bookdown,
                      pandoc2 = pandoc2)
  }
  else if (grepl("latex", opts_knit$get("rmarkdown.pandoc.to"))) {
    str <- latex_value(x, ft.tabcolsep = ft.tabcolsep, ft.align = ft.align,
                       ft.arraystretch = ft.arraystretch, bookdown = bookdown)
  }
  else if (grepl("docx", opts_knit$get("rmarkdown.pandoc.to"))) {
    if (pandoc2) {
      str <- docx_value(x, bookdown = bookdown, ft.align = ft.align,
                        ft.split = ft.split)
    }
    else {
      stop("pandoc version >= 2.0 required for flextable rendering in docx")
    }
  }
  else if (grepl("pptx", opts_knit$get("rmarkdown.pandoc.to"))) {
    if (pandoc_version() < numeric_version("2.4")) {
      stop("pandoc version >= 2.4 required for printing flextable in pptx")
    }
    str <- pptx_value(x, ft.left = ft.left, ft.top = ft.top,
                      bookdown = bookdown)
  }
  else {
    if (is.null(webshot_package <- webshot)) {
      webshot_package <- "webshot"
    }
    if (requireNamespace(webshot_package, quietly = TRUE)) {
      plot_counter <- getFromNamespace("plot_counter",
                                       "knitr")
      in_base_dir <- getFromNamespace("in_base_dir", "knitr")
      tmp <- fig_path("png", number = plot_counter())
      width <- flextable_dim(x)$width
      height <- flextable_dim(x)$height
      in_base_dir({
        dir.create(dirname(tmp), showWarnings = FALSE,
                   recursive = TRUE)
        save_as_image(x, path = tmp, zoom = 3, expand = 0,
                      webshot = webshot_package)
      })
      str <- sprintf("\\includegraphics[width=%.02fin,height=%.02fin,keepaspectratio]{%s}\n",
                     width, height, tmp)
    }
  }
  if (print) {
    cat(str, "\n", sep = "")
  }
  invisible(str)
}
