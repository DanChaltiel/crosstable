


#' For makrdown compatibility, cf https://github.com/davidgohel/flextable/issues/216
#' @keywords internal
#' @noRd
knit_print.flextable = function (x, ...) {
    #debug version2 for markdown documents
    library(knitr)
    html_str=flextable:::html_str
    html_str.flextable=flextable:::html_str.flextable
    pandoc_version=rmarkdown::pandoc_version
    library(htmltools)
    library(graphics)
    
    is_bookdown <- isTRUE(opts_knit$get("bookdown.internal.label"))
    if (is.null(opts_knit$get("rmarkdown.pandoc.to"))) {
        knit_print(asis_output(html_str(x)))
    }
    else if (grepl("(html|slidy|gfm|markdown_strict)", opts_knit$get("rmarkdown.pandoc.to"))) {
        tab_class <- "tabwid"
        if (!is.null(align <- opts_current$get("ft.align"))) {
            if (align == "left") 
                tab_class <- "tabwid tabwid_left"
            else if (align == "right") 
                tab_class <- "tabwid tabwid_right"
        }
        # knit_print(asis_output(htmltools_value(x, class = tab_class, bookdown = is_bookdown)))
        knit_print(htmltools_value(x, class = tab_class, bookdown = is_bookdown))
    }
    else if (grepl("(latex|beamer)", opts_knit$get("rmarkdown.pandoc.to"))) {
        if (is.null(webshot_package <- opts_current$get("webshot"))) {
            webshot_package <- "webshot"
        }
        if (requireNamespace(webshot_package, quietly = TRUE)) {
            webshot_fun <- getFromNamespace("webshot", webshot_package)
            plot_counter <- getFromNamespace("plot_counter", 
                                             "knitr")
            in_base_dir <- getFromNamespace("in_base_dir", "knitr")
            tmp <- fig_path("png", number = plot_counter())
            width <- flextable_dim(x)$width
            height <- flextable_dim(x)$height
            in_base_dir({
                dir.create(dirname(tmp), showWarnings = FALSE, 
                           recursive = TRUE)
                tf <- tempfile(fileext = ".html", tmpdir = ".")
                save_as_html(x = x, path = tf)
                webshot_fun(url = basename(tf), file = tmp, 
                            selector = "body > table", zoom = 3, expand = 0)
                unlink(tf)
            })
            knit_print(asis_output(sprintf("\\includegraphics[width=%.02fin,height=%.02fin,keepaspectratio]{%s}\n", 
                                           width, height, tmp)))
        }
    }
    else if (grepl("docx", opts_knit$get("rmarkdown.pandoc.to"))) {
        if (pandoc_version() >= 2) {
            str <- docx_value(x, print = FALSE, bookdown = is_bookdown)
            knit_print(asis_output(str))
        }
        else {
            stop("pandoc version >= 2.0 required for flextable rendering in docx")
        }
    }
    else if (grepl("pptx", opts_knit$get("rmarkdown.pandoc.to"))) {
        if (pandoc_version() < 2.4) {
            stop("pandoc version >= 2.4 required for printing flextable in pptx")
        }
        if (is.null(left <- opts_current$get("ft.left"))) 
            left <- 1
        if (is.null(top <- opts_current$get("ft.top"))) 
            top <- 2
        uid <- as.integer(runif(n = 1) * 10^9)
        str <- pml_flextable(x, uid = uid, offx = left, offy = top, 
                             cx = 10, cy = 6)
        knit_print(asis_output(paste("```{=openxml}", str, "```", 
                                     sep = "\n")))
    }
    else {
        stop("unsupported format for flextable rendering:", 
             opts_knit$get("rmarkdown.pandoc.to"))
    }
}


