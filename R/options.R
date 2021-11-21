
#' Options for the package `crosstable`
#' 
#' Use this function to manage your `crosstable` parameters globally while taking advantage of autocompletion. Use [crosstable_peek_options()] to see which option is currently set.
#'
#' @param reset if `TRUE`, set all these options to default
#' 
#' @param crosstable_zero_percent set to TRUE so that proportions are not displayed if `n==0`
#' @param crosstable_wrap_id if `id` contains no spaces, wrap it with this maximum number of characters.
#' @param crosstable_verbosity_autotesting one of `default`, `quiet`, or `verbose`
#' @param crosstable_only_round default argument for [format_fixed()]
#' @param crosstable_units default units in [body_add_gg2()] and [body_add_img2()]
#' @param crosstable_peek_docx behavior of [peek()], which will open a `docx` if `TRUE` (default) and an `xlsx` if `FALSE`
#' @param crosstable_compact_padding in flextables, left-padding for non-headers rows when `compact=TRUE`.
#' 
#' @param crosstable_total For setting [crosstable()] arguments globally. 
#' @param crosstable_percent_pattern For setting [crosstable()] arguments globally. 
#' @param crosstable_percent_digits For setting [crosstable()] arguments globally. 
#' @param crosstable_showNA For setting [crosstable()] arguments globally. 
#' @param crosstable_label For setting [crosstable()] arguments globally. 
#' @param crosstable_funs For setting [crosstable()] arguments globally. 
#' @param crosstable_funs_arg For setting [crosstable()] arguments globally. 
#' @param crosstable_cor_method For setting [crosstable()] arguments globally. 
#' @param crosstable_unique_numeric For setting [crosstable()] arguments globally. 
#' @param crosstable_date_format For setting [crosstable()] arguments globally. 
#' @param crosstable_times For setting [crosstable()] arguments globally. 
#' @param crosstable_followup For setting [crosstable()] arguments globally. 
#' @param crosstable_test_arg For setting [crosstable()] arguments globally. 
#' @param crosstable_effect_args For setting [crosstable()] arguments globally. 
#' 
#' @param crosstable_keep_id For setting [as_flextable()] arguments globally. 
#' @param crosstable_autofit For setting [as_flextable()] arguments globally. 
#' @param crosstable_compact For setting [as_flextable()] arguments globally. 
#' @param crosstable_remove_header_keys For setting [as_flextable()] arguments globally. 
#' @param crosstable_show_test_name For setting [as_flextable()] arguments globally. 
#' @param crosstable_padding_v For setting [as_flextable()] arguments globally. 
#' @param crosstable_header_show_n For setting [as_flextable()] arguments globally. 
#' @param crosstable_fontsize_body For setting [as_flextable()] arguments globally. 
#' @param crosstable_fontsize_header For setting [as_flextable()] arguments globally. 
#' @param crosstable_fontsize_subheaders For setting [as_flextable()] arguments globally. Subheaders are only considered when `compact=TRUE`.
#' 
#' @param crosstable_style_normal For specifying styles used in your {officer} template. 
#' @param crosstable_style_character For specifying styles used in your {officer} template. 
#' @param crosstable_style_strong For specifying styles used in your {officer} template. 
#' @param crosstable_style_image For specifying styles used in your {officer} template. 
#' @param crosstable_style_legend For specifying styles used in your {officer} template. 
#' @param crosstable_style_heading For specifying styles used by headings on different levels. Levels will be pasted in the end (e.g. use `"title"` if your level 2 heading style is `"title2"`).
#' @param crosstable_style_list_ordered,crosstable_style_list_unordered For specifying styles used by lists in the `rdocx` template. Needed for [body_add_list()] to work.
#' 
#'   
#' @sparam crosstable_total,crosstable_percent_pattern,crosstable_percent_digits,crosstable_showNA,crosstable_label,crosstable_funs,crosstable_funs_arg,crosstable_cor_method,crosstable_unique_numeric,crosstable_date_format,crosstable_times,crosstable_followup,crosstable_test_arg,crosstable_effect_args, For setting [crosstable()] arguments globally
#' @sparam crosstable_keep_id,crosstable_autofit,crosstable_compact,crosstable_remove_header_keys,crosstable_show_test_name,crosstable_compact_padding,crosstable_padding_v,crosstable_header_show_n,crosstable_fontsize_body,crosstable_fontsize_header,crosstable_fontsize_subheaders, For setting [as_flextable()] arguments globally. Subheaders are only considered when `compact=TRUE`.
#' @sparam crosstable_style_normal,crosstable_style_character,crosstable_style_strong,crosstable_style_image,crosstable_style_legend, For specifying styles in {officer} `rdocx` output.
#'
#' @return Nothing, called for its side effects
#' @export
crosstable_options = function(reset=FALSE, 
                              crosstable_zero_percent, 
                              crosstable_verbosity_autotesting, 
                              crosstable_wrap_id, 
                              crosstable_only_round, 
                              crosstable_units, 
                              crosstable_peek_docx,
                              crosstable_compact_padding, 
                              #crosstable()
                              crosstable_total, crosstable_percent_pattern, crosstable_percent_digits, crosstable_showNA, crosstable_label, crosstable_funs, crosstable_funs_arg, crosstable_cor_method, crosstable_unique_numeric, crosstable_date_format, crosstable_times, crosstable_followup, crosstable_test_arg, crosstable_effect_args, 
                              #as_flextable()
                              crosstable_keep_id, crosstable_autofit, crosstable_compact, crosstable_remove_header_keys, crosstable_show_test_name, crosstable_padding_v, crosstable_header_show_n, crosstable_fontsize_body, crosstable_fontsize_header, crosstable_fontsize_subheaders, 
                              #styles
                              crosstable_style_normal, crosstable_style_character, crosstable_style_strong, crosstable_style_image, crosstable_style_legend, crosstable_style_heading, crosstable_style_list_ordered, crosstable_style_list_unordered){
    
    #TODO externaliser un check_args_crosstable() pour crosstable() et pour crosstable_options()
    if(isTRUE(reset)){
        argg = formals(crosstable_options) %>% map(~NULL)
        argg$reset = NULL
        do.call(options, argg)
        warn("All crosstable options were set back to default.")
        return(invisible())
    }
    
    argg = as.list(match.call())[-1] %>% map(as.character)
    do.call(options, argg)
    invisible()
}



#' See which `crosstable` option is currently set.
#' 
#' @param keep_null set to TRUE to get a list
#'
#' @return A named list of crosstable options
#' @importFrom rlang peek_options
#' @importFrom purrr discard
#' @export
crosstable_peek_options = function(keep_null=FALSE){
    x = formals(crosstable_options)
    rtn = peek_options(names(x))
    if(!isTRUE(keep_null)) rtn = discard(rtn, is.null)
    rtn
}

