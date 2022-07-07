
#' Options for the package `crosstable`
#'
#' Use this function to manage your `crosstable` parameters globally while taking advantage of autocompletion. Use [crosstable_peek_options()] to see which option is currently set and [crosstable_reset_options()] to set all options back to default.
#'
#' @param reset if `TRUE`, set all these options back to default
#' @param ... unused
#'
#' @param zero_percent set to TRUE so that proportions are not displayed if `n==0`
#' @param wrap_id if `id` contains no spaces, wrap it with this maximum number of characters.
#' @param verbosity_autotesting one of `default`, `quiet`, or `verbose`
#' @param verbosity_duplicate_cols one of `default`, `quiet`, or `verbose`.
#' @param only_round default argument for [format_fixed()]
#' @param units default units in [body_add_gg2()] and [body_add_img2()]
#' @param peek_docx behavior of [peek()], which will open a `docx` if `TRUE` (default) and an `xlsx` if `FALSE`
#' @param compact_padding in flextables, left-padding for non-headers rows when `compact=TRUE`.
#' @param font_code font family used to show code, most likely a monospaced typeface such as Consolas (default)
#' @param format_legend_name how the legend name ("Table", "Figure") is formatted. Default to `officer::fp_text_lite(bold=TRUE)`
#' @param table_legend_par_before whether to add an empty paragraph before all table legends
#' @param figure_legend_par_after whether to add an empty paragraph after all figure legends
#' @param table_legend_prefix,figure_legend_prefix a prefix before each legend, after the numbering
#' @param header_show_n_pattern glue pattern used when showing N in the header of flextables. `.col` is the name of the column and `.n` the size of the group. Default to `{.col} (N={.n})`.
#'
#' @param total For setting [crosstable()] arguments globally.
#' @param percent_pattern For setting [crosstable()] arguments globally.
#' @param percent_digits For setting [crosstable()] arguments globally.
#' @param num_digits For setting [crosstable()] arguments globally.
#' @param showNA For setting [crosstable()] arguments globally.
#' @param label For setting [crosstable()] arguments globally.
#' @param funs For setting [crosstable()] arguments globally.
#' @param funs_arg For setting [crosstable()] arguments globally.
#' @param cor_method For setting [crosstable()] arguments globally.
#' @param unique_numeric For setting [crosstable()] arguments globally.
#' @param date_format For setting [crosstable()] arguments globally.
#' @param times For setting [crosstable()] arguments globally.
#' @param followup For setting [crosstable()] arguments globally.
#' @param test_arg For setting [crosstable()] arguments globally.
#' @param effect_args For setting [crosstable()] arguments globally.
#'
#' @param keep_id For setting [as_flextable()] arguments globally.
#' @param autofit For setting [as_flextable()] arguments globally.
#' @param compact For setting [as_flextable()] arguments globally.
#' @param remove_header_keys For setting [as_flextable()] arguments globally.
#' @param show_test_name For setting [as_flextable()] arguments globally.
#' @param padding_v For setting [as_flextable()] arguments globally.
#' @param header_show_n For setting [as_flextable()] arguments globally.
#' @param fontsize_body For setting [as_flextable()] arguments globally.
#' @param fontsize_header For setting [as_flextable()] arguments globally.
#' @param fontsize_subheaders For setting [as_flextable()] arguments globally. Subheaders are only considered when `compact=TRUE`.
#'
#' @param normal_squish Should you squish text in normal paragraphs?
#' @param title_squish Should you squish text in headers paragraphs?
#' @param allow_break allow crosstable rows to break across pages
#' @param style_normal For specifying styles used in your {officer} template.
#' @param style_character For specifying styles used in your {officer} template.
#' @param style_strong For specifying styles used in your {officer} template.
#' @param style_image For specifying styles used in your {officer} template.
#' @param style_legend For specifying styles used in your {officer} template.
#' @param style_heading For specifying styles used by headings on different levels. Levels will be pasted in the end (e.g. use `"title"` if your level 2 heading style is `"title2"`).
#' @param style_list_ordered,style_list_unordered For specifying styles used by lists in the `rdocx` template. Needed for [body_add_list()] to work.
#'
#' @seealso [crosstable_peek_options()] and [crosstable_reset_options()]
#' @return Nothing, called for its side effects
#' @export
crosstable_options = function(zero_percent,
                              verbosity_autotesting,
                              verbosity_duplicate_cols,
                              wrap_id,
                              only_round,
                              units,
                              peek_docx,
                              compact_padding,
                              font_code,
                              format_legend_name,
                              table_legend_par_before,
                              table_legend_prefix,
                              figure_legend_par_after,
                              figure_legend_prefix,
                              header_show_n_pattern,
                              #crosstable()
                              total, percent_pattern, percent_digits, num_digits, showNA, label, funs, funs_arg,
                              cor_method, unique_numeric, date_format, times, followup, test_arg, effect_args,
                              #as_flextable()
                              keep_id, autofit, compact, remove_header_keys, show_test_name, padding_v,
                              header_show_n, fontsize_body, fontsize_header, fontsize_subheaders,
                              #officer
                              normal_squish, title_squish, allow_break,
                              #styles
                              style_normal, style_character, style_strong, style_image,
                              style_legend, style_heading, style_list_ordered, style_list_unordered,
                              ...,
                              reset=deprecated()){

  #TODO externaliser un check_args_crosstable() pour crosstable() et pour crosstable_options()
  if(!missing(reset)){
    deprecate_warn("0.5.0", "crosstable_options(reset)", "crosstable_reset_options()")
    crosstable_reset_options(quiet=TRUE)
    return(invisible())
  }

  argg = as.list(match.call())[-1]
  args_ok = names(formals(crosstable_options)) %>% .[!. %in% c("...", "reset")]
  argdot = list(...)
  argdot_ok = paste0("crosstable_", args_ok)

  unknown_argdot = argdot[!names(argdot) %in% argdot_ok]
  argg = argg[!names(argg) %in% names(unknown_argdot)]
  if(length(unknown_argdot)>0){
    cli::cli_warn(c("Unknown crosstable option{?s} were ignored: {names(unknown_argdot)}."),
                  class="crosstable_unknown_option_warning")
  }

  dup = names(argg) %in% paste0("crosstable_", names(argg))
  dup_args = names(argg)[dup]
  argg = argg[!names(argg) %in% dup_args]
  prefix = ifelse(str_starts(names(argg), "crosstable_"), "", "crosstable_")
  names(argg) = paste0(prefix, names(argg))
  if(length(dup_args)>0){
    cli::cli_warn(c("Duplicated crosstable option{?s} were ignored: {dup_args}.",
                    i='You can now remove the old "crosstable_" prefix.'),
                  class="crosstable_dupl_option_warning")
  }

  options(argg)
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
  names(x) = paste0("crosstable_", names(x))
  rtn = peek_options(names(x))
  if(!isTRUE(keep_null)) rtn = discard(rtn, is.null)
  rtn
}

#' Reset all `crosstable` options.
#'
#' @param quiet set to `TRUE` to remove the message.
#'
#' @return Nothing, called for its side effects
#' @importFrom rlang set_names
#' @importFrom cli cli_inform
#' @importFrom purrr map
#' @export
crosstable_reset_options = function(quiet=FALSE){
  args_ok = names(formals(crosstable_options)) %>% .[!. %in% c("...", "reset")]
  argg = args_ok %>% set_names() %>% map(~NULL)
  names(argg) = paste0("crosstable_", names(argg))
  options(argg)
  if(isFALSE(quiet)) cli_inform("All crosstable options were set back to default.") #nocov
  return(invisible())
}

