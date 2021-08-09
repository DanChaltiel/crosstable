
#' @importFrom purrr imap_dfr map_dfr imap_chr
#' @importFrom survival is.Surv
#' @importFrom glue glue glue_data glue_collapse
#' @keywords internal
#' @noRd
cross_by = function(data_x, data_y, funs, funs_arg, margin, total, percent_digits, 
                    showNA, label, test, times, followup, 
                    test_args, cor_method, effect, effect_args){
    if(!is.null(data_y) && ncol(data_y)>1) abort(glue("data_y has {ncol(data_y)} columns (max=1)"))
    errors = rlang::env()
    
    by_levels = length(unique(na.omit(data_y[[1]])))
    if(!is.numeric(data_y[[1]]) && isTRUE(effect) && by_levels!=2){
        info_by = NULL
        if(!is.null(data_y)) info_by = c()
        warn(c(glue("Cannot calculate crosstable effects as there is has not exactly 2 groups in `by`."),
               i=glue("`by` has {by_levels} levels")),
             class = "crosstable_effect_2groups_warning")
        effect = FALSE
    }
    
    rtn_tbl = imap_dfr(data_x, ~{
        if(all(is.na(.x))) .x = "NA"
        if(anyNA(.x) && "NA" %in% .x) {
            na_string = glue_collapse(which(.x=="NA") , ", ", last = ", and ")
            s1 = if(sum(.x=="NA", na.rm=TRUE)>1) "s" else ""
            na_proper = glue_collapse(which(is.na(.x)), ", ", last = ", and ")
            s2 = if(sum(is.na(.x))>1) "s" else ""
            warn(c(glue('Cannot describe column "{.y}" as it contains both `NA` (missing values) and "NA" (string)'),
                   i=glue('NA as strings on row{s1} {na_string}'),
                   i=glue('NA missing value on row{s2} {na_proper}')),
                 class = "crosstable_na_char_warning")
            errors[[.y]] = data.frame(name=.y, class="Both `NA` and 'NA'")
            return(NULL)
        }
        
        if(inherits(.x, "difftime")){ 
            lab = get_label(.x)
            .x = as.numeric(.x) %>% set_label(lab)
        }
        
        if(is.list(.x)){
            errors[[.y]] = data.frame(name=.y, class="list")
            return(NULL)
        }
        
        if(is.list(.x)){
            errors[[.y]] = data.frame(name=.y, class="list")
            return(NULL)
        }
        
        data_x[.y] = .x
        if(is.numeric.and.not.surv(.x) || is.date(.x)){
            rtn=cross_numeric(data_x[.y], data_y, funs=funs, funs_arg=funs_arg, 
                              showNA=showNA, total=total, label=label, 
                              cor_digits=percent_digits, cor_method=cor_method,
                              test=test, test_args=test_args, effect=effect, effect_args=effect_args)
        } else if(is.character.or.factor(.x)){
            rtn=cross_categorical(data_x[.y], data_y, margin=margin,
                                  showNA=showNA, total=total, label=label, percent_digits=percent_digits,
                                  test=test, test_args=test_args, effect=effect, effect_args=effect_args)
        } else if(is.Surv(.x)){
            rtn=cross_survival(data_x[.y], data_y, times=times, followup=followup,
                               showNA=showNA, total=total, label=label, surv_digits=percent_digits,
                               test=test, test_args=test_args, effect=effect, effect_args=effect_args)
        } else {
            rtn=NULL
        }
        
        if(is.null(rtn)){
            errors[[.y]] = data.frame(name=.y, class=paste_classes(.x))
        }
        rtn
    })
    
    
    errors = as.list(errors) %>% map_dfr(identity)
    if(nrow(errors)>0){
        s=if(nrow(errors)>1) "s" else ""
        errors_s = glue_data(errors, "'{name}' ({class})") %>% glue_collapse(", ", last = ", and ")
        if(is.null(data_y)){
            warn(glue("Cannot describe column{s} {errors_s}"),
                 class = "crosstable_wrong_col_class_warning")
        } else {
            warn(glue("Cannot cross column{s} {errors_s} by column '{y}' ({yy})", 
                      y=names(data_y[1]),  yy=paste_classes(data_y[[1]])),
                 class = "crosstable_wrong_col_class_by_warning")
        }
    }
    
    if("effect" %in% names(rtn_tbl) && any(rtn_tbl$effect=="No effect?")){
        x=rtn_tbl %>% filter(effect=="No effect?") %>% pull(.data$.id) %>% unique
        s=if(length(x)>1) "s" else ""
        v=glue_collapse(x, "', '", last="', and '")
        warn(glue("Cannot calculate crosstable effects for variable{s} '{v}'"),
             class = "crosstable_effect_other_warning")
    }
    
    rownames(rtn_tbl)=NULL
    return(rtn_tbl)
}

