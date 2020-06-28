
#' @importFrom purrr map_dfr
#' @importFrom survival is.Surv
#' @importFrom glue glue glue_data glue_collapse
#' @keywords internal
#' @noRd
cross_by = function(data_x, data_y, funs, funs_arg, margin, total, percent_digits, 
                    showNA, label, test, times, followup, 
                    test_args, cor_method, effect, effect_args){
    
    funs = clear_funs(funs)
    errors = data.frame(name=character(0), class=character(0))
    
    rtn_tbl = map_dfr(names(data_x), ~{
        if(is.numeric.and.not.surv(data_x[[.x]]) || is.date(data_x[[.x]])){
            rtn=cross_numeric(data_x[.x], data_y, funs=funs, funs_arg=funs_arg, 
                              showNA=showNA, total=total, label=label, 
                              cor_digits=percent_digits, cor_method=cor_method,
                              test=test, test_args=test_args, effect=effect, effect_args=effect_args)
        } else if(is.character.or.factor(data_x[[.x]])){
            rtn=cross_categorical(data_x[.x], data_y, margin=margin,
                                  showNA=showNA, total=total, label=label, percent_digits=percent_digits,
                                  test=test, test_args=test_args, effect=effect, effect_args=effect_args)
        } else if(is.Surv(data_x[[.x]])){
            rtn=cross_survival(data_x[.x], data_y, times=times, followup=followup,
                               showNA=showNA, total=total, label=label, surv_digits=percent_digits,
                               test=test, test_args=test_args, effect=effect, effect_args=effect_args)
        } else {
            rtn=NULL
        }
        
        if(is.null(rtn)){
            errors <<- rbind(errors, data.frame(name=.x, class=paste(class(remove_label(data_x[[.x]])), collapse=", ")))
        }
        
        rtn
    })
    
    if(nrow(errors)>0){
        s=if(nrow(errors)>1) "s" else ""
        errors_s = glue_data(errors, "'{name}' ({class})") %>% glue_collapse(", ", last = " and ")
        if(is.null(data_y)){
            warning(call. = FALSE, 
                    glue("Cannot describe column{s} {errors_s}"))
        } else {
            warning(call. = FALSE, 
                    glue("Cannot cross column{s} {errors_s} by column '{y}' ({yy})", 
                         y=names(data_y[1]),  yy=paste(class(remove_label(data_y[[1]] )), collapse=", ")))
        }
    }
    
    if("effect" %in% names(rtn_tbl) && any(rtn_tbl$effect=="No effect?")){
        x=rtn_tbl %>% filter(effect=="No effect?") %>% pull(.data$.id) %>% unique
        s=if(length(x)>1) "s" else ""
        v=glue_collapse(x, "', '", last="' and '")
        warning(glue("Could not calculate crosstable effects for variable{s} '{v}'. Aren't there 2 groups exactly?"), call. = FALSE)
    }
    
    return(rtn_tbl)
}
