

#' @importFrom purrr map_dfr
#' @importFrom expss unlab
#' @importFrom survival is.Surv
#' @importFrom glue glue
#' @keywords internal
#' @noRd
cross_by = function(data_x, data_y, funs, funs_arg, margin, total, percent_digits, 
                    showNA, label, test, times, followup, 
                    test_args, cor_method, effect, effect_args){
    
    funs=clear_funs(funs)
    rtn_tbl = map_dfr(names(data_x), ~{
        if(is.numeric.and.not.surv(data_x[[.x]])){
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
            warning(glue("Cannot cross column '{x}' ({xx}) by column '{y}' ({yy})", 
                         x=names(data_x[.x]), xx=paste(class(unlab(data_x[[.x]])), collapse=", "), 
                         y=names(data_y[1]), yy=paste(class(unlab(data_y[[1]])))), collapse=", ")
        }
        
        rtn
    })
    
    return(rtn_tbl)
}


