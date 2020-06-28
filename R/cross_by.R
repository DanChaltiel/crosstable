

#' @importFrom purrr map_dfr
#' @importFrom expss unlab
#' @importFrom survival is.Surv
#' @importFrom glue glue glue_data glue_collapse
#' @keywords internal
#' @noRd
cross_by = function(data_x, data_y, funs, funs_arg, margin, total, percent_digits, 
                    showNA, label, test, times, followup, 
                    test_args, cor_method, effect, effect_args){
    
    funs=clear_funs(funs)
    
    errors = data.frame(name=character(0), class=character(0))
    
    rtn_tbl = map_dfr(names(data_x), ~{
        if(is.numeric.and.not.surv(data_x[[.x]])){
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
            errors <<- rbind(errors, data.frame(name=.x, class=paste(class(unlab(data_x[[.x]])), collapse=", ")))
            # warning(glue("Cannot cross column '{x}' ({xx}) by column '{y}' ({yy})", 
            #              x=names(data_x[.x]), xx=paste(class(unlab(data_x[[.x]])), collapse=", "), 
            #              y=names(data_y[1]),  yy=paste(class(unlab(data_y[[1]] )), collapse=", ")))
        }
        
        rtn
    })
    
    if(nrow(errors)>0){
        s=if(nrow(errors)>1) "s" else ""
        errors_s = glue_data(errors, "'{name}' ({class})") %>% glue_collapse(", ", last = " and ")
        warning(call. = FALSE, 
                glue("Cannot cross column{s} {errors_s} by column '{y}' ({yy})", 
                     y=names(data_y[1]),  yy=paste(class(unlab(data_y[[1]] )), collapse=", ")))
    }
    
    if("effect" %in% names(rtn_tbl) && any(rtn_tbl$effect=="No effect?")){
        x=rtn_tbl %>% filter(effect=="No effect?") %>% pull(.data$.id) %>% unique
        s=if(length(x)>1) "s" else ""
        v=glue_collapse(x, "', '", last="' and '")
        warning(glue("Could not calculate crosstable effects for variable{s} '{v}'. Aren't there 2 groups exactly?"), call. = FALSE)
    }
    
    return(rtn_tbl)
}


# crosstable(mtcars3, dummy_num, by=disp)
# crosstable(mtcars3, by=disp)
# 
# errors=read.table(header=T, text="     name     class
# 1     cyl    factor
# 2      vs character
# 3      am character
# 4    gear    factor
# 5    cyl3 character
# 6    cyl6 character
# 7   dummy character
# 8  dummy2 character
# 9    test    factor
# 10   surv      Surv")

