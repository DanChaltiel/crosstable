
#' @importFrom survival is.Surv
#' @importFrom dplyr mutate select everything mutate_all .data
#' @keywords internal
#' @noRd
cross_survival=function(data_x, data_y, showNA, total, label, surv_digits, times, followup, 
                        margin, test, test_args, effect, effect_args){
    
    stopifnot(ncol(data_x)==1 && (is.null(data_y) || ncol(data_y)==1))
    stopifnot(is.Surv(data_x[[1]]))
    
    x_name = get_label(data_x, label)
    y_name = get_label(data_y, label)
    
    if(is.null(data_y)){
        rtn=summarise_survival_single(data_x[[1]], times=times, followup=followup, 
                                      digits=surv_digits)
    } else if(is.character.or.factor(data_y[[1]])){
        rtn=summarise_survival_by(data_x[[1]], data_y[[1]], times=times, followup=followup, 
                                  digits=surv_digits, total=total, showNA=showNA, test=test, 
                                  test_args=test_args, effect=effect, effect_args=effect_args)
    } else {
        return(NULL)
    }
    
    rtn = rtn %>% 
        mutate(.id=names(data_x), label=x_name) %>% 
        select(.data$.id, .data$label, everything()) %>% 
        mutate_all(as.character)
    
    rtn
}


