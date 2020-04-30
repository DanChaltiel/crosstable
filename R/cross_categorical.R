
#' @importFrom dplyr select mutate mutate_all everything .data
#' @keywords internal
#' @noRd
cross_categorical=function(data_x, data_y, showNA, total, label, percent_digits, margin,
                           test, test_args, effect, effect_args){
    
    stopifnot(ncol(data_x)==1 && (is.null(data_y) || ncol(data_y)==1))
    stopifnot(is.character.or.factor(data_x[[1]]))
    
    x_name = get_label(data_x, label)
    y_name = get_label(data_y, label)
    
    if(is.null(data_y)){
        rtn=summarize_categorical_single(data_x, showNA=showNA, total=total, 
                                         digits=percent_digits, margin=margin)
    } else if(is.character.or.factor(data_y[[1]])){
        rtn=summarize_categorical_by(data_x[[1]], data_y[[1]], margin=margin, showNA=showNA, 
                                     total=total, digits=percent_digits, 
                                     test=test, test_args=test_args, 
                                     effect=effect, effect_args=effect_args)
    } else {
        return(NULL)
    }
    
    rtn = rtn %>% 
        mutate(.id=names(data_x), label=x_name) %>% 
        select(.data$.id, .data$label, everything()) %>% 
        mutate_all(as.character)
    
    rtn
}

