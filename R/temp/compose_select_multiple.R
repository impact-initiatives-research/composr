#'
#'
#'
#' #' compose new variable from select_multiple
#' #' @param .data an ongoing composition (see new_composition())
#' #' @param variable the name of the select_multiple variable to compose from
#' #' @param from_any_ticked the choices to recode; if ANY of these were ticked, the value will be changed to the value provided in 'to'. supply only one of the `from_...` parameters.
#' #' @param from_all_ticked the choices to recode; if ALL of these were ticked, the value will be changed to the value provided in 'to'. supply only one of the `from_...` parameters.
#' #' @param from_none_ticked the choices NOT to recode; if NONE of these were ticked, the value will be changed to the value provided in 'to'. supply only one of the `from_...` parameters.
#' #' @param from_exactly_ticked the choices to recode; if EXACTLY these were ticked (ALL of them ticked, NONE of the others ticked), the value will be changed to the value provided in 'to'. supply only one of the `from_...` parameters.
#' #' @param from_not_all_ticked the choices NOT to recode; if NOT ALL of these were ticked, the value will be changed to the value provided in 'to'. This also recodes if none or some (but not all..) provided choices were ticked. supply only one of the `from_...` parameters.
#' #' @return input data frame with updated composition
#' compose_from_select_multiple<-function(.data,
#'                                        variable,
#'                                        from_any_ticked=NULL,
#'                                        from_all_ticked=NULL,
#'                                        from_none_ticked=NULL,
#'                                        from_exactly_ticked=NULL,
#'                                        from_not_all_ticked=NULL,
#'                                        to,
#'                                        otherwise = NULL,
#'                                        skipped.to = NULL,
#'                                        na.to = NULL){
#'
#'
#'   available_methods = c(  "from_any_ticked",
#'                           "from_all_ticked",
#'                           "from_none_ticked",
#'                           "from_exactly_ticked",
#'                           "from_not_all_ticked")
#'
#'
#'   methods_used<-sapply(available_methods,function(x){!is.null(eval(parse(text=x)))})
#'
#'   if(length(which(methods_used))!=1){stop(paste("you must supply exactly one (and not more) out of the following arguments:",paste(methods,collapse = ", ")))}
#'
#'
#'   if(!is.null(from_any_ticked)){recoder <- recode_any_ticked}
#'   if(!is.null(from_all_ticked)){recoder <- recode_all_ticked}
#'   if(!is.null(from_none_ticked)){recoder <- recode_none_ticked}
#'   if(!is.null(from_exactly_ticked)){recoder <- recode_exactly_ticked}
#'   if(!is.null(from_not_all_ticked)){recoder <- recode_not_all_ticked}
#'
#'
#'   compose_generic(.data,variable,from,to,recoder,
#'                   recoding_name = paste("select_multiple, ",available_methods[which(methods_used)]),
#'                   otherwise=otherwise)
#'
#' }
#'
#'
#'
#'
#'








#
# recode_any_ticked<-(
#   function(x,from) {any(from %in% x)}
# ) %>% make_select_multiple_vectorized_recoder
#
# recode_all_ticked<-(function(x,from){
#   all(from %in% x)
# }) %>% make_select_multiple_vectorized_recoder
#
# recode_none_ticked<-(function(x,from){
#   !any(from %in% x)
# }) %>% make_select_multiple_vectorized_recoder
#
# recode_not_all_ticked<-(function(x,from){
#   !all(from,x)
# }) %>% make_select_multiple_vectorized_recoder
#
#
# recode_exactly_ticked<-(function(x,from){
#   all(sort(as.character(x)) == sort(as.character(from)))
# }) %>% make_select_multiple_vectorized_recoder
