
#' apply function to each select_multiple response individually
#' @param x a vector of select_multiple responses separated by a space " "
#' @param FUN the function to be applied
#' @param ... further parameters passed to FUN
#' @return each value in x is split into a vector on " " (space); the function in FUN is applied to each of these vectors. We return a vector of these results
#' @export
by_sm<-function(x,FUN,...){
  if(!is.function(FUN)){stop("FUN is not a function. The function must be passed without paranthesis;example:  \"FUN=mean\", not \"FUN=mean()\")")}
  var_list<-strsplit(as.character(x)," ")
  var_evaluated<-sapply(var_list,FUN, ...) %>% unlist
  if(length(var_evaluated)!=length(x)){stop("function supplied to by_response must always return a single value (so that there is one value per response)")}
  var_evaluated
}



#' Check if select_multiple choices were selected
#'
#'@param  x a vector of select multiple responses, with choices separated by spaces
#'@param any TRUE if any of the values supplied here as a vector were selected
#'@param all TRUE if all of the values supplied here as a vector were selected
#'@param exactly TRUE if exactly all of the values supplied here as a vector were selected (an no others)
#'@param any TRUE if none of the values supplied here as a vector were selected
#'@details only supply one of any/all/exactly/any
#'@return a logical vector, same length as x
#'@export
sm_selected<-function(x,
                      any=NULL,
                      all = NULL,
                      exactly=NULL,
                      none = NULL){


  choices_passed<-!(sapply(list(any,all,exactly,none),is.null))

  if( ((choices_passed) %>% which %>% length) !=1) { stop("provide exactly one of the arguments any,all,exactly or none.")}

  stop_if_has_space<-function(x){
    if(any(grepl(" ", x))){
      stop("individual values supplied to any/all/exactly/none must not contain spaces. Individual choices must not contain spaces (since spaces are used to separate them)")
    }
  }
  if(!is.null(any)){
    stop_if_has_space(any)
    return(by_sm(x,selected_any_lgl,from=any))
    }
  if(!is.null(all)){
    stop_if_has_space(all)
    return(by_sm(x,selected_all_lgl,from=all))}
  if(!is.null(exactly)){
    stop_if_has_space(exactly)
    return(by_sm(x,selected_exactly_lgl,from=exactly))}
  if(!is.null(none)){
    stop_if_has_space(none)
    return(by_sm(x,selected_none_lgl,from=none))}

  stop("provide exactly one of the arguments any,all,exactly or none. (2)")


}





