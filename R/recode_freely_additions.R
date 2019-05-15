
#' apply function to each select_multiple response individually
#' @param x a vector of select_multiple responses separated by a space " "
#' @param FUN the function to be applied
#' @param ... further parameters passed to FUN
#' @export
by_sm<-function(x,FUN,...){
  if(!is.function(FUN)){stop("FUN is not a function. The function must be passed without paranthesis;example:  \"FUN=mean\", not \"FUN=mean()\")")}
  var_list<-strsplit(as.character(x)," ")
  var_evaluated<-sapply(var_list,FUN, ...) %>% unlist
  if(length(var_evaluated)!=length(x)){stop("function supplied to by_response must always return a single value (so that there is one value per response)")}
  var_evaluated
}



sm_selected<-function(x,
                      any=NULL,
                      all = NULL,
                      exactly=NULL,
                      none = NULL){


  choices_passed<-!(sapply(list(any,all,exactly,none),is.null))

  if( ((choices_passed) %>% which %>% length) !=1) { stop("provide exactly one of the arguments any,all,exactly or none.")}

  if(!is.null(any)){return(by_sm(x,selected_any_lgl,from=any))}
  if(!is.null(all)){return(by_sm(x,selected_all_lgl,from=all))}
  if(!is.null(exactly)){return(by_sm(x,selected_exactly_lgl,from=exactly))}
  if(!is.null(none)){return(by_sm(x,selected_none_lgl,from=any))}
  stop("provide exactly one of the arguments any,all,exactly or none. (2)")


}





