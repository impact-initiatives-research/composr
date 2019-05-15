

selected_exactly_lgl<-function(x,from){
  if(length(x)!=length(from)){return(FALSE)}
  if(any(is.na(x))){return(NA)}
  all(sort(as.character(x)) == sort(as.character(from)))
}

selected_all_lgl<-function(x,from){
  all(from %in% x)
}

selected_any_lgl<-function(x,from){
  any(from %in% x)
}

selected_none_lgl<-function(x,from){
  !any(from %in% x)
}




make_select_multiple_vectorized_recoder<-function(recode_where){
  assertthat::assert_that(class(recode_where)=="function",msg = "'single_response_recoder' must be a function")

  function(x,from,to){

    assertthat::assert_that(is.vector(x),msg = "x must be a vector")
    assertthat::assert_that(assertthat::is.scalar(to),msg = "'to' takes only a single value")
    if(!is.vector(from)){stop("'from' and 'where...' parameters must be a scalar or a vector" )}

    from <- from %>% strsplit(" ") %>% as.vector %>% unlist



    x_recoded <- rep(NA, length(x))

    to_recode <- x %>% as.character %>% strsplit(" ") %>% lapply(function(x){
      recode_where(x,from)}) %>% unlist

    x_recoded[to_recode] <- to

    x_recoded
  }

}














