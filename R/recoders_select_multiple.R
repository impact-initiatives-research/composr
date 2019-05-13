

selected_exactly_lgl<-function(x,from){
  if(length(x)!=length(from)){return(FALSE)}
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

  function(x,from,to,otherwise=NULL){

    assertthat::assert_that(is.vector(x),msg = "x must be a vector")
    assertthat::assert_that(assertthat::is.scalar(to),msg = "'to' takes only a single value")
    if(!is.null(otherwise)){assertthat::assert_that(assertthat::is.scalar(otherwise),msg = "'otherwise' takes only a single value")}

    from <- from %>% strsplit(" ") %>% as.vector %>% unlist



    x_recoded <- rep(NA, length(x))

    to_recode <- x %>% as.character %>% strsplit(" ") %>% lapply(function(x){
      recode_where(x,from)}) %>% unlist


    x_recoded[to_recode] <- to
    if(!is.null(otherwise)){
      x_recoded[!is.na(x) & !to_recode]<-otherwise
    }

    x_recoded[is.na(x)]<-NA

    x_recoded
  }

}










