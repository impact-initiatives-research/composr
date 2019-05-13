
make_numeric_recoder<-function(recode_where){
  assertthat::assert_that(class(recode_where)=="function",msg = "'recode_where' must be a function")

  function(x,from,to,otherwise=NULL){

    assertthat::assert_that(is.vector(x),msg = "x must be a vector")
    assertthat::assert_that(assertthat::is.scalar(to),msg = "'to' takes only a single value")
    if(!is.null(otherwise)){assertthat::assert_that(assertthat::is.scalar(otherwise),msg = "'otherwise' takes only a single value")}

    from <- from %>% as.numeric

    x_recoded <- rep(NA, length(x))

    to_recode <- x %>% as.numeric %>% recode_where(as.numeric(from))


    x_recoded[to_recode] <- to
    if(!is.null(otherwise)){
      x_recoded[!is.na(x) & !to_recode]<-otherwise
    }

    x_recoded[is.na(x)]<-NA

    x_recoded
  }

}

