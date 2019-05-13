
make_numeric_recoder<-function(recode_where){
  assertthat::assert_that(class(recode_where)=="function",msg = "'recode_where' must be a function")

  function(x,from,to){

    assertthat::assert_that(is.vector(x),msg = "x must be a vector")
    assertthat::assert_that(assertthat::is.scalar(to),msg = "'to' takes only a single value")
    assertthat::assert_that(is.vector(x),msg = "x must be a vector")
    assertthat::assert_that(assertthat::is.scalar(to),msg = "'to' takes only a single value")

    if(!is.vector(from)){stop("'from' and 'where...' parameters must be a scalar or a vector" )}
    if(is.null(from)){stop("'from' and 'where...' parameters must not be NULL")}
    if(any(is.na(from))){stop("'from' and 'where...' parameters must not be NA")}

    from <- from %>% as.character %>% as.numeric
    if(is.na(from)){stop("'from' / where.num... parameter could not be interpreted as a number")}

    x <- as.numeric(as.character(x))

    x_recoded <- rep(NA, length(x))
    to_recode <- recode_where(x,from)

    x_recoded[to_recode] <- to

    x_recoded
  }

}

