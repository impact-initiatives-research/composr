
compose<-function(.data,variable, to,
                    where.selected.any = NULL,
                    where.selected.all = NULL,
                    where.selected.exactly = NULL,
                    where.selected.none = NULL,
                    where.num.equal = NULL,
                    where.num.smaller = NULL,
                    where.num.smaller.equal = NULL,
                    where.num.larger = NULL,
                    where.num.larger.equal = NULL,
                    otherwise = NULL){




  if( (!(sapply(c(
    where.selected.any,
    where.selected.all,
    where.selected.exactly,
    where.selected.none,
    where.num.equal,
    where.num.smaller,
    where.num.smaller.equal,
    where.num.larger,
    where.num.larger.equal),is.null)) %>% which %>% length) !=1) { stop("provide exactly one of the 'where...' arguments.")}


  if(!is.null(where.selected.any)){
    recoder <- make_select_multiple_vectorized_recoder(selected_any_lgl)
    from <- where.selected.any
    recoding_name = paste0(variable,": where any selected from:",paste(where.selected.any,collapse=", "))
  }

  if(!is.null(where.selected.all)){
    recoder <- make_select_multiple_vectorized_recoder(selected_all_lgl)
    from <- where.selected.all
    recoding_name = paste0(variable,": where all selected from:",paste(where.selected.all,collapse=", "))
  }

  if(!is.null(where.selected.exactly)){
    recoder <- make_select_multiple_vectorized_recoder(selected_exactly_lgl)
    from<-where.selected.exactly
    recoding_name = paste0(variable,": where exactly selected:",paste(where.selected.exactly,collapse=", "))
  }

  if(!is.null(where.selected.none)){
    recoder <- make_select_multiple_vectorized_recoder(selected_none_lgl)
    from = where.selected.none
    recoding_name = paste0(variable,": where none selected from:",paste(where.selected.none,collapse=", "))

  }

  if(!is.null(where.num.equal)){
    recoder <- make_numeric_recoder(function(x,from){x == from})
    from = where.num.equal
    recoding_name = paste0(variable,": where num equal:",paste(where.num.equal))

  }

  if(!is.null(where.num.smaller)){
    recoder <- make_numeric_recoder(function(x,from){x < from})
    from = where.num.smaller
    recoding_name = paste0(variable,": where num equal:",paste(where.num.equal))

  }

  if(!is.null(where.num.smaller.equal)){
    recoder <- make_numeric_recoder(function(x,from){x <= from})
    from = where.num.smaller.equal
    recoding_name = paste0(variable,": where num smaller or equal:",paste(where.num.smaller.equal))

  }


  if(!is.null(where.num.larger)){
    recoder <- make_numeric_recoder(function(x,from){x > from})
    from = where.num.larger
    recoding_name = paste0(variable,": where num larger:",paste(where.num.larger))

  }


  if(!is.null(where.num.larger.equal)){
    recoder <- make_numeric_recoder(function(x,from){x >= from})
    from = where.num.larger.equal
    recoding_name = paste0(variable,": where num equal or larger:",paste(where.num.larger.equal))

  }




  compose_generic(.data,variable,from = from, to = to, recoder = recoder, recoding_name = recoding_name)


}
