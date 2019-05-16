


to_where_as_label<-function(to, where){
  code <- paste(where,collapse="\n")
  code <- crayon::italic(crayon::blue(code))
  paste("freely to '",to,"':",code)
}



#' compose freely with a custom condition
#' @param .data an ongoing recoding (see new_recoding())
#' @param to the value to set to
#' @param where.string R code as a character string; evaluated in the namespace of the input data
#' @param questionnaire if you supply a questionnaire, you will be able to use the following functions within condition:
#'
#' - skipped(variable_name)
compose_freely<-function(.data,to,where.string,
                        questionnaire = NULL, ...){

  # catch bad input:
  if(!("composr_composition" %in% class(.data))) {stop("recode_to function chains must start with 'new_recoding()'")}



  assertthat::assert_that(assertthat::is.string(attributes(.data)$target),
                          msg = '`target` for recoding must be a single quoted character string naming a variable name')

  assertthat::assert_that(assertthat::is.scalar(to),
                          msg = '`to` must be only a single value. (see ?recode_to, ?new_recoding or browseVignettes("composr") for more details')


  if(length(to)!=1){"'to' must be a scalar (a single element)"}
  # if there is no questionnaire, then skipped can only be treated like NAs:






  # apply skiplogic:
  if(!is.null(questionnaire)){

    is_skipped<-function(var){
      questionnaire$question_is_skipped(.data, deparsed_string(deparse(substitute(var))))
    }
  }

  condition_result <- eval(parse(text = where.string), .data)


    if(!(is.logical(condition_result))){stop("'where' expression did not return a logical (TRUE/FALSE) vector")}
    if(!is.vector(condition_result)){stop("'where' expression must return a a vector")}
    if(!length(condition_result)==nrow(.data)){stop("'where' expression did not return one value per data row.")}

    x_recoded<-rep(NA,nrow(.data))
    x_recoded[condition_result]<-to
    x_recoded



  #
  #
  #   # otherwise and NA:
  #
  #   is_to <- !is.na(x_recoded)
  #   is_na <- is.na(x) & is.na(x_recoded) & !is_skipped
  #   is_otherwise<-!is_to & !is_na & !is_skipped
  #
  #   x_recoded[is_na] <- na.to
  #   x_recoded[is_otherwise]<-otherwise.to
  #   x_recoded[is_skipped]<-skipped.to

  # add to composition sequence
  attributes(.data)$sequence[,ncol(attributes(.data)$sequence)+1]<-(x_recoded)

  # update new composition variable

  .data[[attributes(.data)$target]]<-collapse_recoding_sequence(attributes(.data)$sequence)

  # add a label to the recoding:

  label <- to_where_as_label(to,where.string)
  attributes(.data)$recodings<-c(attributes(.data)$recodings,label)
  .data
}


deparsed_string<-function(x){
  paste0(as.character(x),collapse = "")
}
