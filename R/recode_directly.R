
#' recode directly to a value
#' @param .data an ongoing recoding (see new_recoding())
#' @param to_expression R code as a character string; evaluated in the namespace of the input data, and result will be the 'to' value; will overwrite everything that is not NA here
#' @param questionnaire if you supply a questionnaire, you will be able to use `is_skipped()` in the expression.
#' @details the expression is evaluated _on each row individually_. in that world, each variable corresponds to an _individual value_. This allows you to do for example max(var1, var2) - this will return the larger value between var1 and  var2 of _each record_.
#' @export
recode_directly<-function(.data,to_expression,
                          questionnaire = NULL, ...){
  to_expression<-deparsed_string(as.character(deparse(substitute(to_expression))))


  # catch bad input:
  if(!("composr_composition" %in% class(.data))) {stop("recode_to function chains must start with 'new_recoding()'")}



  assertthat::assert_that(assertthat::is.string(attributes(.data)$target),
                          msg = '`target` for recoding must be a single quoted character string naming a variable name')




  # apply skiplogic:



  final_result <- purrr::pmap(.data,function(...){

    this<-list(...);

    if(!is.null(questionnaire)){

      is_skipped<-function(var){
        questionnaire$question_is_skipped(as.data.frame(this,stringsAsFactors=F), deparsed_string(deparse(substitute(var))))
      }
    }

    eval(parse(text = to_expression),envir = this)
  }) %>% unlist


  if(!is.vector(final_result)){stop("'where' expression must return a a vector")}
  if(!length(final_result)==nrow(.data)){stop("'where' expression did not return one value per data row.")}

  x_recoded<-final_result



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

  label <- paste0("directly: ",to_expression)
  attributes(.data)$recodings<-c(attributes(.data)$recodings,label)
  .data
}
