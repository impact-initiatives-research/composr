#' use a variable as default value for a composition
#' @param .data a composition data frame (output from new_composition() or any other composition function)
#' @param variable a variable to copy from for default values for the composed variable
#' @export
compose_default<-function(.data,variable){
  recoder<-function(x,from=NULL,to=NULL,...){
    identity(x)
  }

  compose_generic(.data,variable,recoder = recoder,recoding_name="copy")
}






#' Recode select_multiple to value in "to"
#'
#'@param x a composition; output from `new_composition()` or any other `compose` function
#'@param from  vector of possible respones; returns `to` if _any_ of these were selected
#'@param to the new value of fields that contained _any_ of the values in `from`
#'@details NA's stay NA
#'@return vector of the same length as th input vector, all values are either a numeric value specified in "to" or NA's
#'@export
compose_select_multiple_any <- function(.data, variable,from, to,otherwise=NULL){
  recoder<-function(x,from,to,otherwise){

    x_recoded <- rep(NA, length(x))
    from <- from %>% strsplit(" ") %>% as.vector %>% unlist

    # create recoding (isolated from previous recodings):
    match_any <- x %>% as.character %>% strsplit(" ") %>% lapply(function(x){
      match(from, x)})
    make_false_any <- lapply(match_any, function(x){all(is.na(x))}) %>% unlist
    x_recoded[!make_false_any] <- to
    x_recoded

    if(!is.null(otherwise)){
      has_other_value<-!(x %in% c(from,NA))
      x_recoded[is.na(x_recoded) & has_other_value]<-otherwise
    }
    x_recoded
  }

  compose_generic(.data,variable,from,to,recoder,
                  recoding_name = "select_multiple matching any value",
                  otherwise=otherwise)
}




#' Recode select_multiple to value in "to"
#'
#'@param x a composition; output from `new_composition()` or any other `compose` function
#'@param from  vector of possible respones; returns `to` if _any_ of these were selected
#'@param to the new value of fields that contained _any_ of the values in `from`
#'@details NA's stay NA
#'@return vector of the same length as th input vector, all values are either a numeric value specified in "to" or NA's
#'@export
compose_select_multiple_all <- function(.data, variable,from, to,otherwise=NULL){
  assertthat::assert_that(assertthat::is.string(variable))
  assertthat::assert_that(assertthat::is.scalar(to),msg = "'to' takes only a single value")

  recoder<- make_select_multiple_vectorized_recoder(select_multiple_condition_all)

  compose_generic(.data,variable,from,to,recoder,
                  recoding_name = "select_multiple matching any value",
                  otherwise=otherwise)
}



df %>% new_composition("test") %>% compose_select_multiple_all(variable = "var4",from = c("a","b","c"),to = 'ABC ALL')

df %>% new_recoding(source = 'var5',target = "test2") %>%
  recode_to("XXX",where.selected.all = c("a", "b")) %>%
  recode_to("YYY", where.selected.any = c("d","e")) %>%
  recode_to("WOW", where.selected.exactly = c("b","a")) %>%
  new_recoding("var4","test3") %>%
  recode_to("top",where.num.larger = 1) %>%
  recode_to("middle",where.num.smaller.equal = 1) %>%
  recode_to("low",where.num.smaller.equal = 0.5) %>%
  recode_to("lowest",where.num.smaller.equal = 0.2)



df %>% new_composition(composition_name = "target") %>%



  compose_generic(.data,variable,from,to,recoder,
                  recoding_name = "select_multiple matching any value",
                  otherwise=otherwise)














