#' Start a new recoding
#' @param df the source data as a data.frame
#' @param source the variable to recode from
#' @param target the name of the new variable created through the recoding
#' @return the input data frame with
#'  - an additional column named after the value of `target`
#'  - background setup to manage step by step recoding of the source variable
#'  @details
#'
#'  When conditions are conflicting, the last condition that applies is used
#'
#'  recoding is a special case of a composition, where the source variable is defined from the start and does not change.
#' @export
new_recoding<-function(df,source, target){
  composition_name <- target



  if(!is.data.frame(df)){stop("df must be a data.frame")}
  assertthat::is.string(composition_name)
  if(composition_name %in% names(df)){stop(glue::glue("your composition name '{composition_name}' is already a variable in the data"))}
  if(!tibble::is_tibble(df)){df<-tibble::as_tibble(df)}

  class(df)<-c("composr_composition",class(df)) %>% unique
  attributes(df)$composition_name<-composition_name
  attributes(df)$recodings<-c("fill NA")
  attributes(df)$source<-source
  attributes(df)$target<-target
  df[[composition_name]]<-NA
  attributes(df)$sequence<-tibble::tibble(.rows = nrow(df))
  df
}







recode_to<-function(.data,to,
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


  # recode_to is a wrapper for compose that sets the source variable:
  variable<-attributes(.data)$source

  compose(.data = .data,
          variable = variable,
          to = to,
          where.selected.any = where.selected.any,
          where.selected.all = where.selected.all,
          where.selected.exactly = where.selected.exactly,
          where.selected.none = where.selected.none,
          where.num.equal = where.num.equal,
          where.num.smaller = where.num.smaller,
          where.num.smaller.equal = where.num.smaller.equal,
          where.num.larger = where.num.larger,
          where.num.larger.equal = where.num.larger.equal,
          otherwise = otherwise)


}



#' turn active recoding back into a simple data frame
#' @param .data the recoding (see ?new_recoding)
end_recoding<-function(.data){
  end_composition(.data)


}


