#' Start a new composition
#' @param df the source data as a data.frame
#' @param composition_name the name of the variable that will be composed and added to the data
#' @return the input data frame with
#'  - an additional column named after the value of `composition_name`
#'  - background setup to manage step by step composition of that variable from others.
#' @export
new_composition<-function(df,composition_name){

  if(!is.data.frame(df)){stop("df must be a data.frame")}
  assertthat::is.string(composition_name)
  if(composition_name %in% names(df)){stop(glue::glue("your composition name '{composition_name}' is already a variable in the data"))}
  if(!tibble::is_tibble(df)){df<-tibble::as_tibble(df)}

  class(df)<-c("composr_composition",class(df)) %>% unique
  attributes(df)$composition_name<-composition_name
  attributes(df)$recodings<-c("fill NA")
  df[[composition_name]]<-NA
  attributes(df)$sequence<-tibble::tibble(.rows = nrow(df))
  df
}

