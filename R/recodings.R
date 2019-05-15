#' Start a new recoding
#' @param df the source data as a data.frame
#' @param source the variable to recode from
#' @param target the name of the new variable created through the recoding
#' @return the input data frame with
#'  - an additional column named after the value of `target`
#'  - background setup to manage step by step recoding of the source variable
#' @details
#'   When conditions are conflicting, the last condition that applies is used
#'  recoding is a special case of a composition, where the source variable is defined from the start and does not change.
#' @export
new_recoding<-function(df, target,source = NULL){
  source<-deparse(substitute(source))
  if(source=="NULL"){source<-NULL}

  if(!is.null(source)){

    is_quoted <- grepl('^\".*\"$',source) | grepl('^\'.*\'$',source)
    if(is_quoted & !(source %in% names(df))){
      stop(glue::glue("source variable \"{source}\" not found in data. Probably you need to remove the quotes and just put the name directly"))
    }
  }

  target<-deparse(substitute(target))
  target<-gsub("\'", "", target) %>% gsub("\"","", .)


  assertthat::is.string(target)

  df<-new_composition(df,target)
  attributes(df)$source<-source
  df

}





### add recoding condition layers


#' add layer to current recoding
#' @param .data the ongoing recoding obejct, see new_recoding()
#' @param to the value to set the new composition to if the condition is fulfilled
#' @param where.selected.. : a vector of choices; setting values to 'to' where in the source variable any/all/exactly/none of the supplied choices had been selected
#' @param where.num... : a scalar number. setting values to 'to' where the 'source' is equal / smaller / smaller or equal / larger / larger or euqal than the number supplied in where.num...
#' @param otherwise.to an alternative value to be used if the condition is not fulfilled, the source is not NA and not skipped
#' @param skipped.to an alternative value to be used if the  source is NA because the question was skipped (requires to also supply the `questionnaire` parameter)
#' @param na.to an alternative value to be used if the source is NA but not skipped (and the condition is was not fulfilled)
#' @param change.source you can change the source variable used; this will _continue_ to recode to the same target variable, and will overwrite previously fulfilled conditions.
#' @return the updated recoding
#' @examples
#'
#' df<-data.frame(a=1:100,b=sample(letters[1:5],100,T))
#'
#' df %>% new_composition("new_variable_name") %>%
#' compose("a",to = "less than 50" ,where.num.smaller = 50) %>%
#' compose("a",to = "more or equal 50", where.num.larger.equal = 50)
#' compose("b",to = "(size not important)",where.selected.exactly = "d") %>%
#' end_composition()
#'
#' @export
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
                    where = NULL,
                    otherwise.to = NA,
                    skipped.to = NA,
                    na.to = NA,
                    questionnaire = NULL,
                    source = NULL){


  # recode_to is a wrapper for compose that sets the source variable:
  source<-as.character(deparse(substitute(source)))

  if(source!="NULL"){
    .data <- recoding_set_source(.data,source)
  }
    source<-attributes(.data)$source





  where.string<-as.character(deparse(substitute(where)))





  compose(.data = .data,
          source = source,
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
          where.string = where.string,
          otherwise = otherwise.to,
          skipped.to = skipped.to,
          na.to = na.to,
          questionnaire = questionnaire)


}



#' turn active recoding back into a simple data frame
#' @param .data the recoding (see ?new_recoding)
#' @return the data as a regular data.frame (tibble), with the new recoded variable added. All meta information on the recoding process is discarded.
#' @export
end_recoding<-function(.data){
  end_composition(.data)


}




recoding_set_source<-function(.data,source){
  if(!is.null(source)){
  if(source!="NULL"){
    if(is.na(source)){stop("source can't be NA")}
    assertthat::assert_that(assertthat::is.string(source),msg = "source must a column name of the input data.")
    if(!(source %in% names(.data))){paste("source must be a column header in the input data ('", source,"' is not) " )}

    attributes(.data)$source <- source
  }
  }
.data
}

