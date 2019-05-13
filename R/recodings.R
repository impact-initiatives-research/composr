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
new_recoding<-function(df,source, target){

  df<-new_composition(df,target)
  attributes(df)$source<-source
  df

}





### compose: more user friendly interface for "compose.generic" with predefined options


#' add layer to current recoding
#' @param .data the ongoing recoding obejct, see new_recoding()
#' @param to the value to set the new composition to if the condition is fulfilled
#' @param where.selected.. : a vector of choices; setting values to 'to' where in the source variable any/all/exactly/none of the supplied choices had been selected
#' @param where.num... : a scalar number. setting values to 'to' where the 'source' is equal / smaller / smaller or equal / larger / larger or euqal than the number supplied in where.num...
#' @param otherwise an alternative value to be used if the condition is not fulfilled, the source is not NA and not skipped
#' @return the updated composition
#' @expamples
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
                    otherwise.to = NA,
                    skipped.to = NA,
                    na.to = NA,
                    questionnaire = NULL){


  # recode_to is a wrapper for compose that sets the source variable:
  source<-attributes(.data)$source

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
          otherwise = otherwise.to,
          skipped.to = skipped.to,
          na.to = na.to,
          questionnaire = questionnaire)


}



#' turn active recoding back into a simple data frame
#' @param .data the recoding (see ?new_recoding)
#' @return the data as a regular data.frame (tibble), with the new recoded variable added. All meta information on the recoding process is discarded.
end_recoding<-function(.data){
  end_composition(.data)


}


