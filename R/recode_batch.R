

#' apply many recodings at once with vector of 'where' conditions
#' @param df a data frame or an ongoing recoding
#' @param tos a vector of "to" values
#' @param wheres a vector of "where" conditions; R code as strings (evaluated in namespace of the data)
#' @param targets vector of target variables to create as characters. each change triggers a new_recoding(). if left empty, recodes to taret specified in new_recoding().
#' @param return the ongoing recoding from after the last 'where' recoding. return to regular data frame with all new recodings visible with end_recoding()
recode_batch<-function(df,tos,wheres,targets = NULL, questionnaire = NULL){

  if(!is.data.frame(df)){stop("df must be a data frame")}
  assertthat::assert_that(is.character(tos) | is.numeric(tos))
  assertthat::assert_that(is.character(wheres),msg = "'wheres' must be a character vector")
  if(length(tos)!=length(wheres)){stop("'tos' and 'wheres' must have the same length")}
  if(!is.null(targets) & length(targets)!=length(wheres)){
    stop("'targets' and 'wheres' must have the same length. Each change in 'target' will trigger a new_recoding()")
    }



  for(i in 1:length(wheres)){
    if(!is.null(targets)){
    df <- set_target(df,targets[i])
    }
    df <- compose_freely(df,to = tos[i],where.string = wheres[i],questionnaire = questionnaire)

  }

  df

}



new_recoding_<-function(df,target, source = NULL){

  if(!is.null(source)){


    if(!(source %in% names(df))){
      stop(glue::glue("source variable \"{source}\" not found in data."))
    }
  }


  assertthat::is.string(target)

  df<-new_composition(df,target)
  attributes(df)$source<-source
  df
}



set_target<-function(df,target){
 if(is.null(attributes(df)$target)){
   return(df %>% new_recoding_(target))
 }

 if(target==attributes(df)$target){
   return(df)
 }

 if(target != attributes(df)$target){
   return(df %>% new_recoding_(target))
 }

}
