#### new_composition: adds classes and attributes to a data frame

#' Start a new composition
#' @param df the source data as a data.frame
#' @param target the name of the variable that will be composed and added to the data
#' @return the input data frame with
#'  - an additional column named after the value of `target`
#'  - background setup to manage step by step composition of that variable from others.
new_composition<-function(df,target){

  if(!is.data.frame(df)){stop("df must be a data.frame")}
  assertthat::is.string(target)
  if(target %in% names(df)){stop(glue::glue("your composition name '{target}' is already a variable in the data"))}
  if(!tibble::is_tibble(df)){df<-tibble::as_tibble(df)}

  class(df)<-c("composr_composition",class(df)) %>% unique
  attributes(df)$target<-target
  attributes(df)$recodings<-c("NA by default ")
  df[[target]]<-NA
  attributes(df)$sequence<-tibble::tibble(.rows = nrow(df))
  df
}





### compose: more user friendly interface for "compose.generic" with predefined options


#' add layer to current composition
#' @param .data the composition, see new_composition()
#' @param source the name of the source variable to compose from
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
compose<-function(.data,
                  source,
                  to,
                  where.selected.any = NULL,
                  where.selected.all = NULL,
                  where.selected.exactly = NULL,
                  where.selected.none = NULL,
                  where.num.equal = NULL,
                  where.num.smaller = NULL,
                  where.num.smaller.equal = NULL,
                  where.num.larger = NULL,
                  where.num.larger.equal = NULL,
                  where.string = NULL,
                  otherwise.to = NA,
                  skipped.to = NA,
                  na.to = NA,
                  questionnaire = NULL){



  # make sure only one type of "where" condition is used:
  where_condition_passed<-!(sapply(list(
    where.selected.any,
    where.selected.all,
    where.selected.exactly,
    where.selected.none,
    where.num.equal,
    where.num.smaller,
    where.num.smaller.equal,
    where.num.larger,
    where.num.larger.equal),is.null))

  where_condition_passed <- c(where_condition_passed,where.string !="NULL")
  if( ((where_condition_passed) %>% which %>% length) !=1) { stop("provide exactly one of the 'where...' arguments.")}


  # if it's a free 'where' recoding, make it someone else's (= compose_freely) problem:
  if(!where.string=="NULL"){
    if(!all(sapply(list(otherwise.to, skipped.to,na.to),function(x){is.na(x)}))){
      stop("you can not use \"otherswise.to\",\"skipped.to\",\"na.to\" together wih the generic \"where\" or \"where.string\" condition, because the generic 'where' doesn't have a specific source variable that these '...to' parameters would relate to.")
    }
    composition<- compose_freely(.data = .data,
                                 to = to,
                                 where.string = where.string,
                                 questionnaire = questionnaire)
    # if compose_freely was used, stop here:

    return(composition)
  }




  if(!is.null(where.selected.any)){
    recoder <- make_select_multiple_vectorized_recoder(selected_any_lgl)
    from <- where.selected.any
    recoding_name = paste0(source,": where any selected from:",paste(where.selected.any,collapse=", "))
  }

  if(!is.null(where.selected.all)){
    recoder <- make_select_multiple_vectorized_recoder(selected_all_lgl)
    from <- where.selected.all
    recoding_name = paste0(source,": where all selected from:",paste(where.selected.all,collapse=", "))
  }

  if(!is.null(where.selected.exactly)){
    recoder <- make_select_multiple_vectorized_recoder(selected_exactly_lgl)
    from<-where.selected.exactly
    recoding_name = paste0(source,": where exactly selected:",paste(where.selected.exactly,collapse=", "))
  }

  if(!is.null(where.selected.none)){
    recoder <- make_select_multiple_vectorized_recoder(selected_none_lgl)
    from = where.selected.none
    recoding_name = paste0(source,": where none selected from:",paste(where.selected.none,collapse=", "))

  }

  if(!is.null(where.num.equal)){
    recoder <- make_numeric_recoder(function(x,from){round(x,100) == round(from,100)})
    from = where.num.equal
    recoding_name = paste0(source,": where num equal:",paste(where.num.equal))

  }

  if(!is.null(where.num.smaller)){
    recoder <- make_numeric_recoder(function(x,from){x < from})
    from = where.num.smaller
    recoding_name = paste0(source,": where num equal:",paste(where.num.equal))

  }

  if(!is.null(where.num.smaller.equal)){
    recoder <- make_numeric_recoder(function(x,from){x <= from})
    from = where.num.smaller.equal
    recoding_name = paste0(source,": where num smaller or equal:",paste(where.num.smaller.equal))

  }


  if(!is.null(where.num.larger)){
    recoder <- make_numeric_recoder(function(x,from){x > from})
    from = where.num.larger
    recoding_name = paste0(source,": where num larger:",paste(where.num.larger))

  }


  if(!is.null(where.num.larger.equal)){
    recoder <- make_numeric_recoder(function(x,from){x >= from})
    from = where.num.larger.equal
    recoding_name = paste0(source,": where num equal or larger:",paste(where.num.larger.equal))

  }





  compose_generic(.data,
                  source,
                  from = from,
                  to = to,
                  recoder = recoder,
                  recoding_name = recoding_name,
                  otherwise.to = otherwise.to,
                  skipped.to = skipped.to,
                  na.to = na.to,
                  questionnaire = questionnaire)


}




compose_generic<-function(.data,source,from=NULL,to=NULL,recoder,recoding_name,otherwise.to = NA, skipped.to = NA, na.to = NA, questionnaire = NULL, ...){

  # catch bad input:
  if(!("composr_composition" %in% class(.data))) {stop("compose_... function chains must start with 'new_composition()' or 'new_recoding()'")}
  if(source==attributes(.data)$target){stop("you can not compose from a variable to itself; maybe you need to start a `new_composition()` or a 'new_recoding()' with a different `target` variable name?")}

  assertthat::assert_that(assertthat::is.string(source),
                          msg = '`source` must be a single quoted character string naming a variable name, for example:
                          compose(.... , source = "my_column_name")')

  if(!(source %in% names(.data))){stop(paste0("variable '",source,"' provided as `source` not found in dataset"))}

  assertthat::assert_that(assertthat::is.scalar(to),
                          msg = '`to` must be only a single value. (see ?compose, ?recode or browseVignettes("composr") for more details')

  if(!is.vector(from)) {stop("'from' and where... inputs must be a scalar or a vector. (see ?compose, ?recode or browseVignettes('composr') for more details")}

  if(any(is.na(to))){stop("no NAs allowed in 'to' parameter.")}
  if(any(is.na(from))){stop("no NAs allowed in 'from' parameter. If the input data is missing, usually it doesn't make sense to turn that into values. To ignore this very sound advice, you can use the `na.to` parameter.")}
  if(!is.na(na.to)){warning("the na.to parameter is not recommended. Turning NA's / missing data into values is usually a bad idea. Please make 100% sure you're not creating data where really there is none..")}

  if(length(to)!=1){"'to' must be a scalar (a single element)"}
  # if there is no questionnaire, then skipped can only be treated like NAs:





  # apply recoder to source variable
  x<-as.character(.data[[source]])
  x_recoded<-recoder(x, from, to, ...)


  # where do the special cases apply?

  # apply skiplogic:
  if(is.null(questionnaire)){
    if(!(skipped.to %in% na.to)){
      if(!is.na(skipped.to)){
        stop("To treat skipped questions different from NAs, you must supply a questionnaire. (see ?koboquest::load_questionnaire)")
      }
    }
  }

  if(!is.na(skipped.to) & !is.null(questionnaire)){
    is_skipped <- questionnaire$question_is_skipped(.data, source)
  }else{
    is_skipped<-rep(FALSE, nrow(.data))
  }

  # otherwise and NA:

  is_to <- !is.na(x_recoded)
  is_na <- is.na(x) & is.na(x_recoded) & !is_skipped
  is_otherwise<-!is_to & !is_na & !is_skipped

  x_recoded[is_na] <- na.to
  x_recoded[is_otherwise]<-otherwise.to
  x_recoded[is_skipped]<-skipped.to

  # add to composition sequence
  attributes(.data)$sequence[,ncol(attributes(.data)$sequence)+1]<-(x_recoded)

  # update new composition variable

  .data[[attributes(.data)$target]]<-collapse_recoding_sequence(attributes(.data)$sequence)
  attributes(.data)$recodings<-c(attributes(.data)$recodings,recoding_name)
  .data
}



#' end composition
#' @param .data the ongoing composition
#' @details discards all composition meta information
#' @return data.frame with the newly composed variable(s)
end_composition<-function(.data){
  tibble::as_tibble(.data)
}




