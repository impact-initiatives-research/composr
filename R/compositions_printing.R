
collapse_recoding_sequence<-function(sequence){
  apply(sequence,1,function(x){
    # if any recoding worked out, return the last successful recoding value
    if(any(!is.na(x))){
      return(x[max(which(!is.na(x)))])
    }

    # otherwise NA
    return(NA)
  })
}




#' @method print composr_composition
#' @export
print.composr_composition<-function(x){

  x_simple<-x
  attributes(x_simple)$sequence<-NULL
  attributes(x_simple)$recodings<-NULL
  class(x_simple)<-class(x_simple)[class(x_simple)!="composr_composition"]
  cat("\n")
  print(x_simple)


  condition_count<-count_which_condition_applied(attributes(x)$sequence)

  cat(paste("currently composing: "))
  if(length(attributes(x)$recodings)>0){
    cat(paste0(
      paste0(crayon::silver(paste(" \n--->",
                                  paste(attributes(x)$recodings,"(",condition_count,"x effective)"))
      ),collapse=""),
      "\n",crayon::green("===> "),
      crayon::green(crayon::italic(attributes(x)$composition_name),"\n"))

    )
  }

  invisible(x)
}



count_which_condition_applied<-function(composition_sequence){
  which_applies<- apply(composition_sequence,1, function(x){
    suppressWarnings(condition_that_applied<-max(which(!is.na(x))))
    ifelse(is.finite(condition_that_applied),condition_that_applied,0)
  })

  factor(which_applies,0:ncol(composition_sequence)) %>% table %>% as.vector


}



