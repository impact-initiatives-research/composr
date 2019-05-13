compose_generic<-function(.data,variable,from=NULL,to=NULL,recoder,recoding_name,...){

  # catch bad input:
  if(!("composr_composition" %in% class(.data))) {stop("compose_... function chains must start with 'new_composition()' (see ?composr)")}
  if(variable==attributes(.data)$composition_name){stop("you can not compose from a variable to itself; maybe you need to start a `new_composition()`?")}
  if(!(variable %in% names(.data))){stop(paste0("variable '",variable,"' not found in dataset"))}
  if(any(is.na(to))){stop("no NAs allowed in 'to' parameter.")}
  if(any(is.na(from))){stop("no NAs allowed in 'from' parameter.")}

  # apply recoder to source variable
  x<-as.character(.data[[variable]])
  x_recoded<-recoder(x,from,to,...)

  # add to composition sequence
  attributes(.data)$sequence[,ncol(attributes(.data)$sequence)+1]<-(x_recoded)

  # update new composition variable

  .data[[attributes(.data)$composition_name]]<-collapse_recoding_sequence(attributes(.data)$sequence)
  attributes(.data)$recodings<-c(attributes(.data)$recodings,recoding_name)
  .data
}






