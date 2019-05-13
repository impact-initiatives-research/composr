df<-data.frame(var1=sample(LETTERS[1:5],100,T),
               var2=sample(LETTERS[4:10],100,T),
               var3=sample(LETTERS[1:5],100,T),
               var4 = runif(100),
               var5 = sapply(1:100,function(x){
                 sample(letters[1:5],sample(1:5,1)) %>% paste(collapse=" ")
               }) %>% unlist)


any_ticked<-function(x,choices){
  x %>% as.character %>% strsplit(" ") %>% sapply(function(ticked){any(choices %in% ticked)})
}

library(dplyr)
df %>%
  new_recoding("var5","my_target_variable") %>%
  recode_to(1,where.selected.any = c("a","b")) %>%
  recode_to(0,where.selected.all = c("c")) %>%
  recode_to(0,where.selected.all = c("d")) %>%
  recode_to(1,where.selected.all = c("c","d")) %>%




df %>% new_composition("food_access") %>%
  compose_select_multiple_any("var1",c("A","B"),to=0) %>%
  compose_select_multiple_all(variable = "var1",from = c("C"),to = 1)  %>%
  new_composition("food_quality") %>%
  compose_generic("var4",to=0,recoder = function(x,to,...){x_recoded<-rep(NA,length(x)) x_recoded[x<1]<-to},recoding_name = "smaller 1") %>%
  end_composition()
  mutate(food_total = food_access+food_quality)

  end_composition


  attributes(.data)

composition_stats<-function(.data){
  composition_sequence<- attributes(.data)[["sequence"]]
  count_which_condition_applied(composition_sequence) %>% as_tibble %>% print


}







composition_get_sequence<-function(x){
  theseq<- attributes(x)[["sequence"]]
  names(theseq)<-attributes(x)[["recodings"]][-1]
  theseq
}




set_to<-function(.data,value,otherwise.to = NA,na.to = NA,skipped.to = NA){

  is_value<- apply(.data,1,all)
  is_na<-apply(.data,1,function(x){any(is.na(x))})
  is_otherwise<-!is_value & !is_na

  vec<-rep(NA,nrow(.data))
  vec[is_value]<-value
  vec[is_na] <- na.to
  vec[is_otherwise]<-otherwise.to
  tibble::tibble(vec)
}





