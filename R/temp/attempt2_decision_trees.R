

df<-data.frame(a=sample(letters[1:5],100,T),b=sample(letters[4:10],100,T),c=sample(LETTERS[1:5],100,T))




  new_tree<-function(.data){
  attributes(.data)$branches<-list()

  class(.data)<-c("decision_tree",class(.data))
  .data
}




`grow<-`<-function(x, value){
  attributes(x)$branches<-c(attributes(x)$branches,list(value))
  print(x)
  x
}


# branch<-function(.branch,x){c(.branch,x)}
df<-tibble::tibble(var1=rep(c("A","B","C"),4),var2 = rep(c("A","B"),6),var3 = rep(c("A","B","C","D"),3))

tree<-new_tree(df)
(tree)
grow(tree) <-
  branch_any_selected(var = "var1",choices = c("A","B")) %>%
  branch_any_selected(var = "var2",choices = c("A","B","C")) %>%
  branch_not_selected(var = "var3",choices = c("C","D"))
print(tree)

print.decision_tree<-function(x){
  print(attributes(x)$branches)
  invisible(x)
}

get_branches(tree) %>% lapply(length)

get_branches<-function(tree){
  attributes(tree)$branches
}



get_branch_labels(tree)

get_branch_labels<-function(x){
  if(any(class(x)=="decision_tree")){
    x<-attributes(x)$branches
  }
  if(any(class(x)=="branch_condition")){
    return(attributes(x)$label)

  }else{
    lapply(x,get_branch_labels)
  }
}




solve_tree<-function(tree){
data<-tree %>% as_tibble
branches<-attributes(tree)$branches

}

branch<-get_branches(tree)[[1]]
solve_branch(tree,branch)

solve_branch<-function(data,branch){
  apply(data,1,function(x){
    with(x,{
    branch[[1]]()
      }
    )
    }
  )
}



df %>% new_composition("node1")



rm(print.branch_condition)

branch_any_selected<-function(.branch=NULL,var,choices){
  condition<-make_select_multiple_vectorized_recoder(function(){
    (eval(var) %in% choices)
  })
  class(condition)<-c("branch_condition",class(condition))
  attributes(condition)$type="any selected"
  attributes(condition)$var=var
  attributes(condition)$choices=choices
  attributes(condition)$label<-paste0(type," in '",var,"' : ",paste(choices,collapse=", "))
  c(.branch,condition)
}
















print.branch_condition<-function(x){
  attr<-attributes(x)
  with(attr,{
  if(exists("label")){cat(crayon::silver(label))}
    else(cat(crayon::italic("urecognised branch condition")))
  })
  invisible(x)
}



branch_not_selected<-function(.branch=NULL,var,choices){
  condition<-function(){
    !(eval(var) %in% choices)
  }
  class(condition)<-c("branch_condition",class(condition))
  attributes(condition)$type="none selected"
  attributes(condition)$var=var
  attributes(condition)$choices=choices
  attributes(condition)$label<-paste0(type," in '",var,"' : ",paste(choices,collapse=", "))

  c(.branch,condition)
}


branch_not_selected(var="asfd",choices = c("a","b")) %>%
  branch_not_selected(var="asfd",choices = c("a","b"))





print.decision_tree<-function(x){
  if(length(attributes(x)$branches)==0){cat(crayon::silver("tree with no branches"))}

  x<-lapply(attributes(x)$branches,function(branch){
    # cat(crayon::silver(paste0("(",branch,collapse="  -->  ",")")))
    print(branch)
    cat("\n========================\n")
  })
  invisible(x)
  # print(attributes(x)$branches)
}


