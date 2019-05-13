context("expected errors")


testdf<- data.frame(

  letters_sm = sapply(1:12,function(x){letters[x:1] %>% paste(collapse=" ")}),
  letters_sm_facts = as.factor(sapply(12:1,function(x){letters[x:1] %>% paste(collapse=" ")})),
  theletters=letters[1:12],ints_1_12 = 1:12, ints_1_6 = c(1:6,1:6),chars_1_12 = as.character(c(12:1)),
  facs_12_1 = as.factor(as.character(c(12:1))),stringsAsFactors = F)





testthat::test_that("does new_recoding() fail correctly on bad input?" ,{

  expect_error(new_recoding(NULL,NULL,NULL))
  expect_error(new_recoding(testdf,NA,NA))
  expect_error(new_recoding(testdf,NA,NA),regexp = "NA")
  expect_error(new_recoding(testdf,NULL,"asdf"),regexp = "NULL")
  expect_error(new_recoding(testdf,"asdf",NA),regexp = "NULL")
  expect_error(new_recoding(testdf,"asdf","asdf"),regexp = "NULL")

  expect_error(new_recoding(testdf,source = "letters_sm","letters_sm"))
  expect_error(new_recoding(testdf,source = "letters_sm","theletters"))
  ok_new_rec<-new_recoding(testdf,"letters_sm","new")


  expect_error(ok_new_rec %>% recode_to(),"provide")
  expect_error(ok_new_rec %>% recode_to(to = 7),"provide")
  expect_error(ok_new_rec %>% recode_to(to = 7,where.selected.any = 5),"provide")

})




