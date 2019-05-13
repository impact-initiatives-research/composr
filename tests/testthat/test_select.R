context("select conditions")



testdf<- data.frame(

  letters_sm = sapply(1:12,function(x){letters[x:1] %>% paste(collapse=" ")}),
  letters_sm_facts = as.factor(sapply(12:1,function(x){letters[x:1] %>% paste(collapse=" ")})),
  theletters=letters[1:12],ints_1_12 = 1:12, ints_1_6 = c(1:6,1:6),chars_1_12 = as.character(c(12:1)),
  facs_12_1 = as.factor(as.character(c(12:1))),stringsAsFactors = F)






testthat::test_that("'select' work correctly (char input)",{


  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.any =  letters[6:length(letters)]) %>%
    end_recoding()

  correct<- c(rep(NA,5),rep("X",7))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.any correct?")


  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.all =  letters[1:10]) %>%
    end_recoding()
  cbind(recoded$result,correct,recoded$letters_sm)
  print(letters[1:10])
  correct<- c(rep(NA,9),rep("X",3))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.all correct?")




  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.none =  letters[10:26]) %>%
    end_recoding()
  correct<- c(rep("X",9),rep(NA,3))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.none correct?")



  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.none =  letters[10:26]) %>%
    end_recoding()
  correct<- c(rep("X",9),rep(NA,3))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.none correct?")



  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.exactly =  letters[1:5]) %>%
    end_recoding()
  correct<- c(rep(NA,4),"X",rep(NA,7))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.exactly correct?")







})


testthat::test_that("'select' work correctly (char and space separated 'where' input)",{


  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.exactly =  c("a b  ", "c", "   e d")) %>%
    end_recoding()
  correct<- c(rep(NA,4),"X",rep(NA,7))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.exactly correct - mixed spaces and vector input")




  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.any =  c( "h","ASDF", "   e d", "g f   ")) %>%
    end_recoding()
  correct<- c(rep(NA,3),rep("X",9))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.any correct - mixed spaces and vector input for 'where..'")





  recoded <- testdf %>%
    new_recoding("letters_sm", "result") %>%
    recode_to(to = "X",where.selected.all =  c( "a b c ", "   e d", "g f   ")) %>%
    end_recoding()
  correct<- c(rep(NA,6),rep("X",6))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.all  correct - mixed spaces and vector input for 'where..'")


})










