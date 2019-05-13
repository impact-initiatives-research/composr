context("select conditions (factor inputs)")



testdf<- data.frame(

  letters_sm = rev(as.factor(sapply(12:1,function(x){letters[x:1] %>% paste(collapse=" ")}))),
  stringsAsFactors = F)






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
    recode_to(to = "X",where.selected.any =  c( "h","ASDF", "   e d", "g f")) %>%
    end_recoding()
  correct<- c(rep(NA,3),rep("X",9))

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "selected.any correct - mixed spaces and vector input for 'where..'")


})










