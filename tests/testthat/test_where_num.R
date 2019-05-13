context("where smaller")


testdf<- data.frame(theletters=letters[1:12],ints_1_12 = 1:12, ints_1_6 = c(1:6,1:6),chars_1_12 = as.character(c(12:1)),
                    facs_12_1 = as.factor(as.character(c(12:1))),stringsAsFactors = F)






testthat::test_that("'where.num' work correctly",{


  recoded <- testdf %>%
    new_recoding("ints_1_12", "result") %>%
    recode_to(to = "X",where.num.smaller = 5) %>%
    end_recoding()

  correct<- c(rep("X",4),rep(NA,8))
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "smaller correct?")




  recoded <- testdf %>%
    new_recoding("ints_1_12", "result") %>%
    recode_to(to = "X",where.num.smaller.equal = 5) %>%
    end_recoding()

  correct<- c(rep("X",5),rep(NA,7))
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "smaller equal correct?")


  recoded <- testdf %>%
    new_recoding("ints_1_12", "result") %>%
    recode_to(to = "X",where.num.larger = 5) %>%
    end_recoding()

  correct<- c(rep(NA,5),rep("X",7))
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "larger correct?")


  recoded <- testdf %>%
    new_recoding("ints_1_12", "result") %>%
    recode_to(to = "X",where.num.larger.equal = 5) %>%
    end_recoding()

  correct<- c(rep(NA,4),rep("X",8))
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "larger.equal correct?")


  recoded <- testdf %>%
    new_recoding("ints_1_12", "result") %>%
    recode_to(to = "X",where.num.equal = 5) %>%
    end_recoding()

  correct<- c(rep(NA,4),"X",rep(NA,7))
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "num.equal correct?")





})


testthat::test_that("'where.num' work correctly (character input)",{


  recoded <- testdf %>%
    new_recoding("chars_1_12", "result") %>%
    recode_to(to = "X",where.num.smaller = 5) %>%
    end_recoding()

  correct<- c(rep("X",4),rep(NA,8))
  correct<-rev(correct)
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "smaller correct (char input)?")




  recoded <- testdf %>%
    new_recoding("chars_1_12", "result") %>%
    recode_to(to = "X",where.num.smaller.equal = 5) %>%
    end_recoding()

  correct<- c(rep("X",5),rep(NA,7))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "smaller equal correct (char input)?")


  recoded <- testdf %>%
    new_recoding("chars_1_12", "result") %>%
    recode_to(to = "X",where.num.larger = 5) %>%
    end_recoding()

  correct<- c(rep(NA,5),rep("X",7))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "larger correct (char input)?")


  recoded <- testdf %>%
    new_recoding("chars_1_12", "result") %>%
    recode_to(to = "X",where.num.larger.equal = 5) %>%
    end_recoding()

  correct<- c(rep(NA,4),rep("X",8))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "larger.equal correct (char input)?")


  recoded <- testdf %>%
    new_recoding("chars_1_12", "result") %>%
    recode_to(to = "X",where.num.equal = 5) %>%
    end_recoding()

  correct<- c(rep(NA,4),"X",rep(NA,7))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "num.equal correct (char input)?")

})



testthat::test_that("'where.num' work correctly (factor input)",{

  recoded <- testdf %>%
    new_recoding("facs_12_1", "result") %>%
    recode_to(to = "X",where.num.smaller = 5) %>%
    end_recoding()

  correct<- c(rep("X",4),rep(NA,8))
  correct<-rev(correct)
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "smaller correct (char input)?")




  recoded <- testdf %>%
    new_recoding("facs_12_1", "result") %>%
    recode_to(to = "X",where.num.smaller.equal = 5) %>%
    end_recoding()

  correct<- c(rep("X",5),rep(NA,7))
  correct<-rev(correct)
  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "smaller equal correct (char input)?")


  recoded <- testdf %>%
    new_recoding("facs_12_1", "result") %>%
    recode_to(to = "X",where.num.larger = 5) %>%
    end_recoding()

  correct<- c(rep(NA,5),rep("X",7))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "larger correct (char input)?")


  recoded <- testdf %>%
    new_recoding("facs_12_1", "result") %>%
    recode_to(to = "X",where.num.larger.equal = 5) %>%
    end_recoding()

  correct<- c(rep(NA,4),rep("X",8))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "larger.equal correct (char input)?")


  recoded <- testdf %>%
    new_recoding("facs_12_1", "result") %>%
    recode_to(to = "X",where.num.equal = 5) %>%
    end_recoding()

  correct<- c(rep(NA,4),"X",rep(NA,7))
  correct<-rev(correct)

  expect_true(dplyr::all_equal(recoded$result,correct),
              info = dplyr::all_equal(recoded$result,correct),
              "num.equal correct (char input)?")

})










