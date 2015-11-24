library(svamp)

## test_1

a <- report(222240, ppn_obj = "//UBUNTU1/share/result.rda", view = TRUE)
a <- report(c(222240, 94403), ppn_obj = "//UBUNTU1/share/result.rda", view = TRUE)
a <- report(ppn = c(11732,  40196,  88930, 222286), ppn_obj = "//UBUNTU1/share/result.rda", view = TRUE)

## test_2

## res <- tools::assertError(
##   foo(parameter that causes error)
## )

## stopifnot(length(grep("The expected error message",
##                      res[[1]]$message)) > 0)
