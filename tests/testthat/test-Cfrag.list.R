
context("Cfrag.list")

test.list <- list(adoub=as.double(-3:20), anint=as.integer(92:109),
                  achar=c("aaa", "bbbb", "ccccc"))

test_that(
  "Cfrag.list",
  {
    expect_equal(Cfrag.list(test.list, file = "", declaration.only = TRUE),
                 c("\tdouble adoub[];", "\tint anint[];", "\tchar *achar[];")
    )
    expect_equal(Cfrag.list(test.list, file = ""),
                 c("\tdouble adoub[] = {", "\t\t-3, -2, -1,", "\t\t0, 1, 2,",
                   "\t\t3, 4, 5,", "\t\t6, 7, 8,", "\t\t9, 10, 11,", "\t\t12, 13, 14,",
                   "\t\t15, 16, 17,", "\t\t18, 19, 20", "\t};", "\tint anint[] = {",
                   "\t\t92, 93, 94, 95, 96, 97, 98, 99, 100, 101,", "\t\t102, 103, 104, 105, 106, 107, 108, 109",
                   "\t};", "\tchar *achar[] = {", "\t\t\"aaa\", \"bbbb\", \"ccccc\"",
                   "\t};")
    )

  }
)

