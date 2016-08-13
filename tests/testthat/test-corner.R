
context("corner")

tm1 <- matrix(1:100, 10)
tm2 <- tm1; colnames(tm2) <- LETTERS[1:10]
tm3 <- tm1; rownames(tm3) <- letters[1:10]
tm4 <- tm2; rownames(tm4) <- letters[1:10]
td1 <- as.data.frame(tm4)
ta1 <- array(1:1000, rep(10, 3))

test_that(
  "corner",
  {
    expect_equal(corner(tm1),
                 structure(c(1L, 2L, 3L, 4L, 5L, 6L, 11L, 12L, 13L, 14L, 15L,
                             16L, 21L, 22L, 23L, 24L, 25L, 26L, 31L, 32L, 33L, 34L, 35L, 36L,
                             41L, 42L, 43L, 44L, 45L, 46L, 51L, 52L, 53L, 54L, 55L, 56L), .Dim = c(6L,
                                                                                                   6L))
    )
    expect_equal(corner(tm2, "br"),
                 structure(c(45L, 46L, 47L, 48L, 49L, 50L, 55L, 56L, 57L, 58L,
                             59L, 60L, 65L, 66L, 67L, 68L, 69L, 70L, 75L, 76L, 77L, 78L, 79L,
                             80L, 85L, 86L, 87L, 88L, 89L, 90L, 95L, 96L, 97L, 98L, 99L, 100L
                 ), .Dim = c(6L, 6L), .Dimnames = list(NULL, c("E", "F", "G",
                                                               "H", "I", "J")))
    )
    expect_equal(corner(tm3, "bl"),
                 structure(c(5L, 6L, 7L, 8L, 9L, 10L, 15L, 16L, 17L, 18L, 19L,
                             20L, 25L, 26L, 27L, 28L, 29L, 30L, 35L, 36L, 37L, 38L, 39L, 40L,
                             45L, 46L, 47L, 48L, 49L, 50L, 55L, 56L, 57L, 58L, 59L, 60L), .Dim = c(6L,
                                                                                                   6L), .Dimnames = list(c("e", "f", "g", "h", "i", "j"), NULL))
    )
    expect_equal(corner(tm4, "tr"),
                 structure(c(41L, 42L, 43L, 44L, 45L, 46L, 51L, 52L, 53L, 54L,
                             55L, 56L, 61L, 62L, 63L, 64L, 65L, 66L, 71L, 72L, 73L, 74L, 75L,
                             76L, 81L, 82L, 83L, 84L, 85L, 86L, 91L, 92L, 93L, 94L, 95L, 96L
                 ), .Dim = c(6L, 6L), .Dimnames = list(c("a", "b", "c", "d", "e",
                                                         "f"), c("E", "F", "G", "H", "I", "J")))
    )
    expect_equal(corner(tm4[FALSE, ], "tr"),
                 structure(integer(0), .Dim = c(0L, 6L), .Dimnames = list(NULL,
                                                                          c("E", "F", "G", "H", "I", "J")))
    )
    expect_equal(corner(tm4[, FALSE], "tr"),
                 structure(integer(0), .Dim = c(6L, 0L), .Dimnames = list(c("a",
                                                                            "b", "c", "d", "e", "f"), NULL))
    )
    expect_equal(corner(td1, "tr"),
                 structure(list(E = 41:46, F = 51:56, G = 61:66, H = 71:76, I = 81:86,
                                J = 91:96), .Names = c("E", "F", "G", "H", "I", "J"), row.names = c("a",
                                                                                                    "b", "c", "d", "e", "f"), class = "data.frame")
    )
    expect_equal(corner(td1[FALSE, ], n = 7),
                 structure(list(A = integer(0), B = integer(0), C = integer(0),
                                D = integer(0), E = integer(0), F = integer(0), G = integer(0)), .Names = c("A",
                                                                                                            "B", "C", "D", "E", "F", "G"), row.names = character(0), class = "data.frame")
    )
    expect_equal(corner(td1, "tl", n = c(3, 5)),
                 structure(list(A = 1:3, B = 11:13, C = 21:23, D = 31:33, E = 41:43), .Names = c("A",
                                                                                                 "B", "C", "D", "E"), row.names = c("a", "b", "c"), class = "data.frame")
    )
    expect_equal(corner(ta1, n = 2),
                 structure(c(1L, 2L, 11L, 12L, 101L, 102L, 111L, 112L), .Dim = c(2L,
                                                                                 2L, 2L))
    )
    expect_equal(corner(ta1, corner = "brb", n = 2),
                 structure(c(889L, 890L, 899L, 900L, 989L, 990L, 999L, 1000L), .Dim = c(2L,
                                                                                        2L, 2L))
    )
    expect_equal(corner(ta1, corner = "blf", n = 2:4),
                 structure(c(9L, 10L, 19L, 20L, 29L, 30L, 109L, 110L, 119L, 120L,
                             129L, 130L, 209L, 210L, 219L, 220L, 229L, 230L, 309L, 310L, 319L,
                             320L, 329L, 330L), .Dim = 2:4)
    )

  }
)
