test_that("myncurve works", {
  l <- myncurve(1,1,1)
  expect_equal(l$mu,1)
  expect_equal(l$sigma,1)
  expect_equal(l$area,0.5)
})
