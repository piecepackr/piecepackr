test_that("Some affine transformations that earlier attempts failed on", {
skip_on_cran()
# upper right pyramid face
df <- data.frame(x = c(5.66061867341471, 4.96693859488835, 4.51615802188193, 5.20983810040828),
                 y = c(5.20983810040828, 4.51615802188193, 4.96693859488835, 5.66061867341471))
vpi <- at_vp_info(df)
expect_true(vpi$flipped)
expect_equal(vpi$angle, -45)

# left pyramid face
df <- data.frame(x = c(0.548321978113589, 1.63846635232886, 1.63179192889832, 0.541647554683048),
                     y = c(3.20646280984155, 3.67766227203477, 3.44151334764832, 2.97031388545509))
vpi <- at_vp_info(df)
expect_true(vpi$flipped)
expect_equal(vpi$angle, 88.381045)

# lower-left pyramid left
df <- data.frame(x = c(0.993133639601288, 1.58950508250181, 1.23595169190854, 0.639580249008014),
                     y = c(0.639580249008014, 1.23595169190854, 1.58950508250181, 0.993133639601288))
vpi <- at_vp_info(df)
expect_false(vpi$flipped)
expect_equal(vpi$angle, 135)

# bottom pyramid back
df <- data.frame(x = c(2.99330183384669, 3.37276334764832, 3.56293637525157, 3.18347486144994),
                     y = c(0.64809266481788, 1.52599654139832, 1.5313715195592, 0.653467642978757))
vpi <- at_vp_info(df)
expect_true(vpi$flipped)
expect_equal(vpi$angle, -178.381045)

# lower-left pyramid back
df <- data.frame(x = c(0.842511262411246, 1.23595169190854, 1.18364305569535, 0.790202626198057),
                 y = c(0.842511262411246, 1.58950508250181, 1.53719644628862, 0.790202626198057))
vpi <- at_vp_info(df)
expect_false(vpi$flipped)
expect_equal(vpi$angle, -135)
})
