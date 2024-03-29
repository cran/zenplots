library(zenplots)
library(testthat)
pdf(NULL)

test_that("Basics", {
  ### Generate some data #########################################################

  n <- 1000 # sample size
  d <- 20 # dimension
  set.seed(271) # set seed (for reproducibility)
  x <- matrix(rnorm(n*d), ncol = d) # i.i.d. N(0,1) data
  ### Understanding the layout of zenplots #######################################

  ## Auxiliary function
  correctly_fails <- function(x)
    is(tryCatch(x, error = function(e) e), "simpleError")

  ## Loop over packages
  for(pkg in c("graphics", "grid")) {

    ## Call zenplot() in various cases
    ## Zero 2d plots (1st plot on the path)
    expect_true(correctly_fails(zenplot(x[,1], n2dcols = 0, pkg = pkg)))
    z1d <- zenplot(x[,1], n2dcols = 1, pkg = pkg) # 1d
    expect_is(z1d, "zenplot")
    expect_true(correctly_fails(zenplot(x[,1], n2dcols = 1, last1d = FALSE,
                                        pkg = pkg))) # 1d
    expect_true(correctly_fails(zenplot(x[,1], n2dcols = 1, first1d = FALSE,
                                        last1d = FALSE, pkg = pkg))) # 1d

    ## One 2d plot (2nd/3rd plot on the path)
    z2d <- zenplot(x[,1:2], n2dcols = 1, last1d = FALSE, pkg = pkg) # 1d, 2d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:2], n2dcols = 1, first1d = FALSE, last1d = FALSE, pkg = pkg) # 1d, 2d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:2], n2dcols = 1, pkg = pkg) # 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    ## Note:
    ## - The size of the plotted points for 'grid' correspond to roughly
    ##   1/1.6 times the size for 'graphics'.
    ## - To (always) get the plot symbol/text size right would depend on both the
    ##   number of rows and columns of (2d) plots (and is omitted here; manually
    ##   fine-tune these sizes if required)

    ## Two 2d plots (4th/5th plot on the path)
    z2d <- zenplot(x[,1:3], n2dcols = 2, last1d = FALSE, pkg = pkg) # 1d, 2d, 1d, 2d
    expect_is(z2d, "zenplot")
    expect_true(correctly_fails(zenplot(x[,1:3], n2dcols = 1, pkg = pkg))) # 1d, 2d, 1d, 2d, 1d
    expect_true(correctly_fails(zenplot(x[,1:3], n2dcols = 1, method = "double.zigzag", pkg = pkg))) # 1d, 2d, 1d, 2d, 1d
    expect_true(correctly_fails(zenplot(x[,1:3], n2dcols = 1, method = "single.zigzag", pkg = pkg))) # 1d, 2d, 1d, 2d, 1d
    z2d <- zenplot(x[,1:3], n2dcols = 2, pkg = pkg) # 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:3], n2dcols = 2, method = "double.zigzag", pkg = pkg) # 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:3], n2dcols = 2, method = "single.zigzag", pkg = pkg) # 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    ## Three 2d plots (6th/7th plot on the path)
    z2d <- zenplot(x[,1:4], n2dcols = 2, first1d = FALSE, pkg = pkg) # 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:4], n2dcols = 2, last1d = FALSE, pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:4], n2dcols = 2, first1d = FALSE, last1d = FALSE, pkg = pkg) # 2d, 1d, 2d, 1d, 2d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:4], n2dcols = 2, pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:4], n2dcols = 2, method = "double.zigzag", pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:4], n2dcols = 2, method = "single.zigzag", pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    ## Four 2d plots (8th/9th plot on the path)
    z2d <- zenplot(x[,1:5], n2dcols = 2, last1d = FALSE, pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d, 2d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:5], n2dcols = 2, pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:5], n2dcols = 3, pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:5], n2dcols = 3, method = "double.zigzag", pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:5], n2dcols = 3, method = "single.zigzag", pkg = pkg) # 1d, 2d, 1d, 2d, 1d, 2d, 1d, 2d, 1d
    expect_is(z2d, "zenplot")
    ## Large sub-data sets
    z2d <- zenplot(x[,1:18], n2dcols = 5, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:18], n2dcols = 5, method = "double.zigzag", pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:18], n2dcols = 5, method = "single.zigzag", pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x[,1:19], n2dcols = 5, pkg = pkg)
    expect_is(z2d, "zenplot")
    ## The full data with different n2dcols
    z2d <- zenplot(x, n2dcols = 4, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 5, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 6, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 7, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 8, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 9, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 10, pkg = pkg)
    expect_is(z2d, "zenplot")
    z2d <- zenplot(x, n2dcols = 11, pkg = pkg)
    expect_is(z2d, "zenplot")
  }


  ### Examples with predefined plot1d() or plot2d() ##############################

  zplot <- zenplot(x, plot1d = "label", plot2d = "rect")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = "label", plot2d = "rect", pkg = "grid")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = "arrow", plot2d = "rect")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = "arrow", plot2d = "rect", pkg = "grid")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = "arrow", plot2d = "arrow")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = "arrow", plot2d = "arrow", pkg = "grid")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = NULL) # omitting the 1d plots
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot1d = NULL, pkg = "grid")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot2d = NULL) # omitting the 2d plots
  expect_is(zplot, "zenplot")
  zplot <- zenplot(x, plot2d = NULL, pkg = "grid")
  expect_is(zplot, "zenplot")

  ### Iterate over all predefined plot1d() #######################################

  for(p1d in eval(formals(zenplot)$plot1d)) {
    zplot <- zenplot(x, plot1d = p1d) # with graphics
    expect_is(zplot, "zenplot")
    zplot <- zenplot(x, plot1d = p1d, pkg = "grid") # with grid
    expect_is(zplot, "zenplot")
  }


  ### Some missing data ##########################################################

  z <- x

  z[seq_len(n-10), 5] <- NA # all NA except 10 points
  zplot <- zenplot(z, plot1d = "rug")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(z, plot1d = "hist")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(z, plot1d = "density")
  expect_is(zplot, "zenplot")

  z[, 6] <- NA # all NA
  zplot <- zenplot(z, plot1d = "rug")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(z, plot1d = "hist")
  expect_is(zplot, "zenplot")
  zplot <- zenplot(z, plot1d = "density")
  expect_is(zplot, "zenplot")
})
