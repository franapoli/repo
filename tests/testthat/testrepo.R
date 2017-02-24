##############
context("global functions")
##############

build_test_repo <- function(subfolder)
{
    fold1 <- file.path(tempdir(), subfolder)
    rp1 <- repo_open(fold1, T)
    rp1$put(1:3, paste(subfolder, "item 1"), "description", "tags")
    return(rp1$root())
}


wipe_test_repo <- function(subfolder)
{
    fold <- file.path(tempdir(), subfolder)
    unlink(fold, recursive=T)
}


##############
context("repository and items creation")
##############

rp1 <- repo_open(build_test_repo("repo1"))
e <- rp1$entries()[[1]]
data <- rp1$print()
test_that("repository successfully created", {
    expect_equal(rp1$root(), file.path(tempdir(), "repo1"))
    expect_equal(length(e), 15)
    expect_true(file.exists(file.path(rp1$root(), e[["dump"]])))
    expect_true(rp1$has("repo1 item 1"))
    expect_equal(rp1$get("repo1 item 1"), 1:3)
    expect_equal(dim(data), c(3,1))
})

rp1$project("prj name", "prj desc")
rp1$options(prj="prj name")
rp1$put(1:3,"test","testdesc","tags")
##rp1$info("prj name")
e2 <- rp1$entries()[["prj name"]]
e3 <- rp1$entries()[["test"]]

test_that("adding project", {
    expect_true(rp1$has("prj name"))
    expect_true(file.exists(file.path(rp1$root(), e2[["dump"]])))
    expect_true(rp1$has("test"))
    expect_true(file.exists(file.path(rp1$root(), e3[["dump"]])))
    expect_equal(rp1$entries()[["test"]]$prj, "prj name")
})

rp1$rm("test")
test_that("remove items", {
    expect_true(rp1$has("prj name"))
    expect_true(file.exists(file.path(rp1$root(), e2[["dump"]])))
    expect_false(rp1$has("test"))
    expect_false(file.exists(file.path(rp1$root(), e3[["dump"]])))
})


wipe_test_repo("repo1")

##############
context("multiple repositories")
##############

rp1 <- repo_open(build_test_repo("repo1"))
rp2 <- repo_open(build_test_repo("repo2"))

rp1$copy(rp2, "repo1 item 1")

test_that("copy normal item", {

    expect_equal(
        names(rp2$entries()),
        c("repo2 item 1", "repo1 item 1")
    )
    expect_true(rp1$has("repo1 item 1"))
    expect_true(rp2$has("repo2 item 1"))
    expect_true(rp2$has("repo1 item 1"))
    expect_true(!rp2$has("puparuolo"))
    expect_true(!rp1$has("mulignana"))
})

wipe_test_repo("repo1")
wipe_test_repo("repo2")


##############
context("chunks")
##############

src <- tempfile()
fcon <- file(src)

srccode <- paste(
    c('rp <- repo_open(file.path(tempdir(), "temp"), T)',
      paste0('srcf <- "', src, '"'),
      'rp$put(srcf, "src", "src desc", "tags", asattach=T)',
      '## chunk "i1" {',
      'print("Running chunk 1")',
      'x <- 1',
      'rp$put(x, "i1", "item", "tag", src="src")',
      '## chunk "i1" }',
      '',
      '## chunk "i2"{',
      'print("Running chunk 2")',             
      'y <- x+1',
      'rp$put(y, "i2", "item", "tag", src="src", depends="i1")',
      '## chunk "i2" }',
      '',
      '## chunk "i3"{',
      'print("Running chunk 3")',             
      'z <- x+y',
      'rp$put(z, "i3", "item", "tag", src="src", depends=c("i1","i2"))',
      '## chunk "i3"}\n'),
    collapse="\n")

writeLines(srccode, con=fcon)
close(fcon)

source(src)

test_that("test source correctly loaded", {
    expect_equal(x, 1)
    expect_equal(y, 2)
    expect_equal(z, 3)
    expect_equal(rp$get("i1"), 1)
    expect_equal(rp$get("i2"), 2)
    expect_equal(rp$get("i3"), 3)
})


## overwriting objs in workspace and repo:
x <- y <- z <- 0
rp$set("i1", 0)
rp$set("i2", 0)
rp$set("i3", 0)

## rebuilding objects
rp$options(replace=T)
rp$build("i3")

test_that("obj and dependencies build", {
    expect_equal(x, 1)
    expect_equal(y, 2)
    expect_equal(z, 3)
    expect_equal(rp$get("i1"), 1)
    expect_equal(rp$get("i2"), 2)
    expect_equal(rp$get("i3"), 3)
})


wipe_test_repo("temp")
