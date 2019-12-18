## Workflow
## library(devtools)
## library(testthat)

##############
context("global functions")
##############

unipath <- function(a, ...) {
    if(missing(...))
        return(normalizePath(a, mustWork=F))
    return(normalizePath(file.path(a, list(...)[[1]]), mustWork=F))
    }


build_test_repo <- function(subfolder)
{
    fold1 <- unipath(tempdir(), subfolder)
    rp1 <- repo_open(fold1, T)
    rp1$put(1:3, paste(subfolder, "item 1"), "description", "tags")
    return(rp1$root())
}


wipe_test_repo <- function(subfolder)
{
    fold <- unipath(tempdir(), subfolder)
    if(file.exists(fold))
        unlink(fold, recursive=T)
}


wipe_test_repo("repo1")
wipe_test_repo("repo2")
wipe_test_repo("temp")

##############
context("stash")
##############

rp <- repo_open(build_test_repo("repo1"))
stashed <- 1:3
rp$stash(stashed)
rp$stash(stashed, rename="renamed stashed")

test_that("stashed items added", {
    expect_true(rp$has("stashed"))
    expect_true(rp$has("renamed stashed"))
    expect_equal(rp$get("stashed"), rp$get("renamed stashed"))
    expect_equal(length(rp$entries()), 3)
})

rp$put("overwrite", "stashed")
rp$stash("renamed overwrite", "renamed stashed")

test_that("stashed item overwritten", {
    expect_equal(length(rp$entries()), 3)
    expect_equal(rp$get("stashed"), "overwrite")
    expect_equal(rp$get("renamed stashed"), "renamed overwrite")
})

rp$stashclear(force=T)

test_that("clearing stash", {
    expect_equal(length(rp$entries()), 2)
    expect_failure(expect_error(rp$put(NA, "renamed stashed")))
})

wipe_test_repo("repo1")


##############
context("lazydo")
##############

rp <- repo_open(build_test_repo("repo1"))

test_that("lazydo building and loading", {
    expect_message(rp$lazydo(2*10+80), ".*building.*")
    expect_equal(rp$get(names(rp$entries())[[2]]), 2*10+80)
    expect_message(cached <- rp$lazydo(2*10+80), ".*precomputed.*")
    expect_equal(cached, 2*10+80)
    expect_message(rp$lazydo(100), ".*building.*")
    expect_message(rp$lazydo(2*10+80, force=T), ".*building.*")
})

wipe_test_repo("repo1")


##############
context("repository and items creation")
##############

rp1 <- repo_open(build_test_repo("repo1"))
e <- rp1$entries()[[1]]
data <- rp1$print()
test_that("repository successfully created", {
    expect_equal(rp1$root(), unipath(tempdir(), "repo1"))
    expect_equal(length(e), 15)
    expect_true(file.exists(unipath(rp1$root(), e[["dump"]])))
    expect_true(rp1$has("repo1 item 1"))
    expect_equal(rp1$get("repo1 item 1"), 1:3)
    expect_equal(dim(data), c(3,1))
})


`unnamed var` <- 99
rp1$put(`unnamed var`)
test_that("Putting unnamed variable worked", {
    expect_true(rp1$has("unnamed var"))
    expect_equal(rp1$get("unnamed var"), 99)
})

rp1$set("repo1 item 1", description="edited")
test_that("edit entry", {
    expect_equal(rp1$entries()[["repo1 item 1"]]$description, "edited")
})

rp1$project("prj name", "prj desc")
rp1$options(prj="prj name")
rp1$put(1:3,"test","testdesc","tags")
rp1$put(4,"dependantItem","testdesc","tags", depends="test")

e2 <- rp1$entries()[["prj name"]]
e3 <- rp1$entries()[["test"]]

test_that("adding project", {
    expect_true(rp1$has("prj name"))
    expect_true(file.exists(unipath(rp1$root(), e2[["dump"]])))
    expect_true(rp1$has("test"))
    expect_true(file.exists(unipath(rp1$root(), e3[["dump"]])))
    expect_equal(rp1$entries()[["test"]]$prj, "prj name")
    expect_equal(rp1$entries()[["dependantItem"]]$depends, "test")
    expect_equal(rp1$depends("dependantItem"), "test")
})

env=environment()
rp1$load(c("test", "dependantItem"))
test_that("loading items to workspace", {
    expect_true("test" %in% ls(envir=env))
    expect_true("dependantItem" %in% ls(envir=env))
})

rp1$rm("test")
test_that("remove items", {
    expect_true(rp1$has("prj name"))
    expect_true(file.exists(unipath(rp1$root(), e2[["dump"]])))
    expect_false(rp1$has("test"))
    expect_false(file.exists(unipath(rp1$root(), e3[["dump"]])))
})

wipe_test_repo("repo1")


##############
context("test README code")
##############

rp <- repo_open(build_test_repo("repo1"))

rp$put(Inf, "God")
rp$put(0, "user")
rp$put(pi, "The Pi costant", depends="God")
rp$put(1:10, "r", depends="user")
diam <- 2 * rp$get("r")
circum <- 2 * rp$get("The Pi costant") * rp$get("r")
area <- rp$get("The Pi costant") * rp$get("r") ^ 2
rp$put(diam, "diameters", "These are the diameters", depends = "r")
rp$put(circum, "circumferences", "These are the circumferences",
       depends = c("The Pi costant", "r"))

fname <- unipath(tempfile())
fcon <- file(fname, "w")
writeLines("random text", con=fcon)
rp$attach(fname, "an attachment", to=c("circumferences", "diameters"))
close(fcon)    
rp$set("circumferences", src=basename(fname))
rp$put(area, "areas", "This are the areas",
       depends = c("The Pi costant", "r"), src=basename(fname))

m <- rp$dependencies(plot=F)

test_that("dependency matrix entries", {
    expect_equal(nrow(m), ncol(m))
    expect_equal(nrow(m), length(rp$entries()))
    expect_equal(sum(m), 7*1 + 2*2 + 2*3)
    expect_equal(m["The Pi costant", "God"], 1)
    expect_equal(m["r", "user"], 1)
    expect_equal(m["diameters", "r"], 1)
    expect_equal(m["diameters", "r"], 1)
    expect_true(all(m["circumferences", c("The Pi costant", "r")]==1))
    expect_true(all(m["areas", c("The Pi costant", "r")]==1))
    expect_true(all(m[basename(fname), c("circumferences", "diameters")]==2))
    expect_true(all(m[c("circumferences", "areas"), basename(fname)]==3))
})

rm(rp)
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
context("chunk basics")
##############

srccode <- '

rp <- repo_open(file.path(tempdir(), "temp"), T)
print(SRCNAME)
rp$put(SRCNAME, "src", "src desc", "tags", asattach=T)

## chunk "i1" {
print("Running chunk 1")
x <- 1
rp$put(x, "i1", "item", "tag", src="src")
## }

## chunk "i2"{
print("Running chunk 2")
y <- x+1
rp$put(y, "i2", "item", "tag", src="src", depends="i1")
## }

## chunk "i3"{
print("Running chunk 3")
z <- x+y
rp$put(z, "i3", "item", "tag", src="src", depends=c("i1","i2"))
## }
'

SRCNAME <- unipath(tempfile())
fcon <- file(SRCNAME, "w")
message("Writing code:")
message(srccode)
writeLines(srccode, con=fcon)
close(fcon)
message(paste("File", SRCNAME, "created:", file.exists(SRCNAME)))

eval(parse(text=srccode))


expect_equal(x, 1)
expect_equal(y, 2)
expect_equal(z, 3)
expect_equal(rp$get("i1"), 1)
expect_equal(rp$get("i2"), 2)
expect_equal(rp$get("i3"), 3)



## overwriting objs in workspace and repo:
x <- y <- z <- 0
rp$set("i1", 0)
rp$set("i2", 0)
rp$set("i3", 0)

## rebuilding objects
rp$options(replace=T)
rp$build("i3", force=T)

test_that("obj and dependencies build", {
    expect_equal(x, 1)
    expect_equal(y, 2)
    expect_equal(z, 3)
    expect_equal(rp$get("i1"), 1)
    expect_equal(rp$get("i2"), 2)
    expect_equal(rp$get("i3"), 3)
})


context("file naming convention")
rp$put(1,"com1") ## reserved windows name
rp$put(2,"com2")
rp$put(3,"com3")

expect_equal(rp$get("com1"), 1)
expect_equal(rp$get("com2"), 2)
expect_equal(rp$get("com3"), 3)

expect_equal(rp$attr("com1", "path"), file.path(rp$root(), "_", "_"))
expect_equal(rp$attr("com2", "path"), file.path(rp$root(), "_", "_1"))
expect_equal(rp$attr("com3", "path"), file.path(rp$root(), "_", "_2"))

nm <- "item\\name\ndirty"
rp$put("x", nm)
expect_equal(rp$get(nm), "x")
expect_equal(rp$attr(nm, "path"), file.path(rp$root(), "i", "item_name_dirty"))

rp$put(1,"com3")



wipe_test_repo("temp")




## ##############
## context("chunk forks")
## ##############


## srccode <- '

## rp <- repo_open(file.path(tempdir(), "temp"), T)

## rp$options(active_forks = "ACTIVEFORKS")
## rp$put(SRCNAME, "src", "src desc", "tags", asattach=T)

## ## chunk "i1" {
## print("Running chunk 1")
## x <- 1
## rp$put(x, "i1", "item", "tag", src="src")
## ## }

## ## chunk "i2#fork1"{
## print("Running chunk 2")
## y <- x+1
## rp$put(y, "i2#fork1", "item", "tag", src="src", depends="i1")
## ## }

## ## chunk "i2#fork2"{
## print("Running chunk 2")
## y <- x+2
## rp$put(y, "i2#fork2", "item", "tag", src="src", depends="i1")
## ## }

## ## chunk "i3"{
## print("Running chunk 3")
## z <- x+y
## rp$put(z, "i3", "item", "tag", src="src", depends=c("i1","i2"))
## ## }
## '
## SRCNAME <- unipath(tempfile())
## fcon <- file(SRCNAME, open="w")
## srccode <- gsub("ACTIVEFORKS", "fork1", srccode, fixed=T)
## message("Writing code:")
## message(srccode)
## writeLines(srccode, con=fcon)
## close(fcon)
## message(paste("File", SRCNAME, "created:", file.exists(SRCNAME)))

## eval(parse(text=srccode))

## rp$options(replace=T)
## rp$build("i1")
## rp$build("i2#fork1")
## rp$build("i3")

## test_that("test fork1 execution", {
##     expect_equal(x, 1)
##     expect_equal(y, 2)
##     expect_equal(z, 3)
##     expect_equal(rp$get("i1"), 1)
##     expect_equal(rp$get("i2"), 2)
##     expect_equal(rp$get("i3"), 4)
## })


## ## rebuilding objects
## rp$options(replace=T, active_forks="fork2")
## rp$build("i3", force=T)

## test_that("test fork2 execution", {
##     expect_equal(x, 1)
##     expect_equal(y, 3)
##     expect_equal(z, 4)
##     expect_equal(rp$get("i1"), 1)
##     expect_equal(rp$get("i2"), 3)
##     expect_equal(rp$get("i3"), 4)
## })


## wipe_test_repo("temp")
