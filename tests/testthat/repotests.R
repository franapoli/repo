
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
context("repository manipulation")
##############

rp1 <- repo_open(build_test_repo("repo1"))
wipe_test_repo("repo1")

test_that("copy normal item", {
    rp1$copy(rp2, "item 1")
    expect_that(
        bsf.dist(rep(1:10,each=3), rep(1:10,each=3), 10),
        equals(0)
    )
})


##############
context("chunks")
##############

src <- tempfile()

fcon <- file(src)
writeLines(c('print(1)',
             '## rpchunk test1 {',
             'print("2a")',
             'print("2b")',    
             ' ## rpchunk test1 }',
             '',
             'print(3)',
             '## rpchunk i1{',
             'print("4a")',
             'print("4b")',
             'print("4c")',
             '## rpchunk i1 }',
             '',
             'print(5)',
             '## rpchunk test3{',
             'print(6)',
             '## rpchunk test3}'),
           con=fcon)
close(fcon)

rp <- repo_open(build_test_repo("temp"))
rp$put(src, "src", "desc", "tag", asattach=T)
rp$put(1:3, "i1", "item", "tag", src="src")
rp$chunk("i1")

wipe_test_repo("temp")
