
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
writeLines(c("print(1)",
             "## rpchunk test1 {",
             "print(2a)",
             "print(2b)",    
             " ## rpchunk test1 }",
             "",
             "print(3)",
             "## rpchunk test2{",
             "print(4a)",
             "print(4b)",
             "print(4c)",
             "## rpchunk test2 }",
             "",
             "print(5)",
             "## rpchunk test3{",
             "print(6)",
             "## rpchunk test3}"),
           con=fcon)
close(fcon)

rp <- repo_open(build_test_repo("temp"))
rp$put(src, "src", "desc", "tag", asattach=T,replace=T)
rp$put(1:3, "i1", "item", "tag", src="src")

srcfile <- rp$attr("i1","srcfile")
txt <- readLines(srcfile, warn=F)

s0 <- "[[:space:]]*"
s1 <- "[[:space:]]+"
chtag <- "rpchunk"
chname <- "([[:alnum:]]+)"
chopen <- "\\{"
chclose <- "\\}"
startstr <- paste0("^", s0, "#+", s0, chtag, s1, chname, s0, chopen,  s0, "$")
endstr   <- paste0("^", s0, "#+", s0, chtag, s1, chname, s0, chclose, s0, "$")


oks <- which(sapply(txt, grepl, pattern=startstr, perl=T))
tagss <- sapply(txt[oks], sub, pattern=startstr, replacement="\\1", perl=T)
names(oks) <- tagss
if(any(duplicated(tagss)))
    stop(paste("Chunk names are not unique: ",
               unique(tagss[duplicapted(tagss)])))

oke <- which(sapply(txt, grepl, pattern=endstr, perl=T))
tagse <- sapply(txt[oke], sub, pattern=endstr, replacement="\\1", perl=T)
names(oke) <- tagse

chunks <- vector("character", length(tagss))
for(i in 1:length(tagss))
{
    tag <- tagss[i]
    if(!(tag %in% tagse))
        stop(paste(tag, "has no matching end"))
    if(sum(tagse==tag)>1)
        stop(paste(tag, "has more than 1 matching end"))
    if(oke[tag]<oks[tag])
        stop(paste(tag, "has end line before start line"))
    chunks[i] <- paste(txt[(oks[tag]+1):(oke[tag]-1)], collapse="\n")
}

regexpr(, x[[2]], perl=T)
x <- readLines(srcfile,warn=F)
wipe_test_repo("temp")
