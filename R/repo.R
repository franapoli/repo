##
## Source code for Repo
##
## Francesco Napolitano, franapoli@gmail.com
##
## lines with ## ** have TODOs

#' Repo: The Data-centered Data Flow Manager
#'
#' @details
#' The Repo package is meant to help with the management of R data
#' files. It builds one (or more) centralized repository where R
#' objects are stored together with corresponding annotations, tags,
#' dependency notes, provenance traces. It also provides navigation
#' tools to easily locate and load previously stored resources.
#' 
#' Create a new repository with \code{rp <- repo_open()}.
#'
#' Given the object rp of class \code{repo}, the \code{repo} command
#' \code{foo} must be called like this: \code{rp$foo()}. However, the
#' public name of \code{foo} will be \code{repo_foo}, and this name
#' must be used to get help (\code{?repo_foo}).
#'
#' For a complete list of functions, use \code{library(help = "repo")}.
#' 
#' @docType package
#' @name repo-package
#' @author Francesco Napolitano \email{franapoli@@gmail.com}
#' @aliases repo
NULL

#' Open an existing repository or create a new one.
#'
#' If a repository does not exist at the specified location, creates a
#' directory and stores the repository index in it. If a repository
#' exists, the index is loaded and a \code{repo} object is built.
#' 
#' @param root Path to store data in. Defaults to "~/.R_repo".
#' @param force Don't ask for confirmation.
#' @return An object of class \code{repo}.
#' @name repo_open
#' @export
#' @examples
#' ## Creates a new repository in a temporary directory without asking for
#' ## confirmation.
#' rp_path <- file.path(tempdir(), "example_repo")
#' rp <- repo_open(rp_path, TRUE)
#' rp$put(0, "zero", "a random item", "a_tag")
#' rp$info()
#' ## wiping temporary repo
#' unlink(rp_path, TRUE)
NULL

## digest is used in lazydo and bulkedit
#'@import digest
## md5sum for checking stored data
#'@import tools
#'@importFrom graphics pie
#'@importFrom utils download.file packageVersion sessionInfo

globalVariables(c("entries", "thisEnv", "repofile", "root",
                  "indexMD5", "this"))

## repo uses "Local Environment Approach"
## (http://www.cyclismo.org/tutorial/R/s3Classes.html#s3classesmethodslocal)
## All repo objects are defined within the repo_open function. They
## include a list of "methods" that the user can call, and a number of
## functions only used within repo.
repo_open <- function(root=normalizePath(file.path("~",".R_repo"),
                                         mustWork=F), force=F)
{
    root <- normalizePath(root, mustWork=F)
    repofile <- file.path(root, "R_repo.RDS")

    thisEnv <- environment()    
    private_methods <- repo_methods_private()
    for(i in 1:length(private_methods)){
        environment(private_methods[[i]]) <- thisEnv
        thisEnv[[names(private_methods)[[i]]]] <- private_methods[[i]]
    }
    
    me <- repo_methods_public()
    for(i in 1:length(me))
        environment(me[[i]]) <- thisEnv
    class(me) <- append(class(me),"repo")
    assign('this', me, envir=thisEnv)
    assign('entries', list(), envir=thisEnv)
    assign('options', list(), envir=thisEnv)
    
    if(file.exists(repofile))
        {
            message(paste0("Found repo index in \"",
                           repofile, "\"."))
            assign("entries", readRDS(repofile), thisEnv)
        } else {

            if(!file.exists(root))
                {
                    if(force)
                        n <- "yes" else {
                            cat(paste0(
                                "Repo root \"", get("root",thisEnv),
                                "\" does not exist. Create it? "))
                            n <- readline("Type \"yes\" to proceed: ")
                        }
                    if(tolower(n) == "yes") {
                        dir.create(root)
                        message("Repo root created.")
                    } else { message("Nothing done."); return(invisible()) }
                }

            storeIndex()
            message("Repo created.")
        }

    indexMD5 <- md5sum(repofile)
    
    return(me)
}


## if(F)
##     {
##         library(repo)
##         fld <- tempdir()
##         rp <- repo_open(fld, T)
##         rp$put(1, "1", "1", "1")
##         rp$put(2, "2", "2", "2", src=1)
##         rp$put(3, "3", "3", "3", src=1, depends=c(1,2))
##         rp$put(4, "4", "4", "4", depends=3)
##         rp$put(5, "5", "5", "5")
##         rp$put(6, "6", "6", "6", depends=5)
##         pdf(file.path(fld, "temp.pdf"))
##         plot(runif(10))
##         dev.off()
##         rp$attach(file.path(fld, "temp.pdf"), "descr", "tag",replace=T, to=2)
##         rp$set("6",depends=c("4","5"))
        
##         rp2 <- repo_open(file.path(fld, "2"), T)
##         rp$get("temp.pdf")
##         rp$copy(rp2, "temp.pdf", replace=T)


##     }
