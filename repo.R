
## TODOs:
## [ ] Add general repo info at "info" without arguments
## [X] Add optional confirmation to runwithtags
##     [X] Disable confirmation in print
## [ ] Add a function for each object (loads as default)
##     id <- "item1"; assign(id, function(f="get") a[[f]](name=id))
## [ ] Attachments
##     [ ] Attach files
##     [ ] Attach to resource
##     [ ] Manage attachments in print
##     [ ] Manage export attachments
##     [ ] Open attachments
##     [ ] Export
##     [ ] Import
## [ ] Text resource:
##     [ ] Append
##     [ ] Source
## [+] Multiple repos
##     [+] Manage concurrency
##         [X] Make repo_close()
##         [ ] Use MD5
##     [ ] Copy to other repo
## [X] Manage special flags ("hide")
## [ ] Check environments (are the "get"-s and "assign"-s necessary?)
## [X] Set entry details
## [X] Show entry details
## [X] Adjust print: convert to data frame
## [X] Refactor: call "load" "get" and "add" "put"
## [ ] Externalize methods ("me" list)
## [ ] Check the use of "message" (maybe replace with "cat")
## [ ] S3-style methods and documentation
##     [ ] repo_open
##     [ ] check
##     [ ] tags
##     [ ] print
##     [ ] export
##     [ ] unlock
##     [ ] rm
##     [ ] get
##     [ ] entries
##     [ ] set
##     [ ] put
##     [ ] root


library(digest) # digest
library(tools) # md5sum
source("~/git/bbuck/repo/repoS3.R")

#' Open an existing repository or create a new one.
#' 
#' @param root Path to store data in.
#' @return An object of class Repo.
#' @examples
#' repo <- open.repo()
repo_open <- function(root="~/.R_repo")
{
    instID <- format(as.numeric(Sys.time()), digits=20)

    "+" = function(x,y) {
        if(is.character(x) | is.character(y)) {
            return(paste(x , y, sep=""))
        } else {
            .Primitive("+")(x,y)
        }
    }

    getEntry <- function(name) {
            e <- findEntryIndex(name)
            if(is.null(e))
                return(invisible(NULL))
            
            return(entries[[e]])
        }

    runWithTags <- function(f, tags, askconfirm=T, ...) {
            e <- findEntries(tags)
            if(length(e)<1)
                stop("Tags do not match any entry.")
            entr <- entries
            names <- sapply(entr[e], get, x="name")
            if(askconfirm) {
                message(paste0("Matched entries: ",
                               paste(names, collapse=", ")))
                n <- readline("Type \"yes!\" to confirm: ")
            } else n <- "yes!"
            if(n == "yes!") {
                for(i in 1:length(e))
                    get("this", thisEnv)[[f]](name=names[[i]], ...)
                return(invisible())
            } else
                {
                    message("Nothing done.")
                    return(invisible())
                }
        }

    
    hmnRead <- function(bytes) {
        values <- c(
            bytes,
            bytes/2^10,
            bytes/2^20,
            bytes/2^30,
            bytes/2^40,
            bytes/2^50
            )
        names(values) <- c("B","KB", "MB", "GB", "TB", "PB")
        okvals <- values[values>1]
        m <- okvals[which.min(okvals)] ## preserves name
        final <- format(round(m,2), scientific=F)
        return(paste0(final,names(m)))
        }
    
    lockIndex <- function() {
            lockfile <- file(file.path(root, paste0(REPOFNAME, ".lock")))
            writeLines(lockfile, text=instID)
            close(lockfile)
        }

    unlockIndex <- function() {
            unlink(lockFile())
        }

    isIndexLockedByOthers <- function() {
        lfilepath <- lockFile()
        if(!file.exists(lfilepath))
            return(F)
        lockfile <- file(lfilepath)
        return(readLines(lockfile) != instID)
    }

    lockFile <- function()
        { return(file.path(root, paste0(REPOFNAME, ".lock"))) }

    checkSafeAction <- function() {
        if(isIndexLockedByOthers())
            stop("Repo is locked. Please close other instances of this Repo." +
                 "If you know that there are no other instances currently open, " +
                 " You can manually remove the lock in " + lockFile() + ".")
        lockIndex()
    }
    
    storeIndex <- function() {
        if(is.null(repofile))
            stop("Repo is closed.")
        saveRDS(entries, repofile)
        }

    buildpath <- function(resname)
        {
            resname <- digest(resname)
            return(list(root,
                        substr(resname, 1, 2),
                        substr(resname, 3, 4),
                        substr(resname, 5, 6),
                        resname))
        }

    checkName <- function(name)
        {
            if(length(entries)<1)
                return(T)
            names <- sapply(entries, get, x="name")
            return(!(name %in% names))
        }

    storeData <- function(name, obj)
        {
            opath <- buildpath(name)
            if(!file.exists(do.call(file.path, opath[1:2])))
                dir.create(do.call(file.path, opath[1:2]))
            if(!file.exists(do.call(file.path, opath[1:3])))
                dir.create(do.call(file.path, opath[1:3]))
            if(!file.exists(do.call(file.path, opath[1:4])))
                dir.create(do.call(file.path, opath[1:4]))

            fpath <- do.call(file.path, opath)
            saveRDS(obj, fpath)
            return(list(path=fpath, size=file.info(fpath)$size))
        }

    findEntryIndex <- function(name)
        {
            if(is.null(entries) | length(entries)<1) {
                message("Repo is empty.")
                return(NULL)
            }
            names <- sapply(entries, get, x="name")
            w <- match(name, names)
            if(length(w)<1){
                message("Entry not found.")
                return(NULL)
            }
            return(w)
        }

    findEntries <- function(tags)
        {
            tagsets <- lapply(entries, get, x="tags")
            w <- sapply(tagsets, function(x)all(tags %in% x))
            return(which(w))
        }
    
    compressPath <- function(path)
        {
            hp <- path.expand("~")
            return(gsub(paste0("^",hp), "~", path))
        }
    
    cutString <- function(text, len, dotsafter=T)
        {
            if(nchar(text) == len)
                return(text)
            if(nchar(text) < len)
                return(format(text, width = len))
            if(dotsafter) {
                text <- substr(text, 1, len-3)
                return(paste0(text ,"..."))
            } else {
                text <- substr(text, nchar(text)-(len-4), nchar(text))
                return(paste0("...", text))
            }
            
        }
    
    root <- normalizePath(root)
    REPOFNAME <- "R_repo.RDS"
    repofile <- file.path(root, REPOFNAME)
    thisEnv <- environment()
    
    if(file.exists(repofile))
        {
            message(paste0("Found repo index in \"",
                           repofile, "\"."))
            assign("entries", readRDS(repofile), thisEnv)
        }

    if(!file.exists(root))
        {
            message(paste0(
                "Repo root (\"", get("root",thisEnv), "\") does not exist. Create it?"))
            n <- readline("Type \"yes\" to proceed: ")
            if(tolower(n) == "yes") {
                dir.create(root)
                message("New repo created.")
            } else message("Nothing done.")
        }

    me <- list(
        check = function()
        {
            entr <- entries
            for(i in 1:length(entr))
                {
                    cat(paste0("Checking ", entr[[i]]$name, "..."))
                    md5s <- md5sum(path.expand(entr[[i]]$dump))
                    if(md5s != entr[[i]]$checksum) {
                        cat(" changed!")
                        warning("File has changed!")
                    } else cat(" ok.")
                    cat("\n")
                }
            allfiles <- file.path(root, list.files(root, recursive=T))
            dumps <- sapply(entr, get, x="dump")            
            junk <- setdiff(path.expand(allfiles), path.expand(dumps))
            if(length(junk)>0){
                cat("\nThe following files in Repo root have no associated Repo entry:\n")
                cat(paste(junk, collapse="\n"))
            }
            cat("\n")
            invisible()
        },

        allunga = function(env = parent.frame())
        {
            print(ls())
            print(ls(envir=thisEnv))
            me <- get("this", thisEnv)
            me[[length(me)+1]] <- ls
            print(names(me))
            assign("this", me, envir=thisEnv)
        },
        
        tags = function()
        {            
            entr <- entries
            tagset <- unique(unlist(lapply(entr, get, x="tags")))
            return(tagset)
        },

        close = function()
        {
            assign("repofile", NULL, envir=thisEnv)
            assign("entries", NULL, envir=thisEnv)
            unlockIndex()
            message("Repo closed, call open_repo again to reinitialize.")
        },

        print = function(tags=NULL, showhidden=F)
        {
            entr <- entries
            if(length(entr)<1)
                {
                    message("Repository is empty.")
                    return()
                }

            if(!is.null(tags)) {
                w <- findEntries(tags)
                if(length(w)<1)
                    {
                        message("Tags not found.")
                        return(invisible())
                    } else {
                        entr <- entr[w]
                    }
            }

            labels <- c("ID", "Dims", "Tags", "Size")
            names <- sapply(entr, get, x="name")

            a <- matrix(NA, length(names), length(labels))
            colnames(a) <- labels
            rownames(a) <- 1:length(entr)

            tagsets <- sapply(entr, get, x="tags")
            
            a[,"ID"] <- names
            a[,"Dims"] <- sapply(sapply(entr, get, x="dims"), paste, collapse="x")
            a[,"Tags"] <- sapply(tagsets, paste, collapse=", ")
            a[,"Size"] <- sapply(sapply(entr, get, x="size"), hmnRead)

            w <- sapply(tagsets, is.element, el="hide")
            if(showhidden)
                w[w] <- F
            
            print(a[!w,], quote=F)
            
        },

        export = function(name, where=".", tags=NULL)
        {
            if(!xor(missing(name),missing(tags)))
                stop("You must specify eiterh names or tags.")

            if(!is.null(tags)){
                runWithTags("export", tags, where)
            } else {
                ipath = do.call(file.path, buildpath(name))
                file.copy(ipath, file.path(where, paste0(name, ".RDS")))
            }
        },
        
        info = function(name = NULL, tags = NULL)
        {
            if(!xor(missing(name),missing(tags)))
                stop("You must specify either a name or a set of tags.")

            if(!is.null(tags)){
                runWithTags("info", tags, askconfirm=F)
            } else {            
                                        #e <- get("findEntryIndex",thisEnv)(name)
                e <- findEntryIndex(name)
                if(is.null(e))
                    stop("Identifier not found.")

                labels <- c("ID:", "Description:", "Tags:",
                            "Dimensions:", "Timestamp:", "Size on disk:",
                            "Provenance:", "Stored in:", "MD5 checksum:")
                maxlen <- max(sapply(labels, nchar))

                vals <- c(entries[[e]]$name, entries[[e]]$description,
                          paste0(entries[[e]]$tags, collapse=", "),
                          paste(entries[[e]]$dims, collapse="x"),
                          as.character(entries[[e]]$timestamp), hmnRead(entries[[e]]$size),
                          entries[[e]]$source, entries[[e]]$dump,
                          entries[[e]]$checksum)
                cat(paste0(format(labels, width=maxlen+1), vals, "\n"), sep="")
                cat("\n")
            }
        },

        rm = function(name = NULL, tags = NULL)        
        {
            checkSafeAction()
            
            if(!xor(missing(name),missing(tags)))
                stop("You must specify either a name or a set of tags.")

            if(!is.null(tags)){
                runWithTags("rm", tags)
            } else {            
                e <- get("findEntryIndex",thisEnv)(name)
                if(is.null(e))
                    return(invisible(NULL))

                file.remove(entries[[e]]$dump)
                assign("entries", entries[-e], thisEnv)                
                storeIndex()
            }
        },

        get = function(name)
        {
            if(checkName(name))
                stop("ID not found.")
            entry <- getEntry(name)
            data <- readRDS(entry$dump) 
            
            return(data)
        },
        
        load = function(name)
        {
            warning("load is deprecated, use get.")
            return(get("this", thisEnv)$get(name))
        },
        
        entries = function()
        {
            return(get("entries",thisEnv))
        },

        set = function(name, obj=NULL, newname=NULL, description=NULL, tags=NULL, src=NULL)
        {
            checkSafeAction()
            
            if(missing(name) | (missing(newname) & missing(obj) &
                                missing(description) & missing(tags) & missing(src)))
                stop("You must provide name and one of: obj, description, tags, src.")
            if(checkName(name))
                stop("Identifier not found.")

            w <- findEntryIndex(name)
            entr <- entries[[w]]

            entr$timestamp <- Sys.time()
            if(!is.null(newname))
                entr$name <- newname
            if(!is.null(description))
                entr$description <- description
            if(!is.null(tags))
                entr$tags <- tags
            if(!is.null(src))
                entr$src <- newname

            if(!is.null(obj)) {
                if(!is.null(dim(obj)))
                    dims <- dim(obj) else dims <- length(obj)
                
                if(!is.null(newname))
                    file.remove(entr$dump)
                
                fdata <- get("storeData", thisEnv)(entr$name, obj)
                entr$class <- class(obj)
                entr$dump <- fdata[["path"]]
                entr$size <- fdata[["size"]]
                entr$checksum <- md5sum(path.expand(fdata[["path"]]))
                entr$dims <- dims
            }
            entries[[w]] <- entr
            assign("entries", entries, thisEnv)
        },

        add = function(obj, name, description, tags, src=NULL, force_replace=F)
        {
            warning("add is deprecated, use put.")
            return(get("this", thisEnv)$put(obj, name, description, tags, src, force_replace))
        },
        
        put = function(obj, name, description, tags, src=NULL, force_replace=F)
        {
            checkSafeAction()
            
            if(missing(obj) | missing(name) | missing(description) | missing(tags))
                stop("You must provide all of: obj, name, description, tags.")
            
            notexist <- checkName(name)
            if(!notexist & !force_replace)
                {                    
                    message("Identifier already used.")
                    return(invisible(NULL))
                }

            if(!notexist & force_replace)
                get("this", thisEnv)$rm(name)

            if(!is.null(dim(obj)))
                dims <- dim(obj) else
            dims <- length(obj)

            fdata <- get("storeData", thisEnv)(name, obj)
            fname <- fdata[["path"]]
            fsize <- fdata[["size"]]

            if(is.null(src)) {
                storedfrom <- getwd()
            } else storedfrom <- src

            repoE <- list(name = name,
                          description = description,
                          tags = tags,
                          class = class(obj),
                          dims = dims,
                          timestamp = Sys.time(),
                          dump = fname,
                          size = fsize,
                          checksum = md5sum(path.expand(fname)),
                          source = storedfrom)

            entr <- get("entries", thisEnv)           
            entr[[length(entries)+1]] <- repoE
            assign("entries", entr, thisEnv)
            get("storeIndex", thisEnv)()
        },

        test=function()
        {
            print(ls(envir=thisEnv))
        },

        append = function(id, text)
        {
            checkSafeAction()
            
            notexist <- checkName(name)
            if(notexist)
                stop("Identifier not found.")

            e <- findEntryIndex(id)
            ## need class
        },        
        
        root = function()
        {
            return(get("root",thisEnv))
        }

        )


    
    assign('this', me, envir=thisEnv)
    class(me) <- append(class(me),"repo")

    if(!file.exists(repofile)) ## is this check needed?
        {
            saveRDS(get("entries", thisEnv), repofile)
        }
    
    return(me)
}



repo_test <- function(where = "./repotest")
    {
        f <- file.path(where)
        repo <- repo_open(f)
        repo$put(1:10, "item1", "item 1 description", c("tag1", "tag2"))
        repo$put(1:100, "item2", "item 2 description", c("tag2", "tag3"))
        repo$put(matrix(runif(100*100), 100, 100), "item3", "item 3 description", c("tag1", "tag2", "tag3"))
        repo$put(list(1,2,3,4,5), "item4", "item 4 description", c("tag1"))
        repo$put("string", "item5", "item 5 description", c("tag2"))
        return(repo)
    }
