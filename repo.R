library(digest) # digest
library(tools) # md5sum

#' Open an existing repository or create a new one.
#' 
#' @param root Path to store data in.
#' @return An object of class Repo.
#' @examples
#' repo <- open.repo()
repo_open <- function(root="~/.R_repo")
{
    getEntry <- function(name)
        {            
            e <- get("findEntryIndex",thisEnv)(name)
            if(is.null(e))
                return(invisible(NULL))
            
            return(entries[[e]])
        }

    runWithTags <- function(f, tags, ...)
        {
            e <- findEntries(tags)
            if(length(e)<1)
                stop("Tags do not match any entry.")
            entr <- get("entries",thisEnv)
            names <- sapply(entr[e], get, x="name")
            message(paste0("Matched entries: ",
                           paste(names, collapse=", ")))
                n <- readline("Type \"yes!\" to confirm: ")
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
    
    hmnRead <- function(bytes)
        {
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
    
    lockIndex <- function()
        {
            if(!isIndexLocked())
                {
                    lockfile <- file.path(root, paste0(REPOFNAME, ".lock"))
                    file.create(lockfile)
                }
        }

    unlockIndex <- function()
        {
            if(isIndexLocked())
                {
                    lockfile <- file.path(root, paste0(REPOFNAME, ".lock"))
                    invisible(file.remove(lockfile))
                }
        }

    isIndexLocked <- function()
    {
        lockfile <- file.path(root, paste0(REPOFNAME, ".lock"))
        return(file.exists(lockfile))
    }
    
    storeIndex <- function()
        {
            if(isIndexLocked())
                stop("Repo index is locked")

            fname <- get("repofile", thisEnv)
            lockIndex()
            saveRDS(get("entries", thisEnv), fname)
            unlockIndex()
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
            entr <- get("entries", thisEnv)
            if(length(entr)<1)
                return(T)
            names <- sapply(entr, get, x="name")
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
            entr <- get("entries", thisEnv)
            if(is.null(entr) | length(entr)<1) {
                message("Repo is empty.")
                return(NULL)
            }
            names <- sapply(entr, get, x="name")
            w <- match(name, names)
            if(length(w)<1){
                message("Entry not found.")
                return(NULL)
            }
            return(w)
        }

    findEntries <- function(tags)
        {
            entr <- get("entries", thisEnv)
            tagsets <- lapply(entr, get, x="tags")
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

    REPOFNAME <- "R_repo.RDS"
    rsource <- NULL
    thisEnv <- environment()
    assign("defTags", NULL)
    assign("entries", NULL, thisEnv)      
    repofile <- file.path(root, REPOFNAME)
    assign("repofile", repofile, thisEnv)
    
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
                    cat(paste0("checking ", entr[[i]]$name, "..."))
                    md5s <- md5sum(path.expand(entr[[i]]$dump))
                    if(md5s != entr[[i]]$checksum)
                        warning("File has changed!") else cat(" ok!")
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
        
        tags = function()
        {            
            entr <- entries
            tagset <- unique(unlist(lapply(entr, get, x="tags")))
            return(tagset)
        },

        print = function(tags=NULL)
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

            widths <- c(14,14,19,19,10)

            message(paste(
                cutString("Name", widths[1]),
                cutString("Dims", widths[2]),
                cutString("Tags", widths[3]),
                cutString("From", widths[4]),
                cutString("Size", widths[5]),
                collapse=" "))
            
            for(i in 1:length(entr))
                {
                    entri <- entr[[i]]
                    message(
                        paste(
                            cutString(entri$name, widths[1]),
                            ## cutString(paste(
                            ##     gsub(" ", "", gsub("B", "", humanReadable(entri$dims, standard="SI", sep="",digits=0))),
                            ##     collapse=" x "), widths[2]),
                            cutString(paste(entri$dims, collapse="x"), widths[2]),
                            cutString(paste(entri$tags, collapse=", "), widths[3]),
                            cutString(compressPath(entri$source), widths[4], F),
                            cutString(hmnRead(entri$size) , widths[5]),
                            collapse=" "
                            )
                        )
                }

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
        
        unlock = function()
        {
            get("unlockIndex", thisEnv)()
        },

        rm = function(name = NULL, tags = NULL)        
        {
            if(!xor(missing(name),missing(tags)))
                stop("You must specify eiterh a name or tags.")

            if(!is.null(tags)){
                dotags <- runWithTags("rm", tags)
            } else {            
                e <- get("findEntryIndex",thisEnv)(name)
                if(is.null(e))
                    return(invisible(NULL))
            
                entr <- entries
                assign("entries", entr[-e], thisEnv)
            
                ipath = do.call(file.path, buildpath(name))
                file.remove(ipath)

                get("storeIndex", thisEnv)()
            }
        },

        load = function(name)
        {
            entry <- getEntry(name)
            data <- readRDS(entry$dump) 
            
            return(data)
        },
                
        entries = function()
        {
            return(get("entries",thisEnv))
        },
        
        add = function(obj, name, description, tags, src=NULL, force_replace=F)
        {
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
            flags <- c(get("defTags", thisEnv), tags, class(obj))

            fdata <- get("storeData", thisEnv)(name, obj)
            fname <- fdata[["path"]]
            fsize <- fdata[["size"]]

            if(is.null(src)) {
                if(is.null(rsource))
                    storedfrom <- getwd() else
                storedfrom <- rsource
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

#' Show a summary of the repository contents.
#' 
#' @return Used for side effects.
print.repo <- function(repo, tags=NULL) repo$print(tags)

#' Add an R object to the repository.
#' 
#' @param repo An object of cazz Repo.
#' @param obj The R object to store in the repository.
#' @param id The name of the R object to store.
#' @param description An extended description of the R object
#' @return Used for side effects.
#' @examples
#' repo <- open.repo()
#' add.obj(repo, cars, "cars", "R cars dataset")
#' identical(repo$load("cars"), cars)
repo_add <- function(repo, obj, id, description, src, tags) repo$add(obj, id, description, src, tags)

#' Open an existing repository or create a new one.
#' 
#' @param repo An object of class Repo.
#' @param id The identifier of the object to load.
#' @examples
#' repo <- open.repo()
#' add.obj(repo, cars, "cars", "R cars dataset")
#' identical(repo$load("cars"), cars)
repo_load <- function(repo, id) repo$load(id)

#' Open an existing repository or create a new one.
#' 
#' @param An object of class Repo.
#' @examples
#' repo <- open.repo()
#' add.obj(repo, cars, "cars", "R cars dataset")
#' print(repo)
#' repo$rm("cars")
#' print(repo)
repo_rm <- function(repo, id) repo$rm(id)


createTestRepo <- function()
    {
        repo <- open.repo("testrepo")
        repo$add(1:10, "item1", "item 1 description", c("tag1", "tag2"))
        repo$add(1:100, "item2", "item 2 description", c("tag2", "tag3"))
        repo$add(matrix(runif(100*100), 100, 100), "item3", "item 3 description", c("tag1", "tag2", "tag3"))
        repo$add(list(1,2,3,4,5), "item4", "item 4 description", c("tag1"))
        repo$add("string", "item5", "item 5 description", c("tag2"))
        return(repo)
    }
