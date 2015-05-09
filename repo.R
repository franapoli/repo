library(digest)
library(tools)

#' Open an existing repository or create a new one.
#' 
#' @param root Path to store data in.
#' @return An object of class Repo.
#' @examples
#' repo <- open.repo()
open.repo <- function(root="~/.R_repo")
{
    REPOFNAME <- "R_repo.RDS"

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

            saveRDS(obj, file.path(do.call(file.path, opath)))
            return(do.call(file.path, opath))
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

    cutString <- function(text, len, dotsafter=T)
        {
            if(nchar(text) == len)
                return(text)
            if(nchar(text) < len)
                return(format(text, width = len))
            text <- substr(text, 1, len-3)
            if(dotsafter)
                return(paste0(text ,"...")) else
            return(paste0("...", text))
        }

    me <- list(
        thisEnv = thisEnv,
        
        list = function()
        {

            entr <- get("entries",thisEnv)
            if(length(entr)<1)
                {
                    message("Repository is empty.")
                    return()
                }

            widths <- c(20,10,25,25)

            message(paste0(
                cutString("Name", widths[1]),
                cutString("Dim", widths[2]),
                cutString("Tags", widths[3]),
                cutString("From", widths[4], F)))
            
            for(i in 1:length(entr))
                {
                    entri <- entr[[i]]
                    message(
                        paste0(
                            cutString(entri$name, widths[1]),
                            cutString(paste(entri$dims, collapse="x"), widths[2]),
                            cutString(paste(entri$tags, collapse=","), widths[3]),
                            cutString(entri$storedfrom, widths[4])
                            )
                        )
                }

        },

        export = function(objID, where = "")
        {
            entr <- get("this",thisEnv)["getEntry"](objID)
            files.copy(entr$dump, path(where, paste0(objID, ".RDS")))
        },

        unlock = function()
        {
            get("unlockIndex", thisEnv)()
        },

        setDefaultTags = function(tags)
        {
            return(assign("defTags",tags,thisEnv))
        },


        getEntry = function(name)
        {
            
            e <- get("findEntryIndex",thisEnv)(name)
            if(is.null(e))
                return(invisible(NULL))

            return(get("entries", thisEnv)[[entr[[w]]]])
        },

        rm = function(name)
        {
            e <- get("findEntryIndex",thisEnv)(name)
            if(is.null(e))
                return(invisible(NULL))
            
            entr <- get("entries",thisEnv)
            assign("entries", entr[-e], thisEnv)
            
            ipath = do.call(file.path, buildpath(name))
            file.remove(ipath)

            get("storeIndex", thisEnv)()
        },

        load = function(name)
        {
            entry <- get("this", thisEnv)[["getEntry"]](name)
            data <- readRDS(entry$dump) 
            
            return(data)
        },

        export = function(name, where=".")
        {
            ipath = do.call(file.path, buildpath(name))
            file.copy(ipath, file.path(where, name))
        },
        
        checkIntegrity = function()
        {
            entr <- get("entries", thisEnv)
            for(i in 1:length(entr))
                {
                    message(paste0("Checking object ", entr[[i]]$name, "..."))
                    invisible(get("load", thisEnv)(entr[[i]]$name))
                }
        },
        
        wipe = function()
        {
            message(paste0(
                "WARNING: This will completely delete your repo root (\"", get("root",thisEnv), "\") !!!"))
            n <- readline("Type \"Yes!\" to confirm: ")
            if(tolower(n) == "yes!") {
                unlink(root, recursive=T)
                message("Repo root deleted.")
            } else message("Nothing done.")
        },

        getEntries = function()
        {
            return(get("entries",thisEnv))
        },

        add = function(obj, name, description="", tags="", force_replace=F)
        {
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

            fname <- get("storeData", thisEnv)(name, obj)

            repoE <- list(name = name,
                          description = description,
                          tags = tags,
                          class = class(obj),
                          dims = dims,
                          timestamp = Sys.time(),
                          dump = fname,
                          checksum = md5sum(path.expand(fname)),
                          storedfrom = getwd())

            entr <- get("entries", thisEnv)           
            entr[[length(entries)+1]] <- repoE
            assign("entries", entr, thisEnv)
            get("storeIndex", thisEnv)()
        },
        ## Define the accessors for the data fields.

        ## getEnv = function()
        ## {
        ##     return(get("thisEnv",thisEnv))
        ## },

        getRoot = function()
        {
            return(get("root",thisEnv))
        }

        ## setRoot = function(value)
        ## {
        ##     return(assign("root",value,thisEnv))
        ## }
        )

    ## Define the value of the list within the current environment.
    assign('this',me,envir=thisEnv)

    ## Set the name for the class
    class(me) <- append(class(me),"repo")

    if(!file.exists(repofile)) ## is this check needed?
        {
            ## message(paste0("Repo index doesn't exist, creating \"", root, "\"."))
            saveRDS(get("entries", thisEnv), repofile)
        }

    
    return(me)
}

#' Show a summary of the repository contents.
#' 
#' @return Used for side effects.
print.repo <- function(repo) repo$list()

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
add.obj <- function(repo, obj, id, description) repo$add(repo, id, description)

#' Open an existing repository or create a new one.
#' 
#' @param repo An object of class Repo.
#' @param id The identifier of the object to load.
#' @examples
#' repo <- open.repo()
#' add.obj(repo, cars, "cars", "R cars dataset")
#' identical(repo$load("cars"), cars)
load.obj <- function(repo, id) repo$load(name)

#' Open an existing repository or create a new one.
#' 
#' @param An object of class Repo.
#' @examples
#' repo <- open.repo()
#' add.obj(repo, cars, "cars", "R cars dataset")
#' print(repo)
#' repo$rm("cars")
#' print(repo)
rm.obj <- function(repo, name) repo$rm(name)
