
## TODOs:
##
## [X] Add general repo info at "info" without arguments
## [X] Add optional confirmation to runwithtags
##     [X] Disable confirmation in print
## [X] Add a function for each object (loads as default)
##     id <- "item1"; assign(id, function(f="get") a[[f]](name=id))
## [X] change replace with replace
## [X] Add tags
## [ ] Manage internal and multiple provenance
## [ ] Add has-attachment and internal-provenance flags @<>
## [ ] Manage dependency tree
## [+] Attachments
##     [X] Attach files
##     [X] attach shortcut
##     [ ] Attach to resource
##     [+] Manage attachments in print
##         [ ] Manage attached to
##     [ ] Open attachments (look at openPDF)
##     [X] Export
## [ ] Consider switching to readable file names (not really necessary and requires digest)
## [X] Text resource:
##     [X] Append
##     [X] Source [dropped]
## [+] Multiple repos
##     [X] Manage concurrency
##         [X] Make repo_close()
##         [X] Use MD5
##     [ ] Copy to other repo
## [X] Manage special flags ("hide")
## [ ] Check environments (are the "get"-s and "assign"-s necessary?)
##     check this line:  "## NOTE: do not indexMD5 <- md5sum(repofile)... doesn't work"
## [X] Set entry details
## [X] Show entry details
## [X] Adjust print: convert to data frame
## [X] Refactor: call "load" "get" and "add" "put"
## [ ] Externalize methods ("me" list)
## [ ] Check the use of "message" (maybe replace with "cat")
## [ ] S3-style methods and documentation

DEBUG <- F

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
        names(values) <- c("B","kB", "MB", "GB", "TB", "PB")
        okvals <- values[values>1]
        m <- okvals[which.min(okvals)] ## preserves name
        final <- format(round(m,2), scientific=F)
        return(paste0(final, " ", names(m)))
        }
    
    checkIndexUnchanged <- function() {
        if(DEBUG) {
            message(paste0("checkIndexUnchanged: cur MD5 is ", md5sum(repofile)))
            message(paste0("checkIndexUnchanged: stored MD5 is ", indexMD5))
        }
        
      if(indexMD5 != md5sum(repofile))
          stop(format(paste0("Repo index has been modified outside this session. ",
                             "For security reasons I will stop here. Please open the ",
                             "repo again to sync with the latest changes ",
                             "(which may include deletions). You may want to run \"check\" ",
                             "on this session first.")))
    }
    
    storeIndex <- function() {
        saveRDS(entries, repofile)
        if(DEBUG) {
            message(paste0("storeIndex: stored MD5 is ", indexMD5))
            message(paste0("StoreIndex: new MD5 is ", md5sum(repofile)))
        }
        ## NOTE: do not indexMD5 <- md5sum(repofile)... doesn't work
        assign("indexMD5", md5sum(repofile), thisEnv)
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

    storeData <- function(name, obj, attach=F)
        {
            opath <- buildpath(name)
            if(!file.exists(do.call(file.path, opath[1:2])))
                dir.create(do.call(file.path, opath[1:2]))
            if(!file.exists(do.call(file.path, opath[1:3])))
                dir.create(do.call(file.path, opath[1:3]))
            if(!file.exists(do.call(file.path, opath[1:4])))
                dir.create(do.call(file.path, opath[1:4]))

            fpath <- do.call(file.path, opath)
            if(!attach)
                saveRDS(obj, fpath) else 
            file.copy(obj, fpath)
                
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

    isAttachment <- function(name)
        {
            w <- findEntryIndex(name)
            return("attachment" %in% entries[[w]]$tags)
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

        handlers = function()
        {
            h <- list()
            for(i in 1:length(entries))
                {
                    fbody <- paste0('function(f="get", ...)',
                                    'get("this", thisEnv)[[f]](name ="', entries[[i]]$name, '",...)')
                    h[[i]] <- eval(parse(text=fbody))
                }
            names(h) <- sapply(entries, get, x="name")
            return(h)
        },
                
        tags = function()
        {
            entr <- entries
            tagset <- unique(unlist(lapply(entr, get, x="tags")))
            return(tagset)
        },

        print = function(tags=NULL, showall=F)
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
            attachs <- which(sapply(tagsets, is.element, el="attachment"))
            
            tagsets <- sapply(tagsets, setdiff, y="attachment")
            prefixes <- rep("", length(names))
            prefixes[attachs] <- "@"
            
            a[,"ID"] <- paste0(prefixes, names)
            a[,"Dims"] <- sapply(sapply(entr, get, x="dims"), paste, collapse="x")
            a[a[,"Dims"]=="", "Dims"] <- "-"
            a[,"Tags"] <- sapply(tagsets, paste, collapse=", ")
            a[,"Size"] <- sapply(sapply(entr, get, x="size"), hmnRead)

            w <- sapply(tagsets, is.element, el="hide")
            w <- w | !(sapply(lapply(entr, get, x="attachedto"), is.null))
            if(showall)
                w[w] <- F
            
            
            print(data.frame(a[!w,]), quote=F, row.names=F)
            
        },

        export = function(name, where=".", tags=NULL)
        {
            if(!xor(missing(name), is.null(tags)))
                stop("You must specify either names or tags.")

            if(!is.null(tags)){
                runWithTags("export", tags, where)
            } else {
                ipath = do.call(file.path, buildpath(name))
                if(isAttachment(name))
                    fname <- name else fname <- paste0(name, ".RDS")
                file.copy(ipath, file.path(where, fname))
            }
        },
        
        info = function(name = NULL, tags = NULL)
        {
            if(!xor(is.null(name), is.null(tags))) {
                labels <- c("Root:", "Number of items:", "Total size:")
                maxw <- max(sapply(labels, nchar))
                vals <- c(compressPath(root), length(entries),
                          hmnRead(sum(sapply(entries, get, x="size"))))
                lines <- paste(format(labels, width=maxw), vals, sep=" ")
                for(i in 1:length(lines))
                    cat(lines[[i]], "\n")
                return(invisible(NULL))
            }            

            
            if(!is.null(tags)){
                runWithTags("info", tags, askconfirm=F)
            } else {            
                                        #e <- get("findEntryIndex",thisEnv)(name)
                e <- findEntryIndex(name)
                if(is.null(e))
                    stop("Identifier not found.")

                labels <- c("ID:", "Description:", "Tags:",
                            "Dimensions:", "Timestamp:", "Size on disk:",
                            "Provenance:", "Attached to:", "Stored in:", "MD5 checksum:")
                maxlen <- max(sapply(labels, nchar))

                if(is.null(entries[[e]]$attachedto))
                    att <- "-"

                vals <- c(entries[[e]]$name, entries[[e]]$description,
                          paste0(entries[[e]]$tags, collapse=", "),
                          paste(entries[[e]]$dims, collapse="x"),
                          as.character(entries[[e]]$timestamp), hmnRead(entries[[e]]$size),
                          entries[[e]]$source, att, entries[[e]]$dump,
                          entries[[e]]$checksum)
                cat(paste0(format(labels, width=maxlen+1), vals, "\n"), sep="")
                cat("\n")
            }
        },

        rm = function(name = NULL, tags = NULL)        
        {
          checkIndexUnchanged()                   
            
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
            if(isAttachment(name))
                stop("Get is valid for attachemnts.")
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

        tag = function(name = NULL, newtags, tags = NULL)
        {
            if(!xor(is.null(name), is.null(tags)))
                stop("You must provide either name or tags.")
            if(!is.null(tags))
                runWithTags("tag", tags, newtags) else
                    get("this", thisEnv)$set(name, addtags=newtags)                   
        },


        untag = function(name = NULL, rmtags, tags = NULL)
        {
            if(!xor(is.null(name), is.null(tags)))
                stop("You must provide either name or tags.")
            if(!is.null(tags))
                runWithTags("untag", tags, F, rmtags) else {
                    currtags <- getEntry(name)$tags
                    w <- rmtags %in% currtags
                    if(!any(w))
                        warning(paste0("Tag/s ", paste0(rmtags[!w], collapse=", "),
                                       " not present in entry ", name))
                    currtags <- setdiff(currtags, rmtags)
                    get("this", thisEnv)$set(name, tags = currtags)
                }
        },
        
        set = function(name, obj=NULL, newname=NULL, description=NULL, tags=NULL, src=NULL, addtags=NULL)
        {
            checkIndexUnchanged()                    
            
            if(missing(name) | (missing(newname) & missing(obj) & missing(description) &
                                 missing(tags) & missing(addtags)  & missing(src)))
                stop("You must provide name and one of: obj, description, tags or addtags, src.")
            if(!missing(tags) & !missing(addtags))
                stop("You can not specify both tags and addtags.")
            
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
            if(!is.null(addtags))
                entr$tags <- unique(c(entr$tags, addtags))
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
            storeIndex()
        },

        add = function(obj, name, description, tags, src=NULL, replace=F)
        {
            warning("add is deprecated, use put.")
            return(get("this", thisEnv)$put(obj, name, description, tags, src, replace))
        },

        attach = function(filepath, description, tags, src=NULL, replace=F, to=NULL)
        {
            get("this", thisEnv)$put(filepath, basename(filepath),
                                     description, tags, src, replace, T, to)
        },
        
        put = function(obj, name, description, tags, src=NULL, replace=F, asattach=F, to=NULL)
        {
            checkIndexUnchanged()
            
            if(missing(obj) | missing(name) | missing(description) | missing(tags))
                stop("You must provide all of: obj, name, description, tags.")

            if(!is.null(to))
                asattach <- T
            
            notexist <- checkName(name)
            if(!notexist & !replace)
                {                    
                    message("Identifier already used.")
                    return(invisible(NULL))
                }

            if(!notexist & replace)
                get("this", thisEnv)$rm(name)

            if(!asattach) {
                if(!is.null(dim(obj)))
                    dims <- dim(obj) else
                dims <- length(obj)
            } else {
                dims <- NULL
                tags <- unique(c(tags, "attachment"))
            }

            fdata <- get("storeData", thisEnv)(name, obj, asattach)
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
                          source = storedfrom,
                          attachedto = NULL)

            entr <- get("entries", thisEnv)
            entr[[length(entries)+1]] <- repoE
            assign("entries", entr, thisEnv)
            get("storeIndex", thisEnv)()
        },        
        
        test=function()
        {
            print(ls(envir=thisEnv))
        },

        append = function(id, txtorfunc)
        {
          checkIndexUnchanged()
                                
          notexist <- checkName(id)
          if(notexist)
              stop("Identifier not found.")

          if(class(txtorfunc)=="function")
              txtorfunc <- paste0("\n",
                                  paste(deparse(txtorfunc), collapse="\n"),
                                  "\n")

          if(class(txtorfunc)!="character")
              stop("txtorfunc must be an object of class function or character")
          
          #e <- findEntryIndex(id)
          curobj <- this$get(id)
          this$set(id, obj=paste0(curobj, txtorfunc))
        },        
        
        root = function()
        {
            return(get("root",thisEnv))
        }

        )


    root <- normalizePath(root, mustWork=F)
    REPOFNAME <- "R_repo.RDS"
    repofile <- file.path(root, REPOFNAME)
    thisEnv <- environment()
    class(me) <- append(class(me),"repo")
    assign('this', me, envir=thisEnv)
    assign("entries", list(), envir=thisEnv)
    
    if(file.exists(repofile))
        {
            message(paste0("Found repo index in \"",
                           repofile, "\"."))
            assign("entries", readRDS(repofile), thisEnv)
        } else {

            if(!file.exists(root))
                {
                    message(paste0(
                        "Repo root (\"", get("root",thisEnv), "\") does not exist. Create it?"))
                    n <- readline("Type \"yes\" to proceed: ")
                    if(tolower(n) == "yes") {
                        dir.create(root)
                        message("Repo root created.")
                    } else message("Nothing done.")
                }

            saveRDS(me, repofile)
            message("Repo created.")
        }

    indexMD5 <- md5sum(repofile)
    
    return(me)
}



repo_test <- function(where = "./repotest")
    {
        f <- file.path(where)
        repo <- repo_open(f)
        repo$put(1:10, "item1", "item 1 description", c("tag1", "tag2"))
        repo$put(1:100, "item2", "item 2 description", c("tag2", "tag3"))
        repo$put(matrix(runif(100*100), 100, 100), "item3",
                 "item 3 description", c("tag1", "tag2", "tag3"))
        repo$put(list(1,2,3,4,5), "item4", "item 4 description", c("tag1"))
        repo$put("string", "item5", "item 5 description", c("tag2"), replace=T)
        return(repo)
    }
