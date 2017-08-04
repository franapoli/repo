############################################
########## PRIVATE METHODS #################
############################################

    ## **
    ## This function is meant the centralize messages. in order to
    ## better manage string constants. A few messages are still
    ## around, centralization is ongoing.
    handleErr <- function(err, ...)
        {
            pars <- list(...)
            if(length(pars)>0)
                lpars <- paste0(paste(pars[[1]], collapse=", "), ".")
            switch(err,
                   ## "DEBUG" = {
                   ##     message(pars[[1]])
                   ## },
                   "ID_NOT_FOUND" = {
                       stop(paste0("Item not found: ", lpars))
                   },
                   "ID_EXISTING" = {
                       stop(paste0("There is already an item with this name: ", lpars))
                   },
                   "ID_RESERVED" = {
                       stop(paste0("Id not valid (reserved): ", lpars))
                   },
                   "TAG_RESERVED" = {
                       warning(paste0("Reserved TAG used: ", lpars))
                   },
                   "EMPTY_REPO" = {
                       stop("Repo is empty.")
                   },
                   "MISS_OBJ_HAS_URL" = {
                       stop(paste0("The file object could not be found. ",
                                   "However, it can be downloaded using pull."))
                   },
                   "NO_URL" = {
                       stop("The object has no associated URL.")
                   },
                   "LAZY_FOUND" = {
                       message("lazydo found precomputed resource.")
                   },
                   "LAZY_NOT_FOUND" = {
                       message("lazydo is building resource from code.")
                   },
                   "LAZY_NOT_EXPR" = {
                       stop("expr must be of class expression.")
                   },
                   "LAZY_NAME" = {
                       message(paste0("Cached item name is: ", lpars))
                   },
                   "DATA_ALREADY_THERE" = {
                       stop(paste0("There is existing content for ", lpars, ". ",
                                   "Use replace=T to overwrite."))
                   },
                   "ATTACHMENT_FILE_NOT_FOUND" = {
                       stop(paste0("Attachment file not found: ", lpars))
                   },
                   "INFO_BUILDING_DEPS" = {
                       message(paste("Building dependency:", lpars))
                   },
                   "CHUNK_NOSOURCE" = {
                       stop(paste("The following items have no associated source code object:", lpars))
                   },
                   "CHUNK_NOCHUNK" = {
                       stop(paste("The following items have no associated code chunk:", lpars))
                   },
                   "FORK_CONFLICT" = {
                       stop(paste("The following forks are all active and contain the same item:", lpars))
                   },
                   "FORK_NOACTIVEFORK" = {
                       stop(paste("None of the active forks contain the following item:", lpars))
                   }
                   )
        }

    updatePrjInfo <- function(name)
    {
        sess <- sessionInfo()
        pkg <- attr(sess$otherPkgs, "names")
        pkgv <- sapply(pkg, function(x) as.character(packageVersion(x)))
        get("this", thisEnv)$set(name, obj=list(session = sess, pkgv = pkgv))
    }


findChunkData <- function(textlines) {
        s0 <- "[[:blank:]]*"
        s1 <- "[[:blank:]]+"
        chtag <- "chunk"
        chname <- "[\"|'](.+)[\"|']"
        chopen <- "\\{"
        chclose <- "\\}"
        startstr <- paste0("^", s0, "#+", s0, chtag, s1, chname, s0, chopen,  s0, "$")
        endstr   <- paste0("^", s0, "#+", s0, chclose, s0, "$")

        oks <- which(sapply(textlines, grepl, pattern=startstr, perl=T))
        tagss <- sapply(textlines[oks], sub, pattern=startstr, replacement="\\1", perl=T)
        oke <- which(sapply(textlines, grepl, pattern=endstr, perl=T))
        tagse <- sapply(textlines[oke], sub, pattern=endstr, replacement="\\1", perl=T)
        names(oke) <- tagse
        return(list(oks=oks, tagss=tagss, oke=oke, tagse=tagse))
}


## reads all file source code associated with an item. The item must
## already be in the repo.
getSourceLines <- function(name, src) {
    entry <- getEntry(name)
    if(is.null(entry$source) && is.null(src))
        return(NULL)

    if(checkName(src))
        handleErr("ID_NOT_FOUND", src)
    
    if(is.null(src)) {
        srcfile <- get("this", thisEnv)$attr(name, "srcfile")
        } else srcfile <- get("this", thisEnv)$get(src)
    
    txt <- readLines(srcfile)
}

## change name if item has an associated fork. Must work before the
## item is in the repo, in that case srcItem must be supplied. Is
## srcItem is NULL, the item is assumed to be in the repo already.
forkedName <- function(name, srcItem=NULL) {
        
    #srclines <- readLines(getEntry(srcItem)$dump)
    act_forks <- get("options", thisEnv)[["active_forks"]]
    forks <- unique(getItemForks(name)$v)
    if(length(forks)==0)
        return(name)
    if(sum(forks %in% act_forks)>1)
        handleErr("FORK_CONFLICT", forks)
    if(sum(forks %in% act_forks)==0)
        handleErr("FORK_NOACTIVEFORK", name)
    return(paste0(name, "#", forks[forks %in% act_forks]))
}

## Removes inactive chunks from chunkdata
deforkChunkData <- function(chunkdata, active_chunks=NULL)
{
    tags <- chunkdata$tagss
    forkedi <- grep("#", tags)
    forks <- gsub(".+#", "", tags[forkedi])
    ok <- (1:length(tags))[-forkedi[!forks %in% active_chunks]]
    chunkdata <- lapply(chunkdata, `[`, ok)
    chunkdata$tagss <- gsub("#.+", "", chunkdata$tagss)
    chunkdata$tagse <- gsub("#.+", "", chunkdata$tagse)
    return(chunkdata)
}
    
getChunk <- function(name, active_chunks=NULL, src=NULL)
{
    entry <- getEntry(name)
    if(is.null(entry$source))
        if(is.null(src))
            return(NULL)
    
    txt <- getSourceLines(name, src)
    
    if(is.null(txt))
        return(NULL)
    chunkData <- findChunkData(txt)
    ##chunkData <- deforkChunkData(findChunkData(txt), active_chunks)
    tagss <- chunkData$tagss
    oks <- chunkData$oks
    tagse <- chunkData$tagse
    oke <- chunkData$oke
    if(is.null(src)) 
        cname <- entry$chunk else cname <- name
    if(!(cname %in% tagss))
        return(NULL)
    
    names(oks) <- names(oke) <- tagss
    if(any(duplicated(tagss)))
        stop(paste("Chunk names are not unique: ",
                   unique(tagss[duplicated(tagss)])))
    

    chunks <- vector("character", length(tagss))
    for(i in which(tagss == cname)) ## written to find all chunks
    {
        tag <- tagss[i]
        ## if(!(tag %in% tagse))
        ##     stop(paste(tag, "has no matching end"))
        ## if(sum(tagse==tag)>1)
        ##     stop(paste(tag, "has more than 1 matching end"))
        
        if(length(oks)!=length(oke))
            stop("Some chunks are open, check syntax")
        if(oke[tag]<oks[tag])
            stop(paste(tag, "has end line before start line"))
        chunks[i] <- paste(txt[(oks[tag]+1):(oke[tag]-1)], collapse="\n")
    }
    return(chunks[[which(tagss==cname)]])
        
}

    ## Finds the connected component in the dependancy graph
    ## containing a given set of items. Useful to extract indipendent
    ## pieces of a repository.
    getRelatives <- function(names, ascendants=T, firstset=names)
    {
        makenull <- function(res) {
            if(length(res)==0)
                return(NULL)
            return(res)
        }
        
        m <- obj <- get("this", thisEnv)$dependencies(plot=F)
        allnames <- sapply(entries, get, x="name")
        if(ascendants)
            col <- m[, allnames %in% names, drop=F] else col <- t(m[allnames %in% names,,drop=F])

        col <- apply(col!=0, 1, any)
        
        if(all(col==0)) {
            return(makenull(setdiff(names, firstset)))
        } else {
            asc <- names(which(col!=0))
            if(all(asc %in% names))
                return(makenull(setdiff(names, firstset)))
            return(makenull(setdiff(getRelatives(c(names, asc), ascendants, firstset), firstset)))
        }
    }

    ## Given an item name, builds the file path where actual object is
    ## stored. For legacy reasons handles both absolute and relative
    ## to repository root paths.
    getFile <- function(name)
        {
            entry <- getEntry(name)

            if(substr(normalizePath(entry$dump, mustWork=F), 1, nchar(root)) == root) {
                fpath <- normalizePath(entry$dump, mustWork=F)
                } else fpath <- normalizePath(file.path(root, entry$dump), mustWork=F)

            return(fpath)
        }

## Given an item name, returns associated source object
getSource <- function(name)
        {
            entry <- getEntry(name)
            return(invisible(entry$source))
        }


    ## Stores actual data for an existing item to a file.
    setData <- function(name, obj, asattach)
    {
        w <- findEntryIndex(name)
        newdata <- list()

        if(!asattach) {
            if(!is.null(dim(obj)))
                dims <- dim(obj) else dims <- length(obj)
        } else dims <- list(NULL)

        rmData(name, "temp")            
        
        tryCatch({
            fdata <- storeData(name, obj, asattach)
        }, error = function(e) {
            print(e)
            rmData(name, "undo")
        }, finally = {
            rmData(name, "finalize")
        }
        )                
        
        newdata[["dump"]] <- relativePath(fdata[["path"]])
        newdata[["size"]] <- fdata[["size"]]
        newdata[["checksum"]] <- md5sum(path.expand(fdata[["path"]]))
        newdata[["dims"]] <- dims

        return(newdata)
    }

    ## **
    ## Checks wether a set of provided tags are OK. A few tags are
    ## reserved, but this needs to be handled better.
    checkTags <- function(tags, name=NULL, replace=F)
    {

        dups <- which(duplicated(tolower(tags)))
        if(length(dups)>0)
            warning(paste0("The following tags are duplicated (not case sensitive): ",
                        paste0(tags[dups], collapse=", ")))
        
        if(!is.null(name)) {
            e <- getEntry(name)
            comm <- intersect(tolower(e$tags), tolower(tags))
            if(length(comm)>0 && !replace)
                warning(paste0("The following tags are already present (not case sensitive): ",
                            paste0(comm, collapse=", ")))
        }
        reservedTags <- c("stash", "attachment", "#project")
        if(any(tolower(tags) %in% reservedTags))
            handleErr("TAG_RESERVED", tags[tolower(tags) %in% reservedTags])                 

        return(unique(tags))
    }

    
    ## Checks if an item has multiple version (other items by the same
    ## name + "#1", "#2", ... and return them
    checkVersions <- function(name)
        {
            names <- sapply(entries, get, x="name")
            ## searching for names ending with # and a number
            w <- regexpr(paste0(name, "#[[:digit:]]+$"), names)
            
            ## extract the numbers
            v <- as.numeric(gsub(paste0(name,"#"),"",names[w!=-1]))
            if(length(v)>0)
                newname <- paste0(name, "#", max(v)+1)
            else
                newname <- paste0(name, "#1")
            
            return(list(w=which(w!=-1), v=v, new=newname))
        }

    ## Checks if an item has multiple forks (other items by the same
    ## name + "#fork1", "#fork2", ... and return them
    getItemForks <- function(name)
        {
            names <- sapply(entries, get, x="name")
            ## searching for names ending with #, then anything but a
            ## number, then whatever
            w <- regexpr(paste0(name, "#[^[:digit:]].+$"), names)
            
            ## extract the version names
            v <- gsub(paste0(name,"#"),"",names[w!=-1])
            
            return(list(w=which(w!=-1), v=v))
        }


    ## Makes a path relative to the repository root
    relativePath <- function(path)
    {
        sep <- .Platform$file.sep
        root <- get("root",thisEnv)                
        relpath <- gsub(paste0(root, sep), "", path ,fixed=T)
        return(relpath)
    }

    
    ## this makes sure the repository is not empty
    stopOnEmpty <- function(doreturn=F) {
        if(length(entries)<1) {
            if(doreturn)
                return(1) else handleErr("EMPTY_REPO")
        }
        return(0)
    }
    
    
    ## ** Only used by get for deprecated formats, need to be cleaned
    setEntry <- function(name, newEntry)
    {
        stopOnNotFound(name)
        e <- findEntryIndex(name)
        entries[[e]] <- newEntry
        assign("entries", entries, thisEnv)
    }

    ## makes sure that an item exists
    stopOnNotFound <- function(names=NULL, tags=NULL)
        {
            stopOnEmpty()
            allnames <- sapply(entries, get, x="name")
            w <- match(names, allnames)
            if(any(is.na(w)))
                handleErr("ID_NOT_FOUND", names[is.na(w)])
        }
    
    ## returns an item's entry
    getEntry <- function(name) {
            e <- findEntryIndex(name)
            if(is.null(e))
                return(invisible(NULL))
            
            return(entries[[e]])
        }

    
    ## ** messages need externalization
    ##
    ## this is used by all function that support running on multiple
    ## names or tags
    runWithTags <- function(f, tags, names, askconfirm, tagfun="OR", ...) {
        if(!is.null(tags))
            e <- findEntries(tags, tagfun) else {
                dbnames <- sapply(entries, get, x="name")
                e <- match(names, dbnames)
                if(any(is.na(e))) {
                    errmsg <- paste0("The following names could not be found: ",
                           paste(names[which(is.na(e))], collapse=", "))
                    stop(errmsg)
                }
            }
                            
            if(length(e)<1)
                stop("Tag or name list does not match any entry.")
            entr <- entries
            names <- sapply(entr[e], get, x="name")
            if(askconfirm) {
                cat(paste0("Matched entries:\n",
                               paste(names, collapse="\n"), "\n"))
                n <- readline("Type \"yes\" to confirm: ")
            } else n <- "yes"
            if(n == "yes") {
                for(i in 1:length(e))
                    get("this", thisEnv)[[f]](name=names[[i]], ...)
                return(invisible())
            } else
                {
                    message("Nothing done.")
                    return(invisible())
                }
        }


    ## makes units of measures human readable for print
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

    
    ## ** messages to be externalized
    ## ** multiple writes are not 100% safe
    ##
    ## checks that the repository index has not changed since last
    ## write. This is necessary when the same repository is open in
    ## multiple instances. For example, one may try to read something
    ## that has been removed. Race conditions are not yet completely
    ## avoided, anyway they are extrimely rare and only involve meta
    ## data (not data).
    checkIndexUnchanged <- function() {
        ## if(DEBUG) {
        ##     message(paste0("checkIndexUnchanged: cur MD5 is ", md5sum(repofile)))
        ##     message(paste0("checkIndexUnchanged: stored MD5 is ", indexMD5))
        ## }
        
      if(indexMD5 != md5sum(repofile))
          stop(format(paste0("Repo index has been modified outside this session. ",
                             "For security reasons I will stop here. Please open the ",
                             "repo again to sync with the latest changes ",
                             "(which may include deletions). You may want to run \"check\" ",
                             "on this session first.")))
    }


    ## store all meta data
    storeIndex <- function() {
        saveRDS(entries, repofile)
        ## if(DEBUG) {
        ##     message(paste0("storeIndex: stored MD5 is ", indexMD5))
        ##     message(paste0("StoreIndex: new MD5 is ", md5sum(repofile)))
        ## }
        ## NOTE: do not indexMD5 <- md5sum(repofile)... doesn't work
        assign("indexMD5", md5sum(repofile), thisEnv)
        }


    ## **
    ## this is now only used by storeData, neads cleaning
    buildpath <- function(resname)
        {
            resname <- paste0(sample(c(0:9,letters), 32, T),collapse="")
            return(list(root,
                        substr(resname, 1, 2),
                        substr(resname, 3, 4),
                        substr(resname, 5, 6),
                        resname))
        }

    ## check if an item name already exists
    checkName <- function(name)
        {
            if(length(entries)<1)
                return(T)
            names <- sapply(entries, get, x="name")
            return(!all((name %in% names)))
        }

    ## Delete itme's file. For safety it's done in 3 phases. 1)
    ## Existing file is renamed, 2) new file is created, 3) old file
    ## is removed. If anything goes wrong before 3, old file is
    ## restored.
    rmData <- function(name, phase)
    {
        fpath <- getFile(name)
        fpath_temp <- paste0(fpath, ".remove_me")

        if(!file.exists(fpath) && !file.exists(fpath_temp)){
            ## this should never happen, unless file was removed from
            ## something else:
            warning(paste("File to be removed was not found:", fpath))
            return(invisible(0))
        }
        
        if(phase=="temp") {
            file.rename(fpath, fpath_temp)
        } 
        if(phase=="finalize") {
            file.remove(fpath_temp)
        }
        if(phase=="undo") {
            file.rename(fpath_temp, fpath)
        }
        
        return(invisible(NULL))
    }

    ## stores data in RDS format or copies it if it's an attachment
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

            if(!attach) {
                saveRDS(obj, fpath)
                } else {
                    didwork <- file.copy(obj, fpath)
                    if(!didwork)
                        stop(paste0("There was an error while trying to write file: ",
                                    fpath))
                }
                
            return(list(path=fpath, size=file.info(fpath)$size))
        }

    ## builds the item's dependency graph
    depgraph <- function(tags = NULL, tagfun, depends=T, attached=T, generated=T)
        {            
            stopOnEmpty()
            
            if(!any(c(depends, attached, generated)))
              stop("One of depends, attached or generated must be true.")

            if(!is.null(tags))
                sube <- findEntries(tags, tagfun) else sube <- 1:length(entries)

            nodes <- unique(unlist(sapply(entries[sube], get, x="name")))

            ## if(generated) {
            ##   srcs <- unique(unlist(sapply(entries, get, x="source")))
            ##   nodes <- c(nodes, srcs)
            ## }
            n <- length(nodes)
            depgraph <- matrix(0,n,n)
            for(i in 1:n)
                {
                  e <- entries[sube][[i]]
                  if(depends) {
                    w <- match(e$depends, nodes)
                    depgraph[i, w] <- 1
                  }
                  if(attached) {
                    w <- match(e$attachedto, nodes)
                    depgraph[i, w] <- 2
                  }
                  if(generated) {
                      w <- match(e$source, nodes)
                      depgraph[i, w] <- 3
                    }
                }
            rownames(depgraph) <- colnames(depgraph) <- nodes
            return(depgraph)
        }


    ## ** like getEntry, but returns just the index. There's an
    ## inconsistency between "find" and "get" in their names
    findEntryIndex <- function(name)
        {
            if(is.null(entries) | length(entries)<1) {
                return(-1)
            }
            names <- sapply(entries, get, x="name")
            w <- match(name, names)
            if(length(w)<1){
                return(NULL)
            }
            return(w)
        }

    ## finds a set of entries using tags and logic operators or string
    ## matching
    findEntries <- function(tags=NULL, tagfun="OR", find=NULL)
        {
            if(!is.null(tags)) {
                   tagsets <- lapply(entries, get, x="tags")

                   if(is.character(tagfun) && tagfun =="AND")
                       tagfun <- function(x, tags=tags)all(tags %in% x)
                   if(is.character(tagfun) && tagfun=="NOT")
                       tagfun <- function(x, tags=tags)all(!(tags %in% x))
                   if(is.character(tagfun) && tagfun=="OR")
                       tagfun <- function(x, tags=tags)any(tags %in% x)

                   if(class(tagfun)!="function")
                       stop("tagfun must be either a function or one of OR, AND, NOT")

                   w <- sapply(tagsets, tagfun, tags)
               } else {
                   strmat <- entriesToMat(1:length(entries))
                   w <- apply(strmat, 1, function(l)
                       length(grep(find, l, ignore.case=T))>0)
               }
               
            return(which(w))
        }

    ## tells wether an item is an attachment
    isAttachment <- function(name)
        {
            w <- findEntryIndex(name)
            return("attachment" %in% entries[[w]]$tags)
        }
    isProject <- function(name)
        {
            w <- findEntryIndex(name)
            return("#project" %in% entries[[w]]$tags)
        }

    ## returns the list of items attached to an item
    attachments <- function(name)
        {
            r <- match(name,  sapply(entries, get, x="attachedto"))
            if(is.na(r))
                return(NULL)            
            return(r)
        }

    ## returns the list of items dependant on an item
    dependants <- function(name)
        {
            r <- sapply(sapply(entries, get, x="depends"), match, x=name)
            w <- which(!is.na(r))
            if(length(w)<1)
                return(NULL)            
            return(w)
        }

    ## returns the list of items belonging to a project item
    prjmembers <- function(name)
        {
            r <- sapply(sapply(entries,`[[`,"prj"), match, x=name)
            w <- which(!is.na(r))
            if(length(w)<1)
                return(NULL)            
            return(w)
        }
    
    
    ## ** shortens a path using the home (~) notation. Only used by
    ## "info", so could be redesigned.
    compressPath <- function(path)
        {
            hp <- path.expand("~")
            return(gsub(paste0("^",hp), "~", path))
        }


    ## ** creates a table with all items metadata to be shown by
    ## print. Currently filters are applied in the end, which slows
    ## down the process. The flags column is currently not used.
    entriesToMat <- function(w)
        {
            entr <- entries[w]

            labels <- c("ID", "a@><", "Dims", "Tags", "Size")
            names <- sapply(entr, get, x="name")

            a <- matrix(NA, length(names), length(labels))
            colnames(a) <- labels

            attachs <- depends <- hasattach <- allows <- rep(" ", length(entr))
                            
            tagsets <- lapply(entr, get, x="tags")
            attachs[sapply(tagsets, is.element, el="attachment")] <- "x"
            depends[sapply(lapply(entr, get, x="depends"), length)>0] <- "x"
            allows[!sapply(lapply(names, dependants), length)>0] <- "x"
            hasattach[!sapply((sapply(names, attachments)), is.null)] <- "x"            

            flags <- paste0(attachs, hasattach, depends, allows)
            
            descriptions <- sapply(entr, get, x="description")
            prefixes <- rep("", length(names))
            prefixes[attachs == "x"] <- "@"                        

            a[,"ID"] <- paste0(prefixes, names)
            a[,2] <- flags
            a[,"Dims"] <- sapply(lapply(entr, get, x="dims"), paste, collapse="x");
            a[a[,"Dims"]=="", "Dims"] <- "-"            
            a[,"Tags"] <- sapply(tagsets, paste, collapse=", ")
            a[,"Size"] <- sapply(lapply(entr, get, x="size"), hmnRead)

            return(a)
        }

repo_methods_private <- function()
{
    methods = list(
        handleErr = handleErr,
        updatePrjInfo = updatePrjInfo,
        getChunk = getChunk,
        getRelatives = getRelatives,
        getFile = getFile,
        setData = setData,
        checkTags = checkTags,
        checkVersions = checkVersions,
        relativePath = relativePath,
        stopOnEmpty = stopOnEmpty,
        setEntry = setEntry,
        stopOnNotFound = stopOnNotFound,
        getEntry = getEntry,
        runWithTags = runWithTags,
        hmnRead = hmnRead,
        checkIndexUnchanged = checkIndexUnchanged,
        storeIndex = storeIndex,
        buildpath = buildpath,
        checkName = checkName,
        rmData = rmData,
        storeData = storeData,
        depgraph = depgraph,
        findEntryIndex = findEntryIndex,
        findEntries = findEntries,
        isAttachment = isAttachment,
        isProject = isProject,
        attachments = attachments,
        dependants = dependants,
        prjmembers = prjmembers,
        compressPath = compressPath,
        entriesToMat = entriesToMat,
        getSource = getSource,
        forkedName = forkedName,
        getItemForks = getItemForks,
        deforkChunkData = deforkChunkData,
        getSourceLines = getSourceLines
    )

    return(methods)
}
