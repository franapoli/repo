
globalVariables(c("DEBUG", "entries", "this"))

library(digest) # digest
library(tools) # md5sum

repo_open <- function(root="~/.R_repo", force=F)
{
    DEBUG <- F

    checkVersions <- function(name)
        {
            names <- sapply(entries, get, x="name")
            #names <- c("sadf","fasd#2","abc","abc#1", "abc#2","sdfa","abc#23")
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

    
    stopOnEmpty <- function(doreturn=F) {
        if(length(entries)<1) {
            if(doreturn)
                return(1) else stop("Repository is empty.", call.=F)
        }
        return(0)
    }

    stopOnNotFound <- function(names=NULL, tags=NULL)
        {
            stopOnEmpty()
            allnames <- sapply(entries, get, x="name")
            w <- match(names, allnames)
            if(all(is.na(w))){
                msg <- paste0("The following entries could not be matched: ",
                              paste(names[is.na(w)], collapse=", "), ".")
                stop(msg, call.=F)
            }
        }
    
    getEntry <- function(name) {
            e <- findEntryIndex(name)
            if(is.null(e))
                return(invisible(NULL))
            
            return(entries[[e]])
        }

    runWithTags <- function(f, tags, names, askconfirm, ...) {
        if(!is.null(tags))
            e <- findEntries(tags) else {
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
                cat(paste0("Matched entries: ",
                               paste(names, collapse=", "), "\n"))
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

    depgraph <- function(depends=T, attached=T, generated=T)
        {
            stopOnEmpty()
            if(!any(c(depends, attached, generated)))
              stop("One of depends, attached and generated must be true.")
            
            nodes <- unique(unlist(sapply(entries, get, x="name")))
            if(generated) {
              srcs <- unique(unlist(sapply(entries, get, x="source")))
              nodes <- c(nodes, srcs)
            }
            n <- length(nodes)
            depgraph <- matrix(0,n,n)
            for(i in 1:length(entries))
                {
                  e <- entries[[i]]
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

    checkFoundEntry <- function(e)
        {
            if(is.null(e))
                cat("Entry not found.\n")
            if(e==-1)
                cat("Repo is empty.\n")
        }
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

    attachments <- function(name)
        {
            r <- match(name,  sapply(entries, get, x="attachedto"))
            if(is.na(r))
                return(NULL)            
            return(r)
        }

    dependants <- function(name)
        {
            r <- sapply(sapply(entries, get, x="depends"), match, x=name)
            w <- which(!is.na(r))
            if(length(w)<1)
                return(NULL)            
            return(w)
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
        dependencies = function(depends=T, attached=T, generated=T, plot=T)
        {
          deps <- depgraph(depends, attached, generated)
          if(plot) {
              if (requireNamespace("igraph", quietly = TRUE)) {
                  deps2 <- deps
                  rownames(deps2) <- colnames(deps2) <- basename(rownames(deps))
                  g <- igraph::graph.adjacency(deps2, weighted=c("type"))
                  igraph::plot.igraph(g, edge.label=c("depends", "attached", "generated")
                               [igraph::get.edge.attribute(g,"type")])
              } else {
                  stop("The suggested package igraph is not installed.")
              }              
          }
          invisible(depgraph())
        },

        check = function()
            {
                stopOnEmpty()
            entr <- entries
            for(i in 1:length(entr))
                {
                    cat(paste0("Checking ", entr[[i]]$name, "..."))
                    if(file.exists(path.expand(entr[[i]]$dump))){
                        md5s <- md5sum(path.expand(entr[[i]]$dump))
                        if(md5s != entr[[i]]$checksum) {
                            cat(" changed!")
                            warning("File has changed!")
                        } else cat(" ok.")
                    } else {
                        cat(" not found!")
                        warning("Missing file!")
                    }
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

        copy = function(destrepo, name, tags=NULL)
        {            
            if(!("repo" %in% class(destrepo)))
                stop("destrepo must be an object of class repo.")
            if(!xor(missing(name), is.null(tags)))
                stop("You must specify either names or tags.")

            if(length(name) > 1 | !is.null(tags))
                runWithTags("copy", tags, name, T, destrepo) else {
                    e <- findEntryIndex(name)
                    checkFoundEntry(e)
                    entr <- entries[[e]]
                    obj <- get("this", thisEnv)$get(name)
                    destrepo$put(obj, name, entr$description, entr$tags, entr$source, entr$depends)
                }
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
            h[[length(h)+1]] <- get("this", thisEnv)
            names(h) <- c(sapply(entries, get, x="name"), "repo")
            return(h)
        },
                
        tags = function()
        {
            entr <- entries
            tagset <- unique(unlist(lapply(entr, get, x="tags")))
            return(tagset)
        },

        sys = function(name, command)
        {
            stopOnNotFound(name)
            e <- getEntry(name)
            syscomm <- paste0(command, " ", e[["dump"]])
            message(paste("Running system command:", syscomm))
            system(syscomm)
        },

        print = function(tags=NULL, all=F, show="ds")
        {
            stopOnEmpty()
            
            entr <- entries
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

            labels <- c("ID", "a@><", "Dims", "Tags", "Size")
            names <- sapply(entr, get, x="name")

            a <- matrix(NA, length(names), length(labels))
            colnames(a) <- labels

            attachs <- depends <- hasattach <- allows <- rep(" ", length(entr))
                            
            tagsets <- lapply(entr, get, x="tags")
            attachs[sapply(tagsets, is.element, el="attachment")] <- "x"
            depends[sapply(lapply(entr, get, x="depends"), length)>0] <- "x"
            allows[!sapply((sapply(names, dependants)), is.null)] <- "x"
            hasattach[!sapply((sapply(names, attachments)), is.null)] <- "x"

            flags <- paste0(attachs, hasattach, depends, allows)

            descriptions <- sapply(entr, get, x="description")
            
            tagsets <- lapply(tagsets, setdiff, y="attachment")
            #tagsets <- lapply(tagsets, setdiff, y="hide")
            tagsets <- lapply(tagsets, setdiff, y="stash")

            prefixes <- rep("", length(names))
            prefixes[attachs == "x"] <- "@"
            
            a[,"ID"] <- paste0(prefixes, names)
            a[,2] <- flags
            a[,"Dims"] <- sapply(lapply(entr, get, x="dims"), paste, collapse="x"); a[a[,"Dims"]=="", "Dims"] <- "-"            
            a[,"Tags"] <- sapply(tagsets, paste, collapse=", ")
            a[,"Size"] <- sapply(lapply(entr, get, x="size"), hmnRead)

            h <- rep(F,length(entr))
            if(!("hide" %in% tags))
              h <- sapply(tagsets, is.element, el="hide")
            h <- h | !(sapply(lapply(entr, get, x="attachedto"), is.null))
            cols <- c(T, sapply(c("f","d","t","s"), grepl, show))
            if(all)
                h[h] <- F            
            m <- as.data.frame(a[!h,cols], nm="")
            ## if(sum(!h)>1)
            ##     print(m, quote=F, row.names=F) else
            ## print(m, quote=F, row.names=T)
            if(sum(!h)>1)
                print(m, quote=F, row.names=F) else print(t(m), quote=F, row.names=F)

            invisible(m)
        },

        export = function(name, where=".", tags=NULL)
        {
            if(!xor(missing(name), is.null(tags)))
                stop("You must specify either names or tags.")

            if(!is.null(tags) | length(name)>1){
                runWithTags("export", tags, name, T, where)
            } else {
                ipath = do.call(file.path, buildpath(name))
                if(isAttachment(name))
                    fname <- name else fname <- paste0(name, ".RDS")
                file.copy(ipath, file.path(where, fname))
            }
        },
        
        info = function(name = NULL, tags = NULL)
        {
            stopOnEmpty()
            
            if(!is.null(name))
                stopOnNotFound(name)
            
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

            
            if(!is.null(tags) | length(name)>1){
                runWithTags("info", tags, name, askconfirm=F)
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
                          paste(entries[[e]]$source, collapse=", "), att, entries[[e]]$dump,
                          entries[[e]]$checksum)
                cat(paste0(format(labels, width=maxlen+1), vals, "\n"), sep="")
                cat("\n")
            }
        },

        rm = function(name = NULL, tags = NULL, force)
        {
          checkIndexUnchanged()                   
            
            if(!xor(missing(name),missing(tags)))
                stop("You must specify either a name or a set of tags.")

            if(!is.null(tags) | length(name)>1){
                runWithTags("rm", tags, name, !force)
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
                
        entries = function()
        {
            return(get("entries",thisEnv))
        },

        tag = function(name = NULL, newtags, tags = NULL)
        {
            if(!xor(is.null(name), is.null(tags)))
                stop("You must provide either name or tags.")
            if(!is.null(tags) | length(name)>1)
                runWithTags("tag", tags, name, F, newtags) else
                    get("this", thisEnv)$set(name, addtags=newtags)                   
        },


        untag = function(name = NULL, rmtags, tags = NULL)
        {
            if(!xor(is.null(name), is.null(tags)))
                stop("You must provide either name or tags.")
            if(!is.null(tags) | length(name)>1)
                runWithTags("untag", tags, name, F, rmtags) else {
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

        attach = function(filepath, description, tags, src=NULL, replace=F, to=NULL)
        {
            get("this", thisEnv)$put(filepath, basename(filepath),
                                     description, tags, src, replace=replace, asattach=T, to=to)
        },


        stash = function(name, rename = name, env=parent.frame())
            {
                if(!stopOnEmpty(T)){
                    e <- getEntry(rename)
                    if(!is.null(e))
                        if(!"stash" %in% e$tags)
                            stop(paste("A non-stash entry by the same name already exists,",
                                       "try setting the rename parameter."))
                }
            
            obj <- get(name, envir=env)
            get("this", thisEnv)$put(obj, rename, "Stashed object",
                                     c("stash", "hide"), replace=T)
        },

        stashclear = function(force=F)
        {
            get("this", thisEnv)$rm(tags=c("stash", "hide"), force=force)
        },

        
        put = function(obj, name, description, tags, src=NULL,
            depends=NULL, replace=F, asattach=F, to=NULL, addversion=F)
        {
            checkIndexUnchanged()
            
            if(missing(obj) | missing(name) | missing(description) | missing(tags))
                stop("You must provide all of: obj, name, description, tags.")

            if(!is.null(to))
                asattach <- T

            if(name == "repo")
                stop("Name repo is reserved.")
            
            notexist <- checkName(name)
            if(!notexist & !replace & !addversion)
                {                    
                    cat("Identifier already used.\n")
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

            if(!is.null(to))
                stopOnNotFound(to)

            fdata <- get("storeData", thisEnv)(name, obj, asattach)
            fname <- fdata[["path"]]
            fsize <- fdata[["size"]]

            if(is.null(src)) {
                storedfrom <- getwd()
            } else storedfrom <- src
            
            if(!all(sapply(storedfrom, checkName)))
                message("At least one provenance is internal.")
           
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
                          depends = depends,
                          attachedto = to)

            if(!notexist & addversion) {
                newname <- checkVersions(name)$new
                get("this", thisEnv)$set(name, newname=newname)
                get("this", thisEnv)$tag(newname, "hide")
            }            
            
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
    assign('entries', list(), envir=thisEnv)
    
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
                    } else message("Nothing done.")
                }

            storeIndex()
            message("Repo created.")
        }

    indexMD5 <- md5sum(repofile)
    
    return(me)
}

