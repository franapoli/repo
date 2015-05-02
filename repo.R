library(digest)
REPOFNAME <- "R_repo.RDS"

repo <- function(root="~/.R_repo")
{
      storeIndex = function()
          {
              fname <- get("repofile", thisEnv)
              saveRDS(get("entries", thisEnv), fname)
          }

      thisEnv <- environment()
      assign("defFlags", NULL)
      assign("entries", list(), thisEnv)      
      
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
              message(paste0("Repo root doesn't exist, creating \"", root, "\"."))
              dir.create(root)
          }

      ## Create the list used to represent an
      ## object for this class
      me <- list(
          ## Define the environment where this list is defined so
          ## that I can refer to it later.
          thisEnv = thisEnv,

          list = function()
          {

              entr <- get("entries",thisEnv)

              message(paste0(
                  "Name", "\t",
                  "Dim", "\t",
                  "Time", "\t",
                  "From"))
              
              for(i in 1:length(entr))
                  {
                      entri <- entr[[i]]
                      message(
                          paste0(
                              entri$name,"\t",
                              paste(entri$dims, collapse="x"), "\t",
                              entri$timestamp, "\t",
                              entri$storedfrom
                              )
                          )
                  }

          },

          setDefaultTags = function(tags)
          {
              return(assign("defFlags",tags,thisEnv))
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

          add = function(obj, name, description="", tags="")
          {
              entr <- get("entries", thisEnv)
              
              if(!is.null(dim(obj)))
                  dims <- dim(matrix(runif(12),3,3)) else
              dims <- length(name)
              flags <- c(get("defFlags", thisEnv), tags, class(obj))
              fname <- file.path(root, digest(name))
              repoE <- list(name = name,
                            description = description,
                            tags = tags,
                            class = class(obj),
                            dims = dims,
                            timestamp = Sys.time(),
                            storedfrom = getwd(),
                            path = fname)

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
          },

          setRoot = function(value)
          {
              return(assign("root",value,thisEnv))
          }
          )

      ## Define the value of the list within the current environment.
      assign('this',me,envir=thisEnv)

      ## Set the name for the class
      class(me) <- append(class(me),"repo")

      if(!file.exists(repofile))
          {
              message(paste0("Repo index doesn't exist, creating \"", root, "\"."))
           saveRDS(get("entries", thisEnv), repofile)
          }

      
      return(me)
  }
