## library(roxygen2)
## package.skeleton('repo', code_files='repoS3.R', force=F)

## # `R CMD roxygen -d helloRoxygen' works, too.
## roxygenize('helloRoxygen', roxygen.dir='helloRoxygen', copy.package=FALSE, unlink.target=FALSE)

#' Check repo's integrity.
#' 
#' @param repo An object of class repo.
#' @return Used for side effects.
#' @examples
#' repo$check()
#' ## Checking item1... ok.
#' ## Checking item2... ok.
#' ## Checking item3... ok.
#' ## Checking item4... ok.
#' ## Checking item5... ok.
repo_check <- function(repo)
    repo$check()

#' Provides alternative access to repo items.
#' 
#' @param repo An object of class repo.
#' @return A list of functions.
#' @examples
#' repo <- repo_test()
#' h <- repo_handlers(repo)
#' ## handlers have the same names as the items in the repo.
#' names(h)
#' ## Without arguments, function "item1" loads item named "item1".
#' i1 <- h$item1()
#' ## Arguments can be used to call other repo functions on the item.
#' h$item1("info")
repo_handlers <- function(repo)repo$handlers()

#' Export repo's items to RDS file.
#' 
#' @param repo An object of class repo.
#' @param name Name of the item to export.
#' @param where Destination directory
#' @param tags List of tags: all items tagged with all the tags in the
#' list will be exported.
#' @return TRUE on success, FALSE otherwise.
#' @examples
#' repo_export(repo, "item1")
repo_export <- function(repo, name, where=".", tags=NULL)
    repo$export(name, where, tags)

#' Provides detailed information about an item.
#' 
#' @param repo An object of class repo.
#' @param name Item name. If both name and tags are NULL, information
#' about the whole repo will be provided.
#' @param tags List of tags: info will run on all items matching the tag list.
#' @return Used for side effects.
#' @examples
#' repo_info(repo, name="item1")
repo_info <- function(repo, name = NULL, tags = NULL)
    repo$info(name, tags)

#' Remove item from the repo (and the disk).
#' 
#' @param repo An object of class repo.
#' @param name An item's name.
#' @param tags A list of tags: all items matching the list will be
#' removed.
#' @return Used for side effects.
#' @examples
#' repo_rm(repo, "item1")
repo_rm <- function(repo, name = NULL, tags = NULL)
    repo$rm(name, tags)

#' Retrieve an item from the repo.
#' 
#' @param repo An object of class repo.
#' @param name An item's name.
#' @return The previously stored object.
#' @examples
#' x <- repo_get(repo, "item1")
repo_get <- function(repo, name)repo$get(name)

#' Low-level list of item entries.
#' 
#' @param repo An object of class repo.
#' @return A list of item entries.
#' @examples
#' l <- repo_entries(repo)
repo_entries <- function(repo)repo$entries()

#' Add tags to an item.
#' 
#' @param repo An object of class repo.
#' @param name An item name.
#' @param newtags A list of tags that will be added to the item's tag
#' list.
#' @param tags A list of tags: newtags will be added to all items
#' matching the list.
#' @return Used for side effects.
#' @examples
#' repo_tag(repo, "item1", c("tag1", "tag2"))
repo_tag <- function(repo, name = NULL, newtags, tags = NULL)
    repo$tag(name, newtags, tags)

#' Remove tags from an item.
#' 
#' @param repo An object of class repo.
#' @param name An item name.
#' @param rmtags A list of tags that will be removed from the item's
#' tag list.
#' @param tags A list of tags: rmtags will be removed from all items
#' matching the list.
#' @return Used for side effects.
#' @examples
#' repo_untag(repo, "item1", c("tag2"))
repo_untag <- function(repo, name = NULL, rmtags, tags = NULL)
    repo$untag(name, rmtags, tags)

#' Edit an existing item.
#' 
#' @param repo An object of class repo.
#' @param name An item name.
#' @param obj An R object to replace the one currently associated with the item.
#' @param newname Newname of the item.
#' @param description Item's description.
#' @param tags New item's tags as a ist of character.
#' @param src New item's provenance as a list of character.
#' @param addtags Tags to be added to current item's tags. Can not be
#' used together with the parameter "tags".
#' @return Used for side effects.
#' @examples
#' ###code
repo_set <- function(repo, name, obj=NULL, newname=NULL, description=NULL, tags=NULL, src=NULL, addtags=NULL)
    repo$set(name, obj, newname, description, tags, src, addtags)

#' Create a new item in the repo.
#' 
#' @param repo An object of class repo.
#' @param obj An R object to store in the repo.
#' @param name A character identifier for the new item.
#' @param description A character description of the item.
#' @param tags A list of tags to sort the item. Tags are useful for
#' selecting sets of items and run bulk actions.
#' @param src The item's provenance as a list of character. Usually
#' the name of the script producing the stored object, a website where
#' the object was downloaded, and so on. If one of the provenance
#' strings matches the name of a repo's item, this will create a
#' dependency link.
#' @param replace If the item exists, overwrite the specified fields.
#' @return Used for side effects.
#' @examples
#' ###code
repo_put <- function(repo, obj, name, description, tags, src=NULL, replace=F)
    repo$put(obj, name, description, tags, src, replace)

#' Create a new item from an existing file.
#' 
#' @param repo An object of class repo.
#' @param filepath The path to the file to be stored in the repo.
#' @param descritpion A character description of the item.
#' @param tags A list of tags to sort the item. Tags are useful for
#' selecting sets of items and run bulk actions.
#' @param src The item's provenance as a list of character. Usually
#' the name of the script producing the stored object, a website where
#' the object was downloaded, and so on. If one of the provenance
#' strings matches the name of a repo's item, this will create a
#' dependency link.
#' @param replace If the item exists, overwrite the specified fields.
#' @param to An existing item name to attach the file to.
#' @return Used for side effects.
#' @examples
#' ###code
repo_attach <- function(repo, filepath, description, tags, src=NULL, replace=F, to=NULL)
    repo$attach(filepath, description, tags, src, replace, to)

#' Append text to an existing item content.
#' 
#' @param repo An object of class repo.
#' @param id The name of an item whose object is of class character.
#' @param txtorfunc Text to be appended to the item's object. It can
#' also be a on object of class function: in this case, its source is
#' appended.
#' @return Used for side effects.
#' @examples
#' ###code
repo_append <- function(repo, id, txtorfunc)
    repo$append(id, txtorfunc)

#' Copy resource to another repo
#' 
#' @param repo An object of class repo.
#' @return Used for side effects.
#' @examples
#' ###code
repo_copy <- function()
    repo$append(id, txtorfunc)


#' Description
#' 
#' @param repo An object of class repo.
#' @return Path to the root of the repo.
#' @examples
#' ###code
repo_root <- function(repo)
    repo$root()

#' Show a summary of the repository contents.
#'
#' @param repo An object of class repo.
#' @param tags A list of character tags. Only items matching all the
#' tags will be shown.
#' @param all Show also items tagged with "hide".
#' @return Used for side effects.
print.repo <- function(repo, tags=NULL, all=F)
    repo$print(tags, all)


#' Build dependency graph
#'
#' @param repo An object of class repo.
#' @param depends 
#' @param attached 
#' @param generated 
#' @param plot 
#' @return Used for side effects.
repo_dependencies <- function(repo, depends=T, attached=T, generated=T, plot=T)
    repo$dependencies(depends, attached, generated, plot)

#' Copy items to another repo
#'
#' @param repo An object of class repo.
#' @param destrepo
#' @param name
#' @param tags
#' @return Used for side effects.
repo_copy <- function(repo, destrepo, name, tags=NULL)
    repo$copy(destrepo, name, tags)

#' List all tags
#'
#' @param repo An object of class repo.
#' @return Used for side effects.
repo_tags <- function(repo)
    repo$tags()

#' Run system call on item
#'
#' @param repo An object of class repo.
#' @return Used for side effects.
repo_sys <- function(repo, name, command)
    repo$sys(name, command)

#' Quickly store temporary data
#'
#' @param repo An object of class repo.
#' @return Used for side effects.
repo_stash <- function(repo, name)
    repo$stash(name)

#' Remove stashed data
#'
#' @param repo An object of class repo.
#' @return Used for side effects.
repo_stashclear <- function(repo)
    repo$stashclear()
