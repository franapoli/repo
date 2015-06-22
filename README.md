# repo
*Thou Shalt Not save() or load()*

This package implements an R objects repository manager. Instead of manually storing and retrieving R objects into and from the appropriate directories and files, repo offers an abstraction layer that takes care of storing, retrieving and sorting the objects. For each stored object, you provide a name, a description, a list of tags and optional dependence and provenance information. Repo will store the object together with all the metadata an provide easy access to it. Repo can display a compact list of all the items stored and provide search by tags, load in the workspace, remove from disk, export to RDS files, edit metadata, manage provenance traces, show dependency graph and more.

[work in progress]
