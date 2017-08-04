library(knitr)
print("Cleaning README-*.png in ../inst...")
system("rm ../inst/README-*.png")

print("Cleaning README-*.png in here...")
system("rm inst/README-*.png")

print("kniting...")
knit("README.Rmd")

print("Moving pngs to ../inst")
system("mv inst/README-*.png ../inst")

print("Removing inst from here...")
system("rmdir inst")

print("Moving README.md ..")
system("mv README.md ..")
