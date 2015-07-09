\name{repo_test}
\alias{repo_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
repo_test(where = "./repotest")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{where}{
%%     ~~Describe \code{where} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (where = "./repotest") 
{
    repo <- repo_open("repodemo")
    src <- normalizePath("repo.R")
    myiris <- scale(as.matrix(iris[, 1:4]))
    rownames(myiris) <- iris$Species
    repo$put(myiris, "myiris", paste("A normalized version of the iris dataset coming with R.", 
        "Normalization is made with the scale function with default parameters."), 
        c("dataset", "iris", "repodemo"), src, replace = T)
    irispca <- princomp(myiris)
    iris2d <- irispca$scores[, c(1, 2)]
    pdf("iris2D.pdf")
    plot(iris2d, main = "2D visualization of the Iris dataset", 
        col = iris$Species)
    dev.off()
    repo$attach("iris2D.pdf", "Iris 2D visualization obtained with PCA.", 
        c("visualization", "iris", "repodemo"), src, replace = T, 
        to = "myiris")
    pdf("irispca.pdf")
    plot(irispca)
    dev.off()
    repo$attach("irispca.pdf", "Variance explained by the PCs of the Iris dataset", 
        c("visualization", "iris", "repodemo"), src, replace = T, 
        to = "iris2D.pdf")
    kiris <- kmeans(myiris, 5)$cluster
    repo$put(kiris, "iris_5clu", "Kmeans clustering of the Iris data, k=5.", 
        c("metadata", "iris", "kmeans", "clustering", "repodemo"), 
        src, "myiris", T)
    pdf("iris2Dkm.pdf")
    plot(iris2d, main = "Iris dataset kmeans clustering", col = kiris)
    dev.off()
    repo$attach("iris2Dkm.pdf", "Iris K-means clustering.", c("visualization", 
        "iris", "clustering", "kmeans", "repodemo"), src, replace = T, 
        to = "iris_5clu")
    res <- table(rownames(myiris), kiris)
    repo$put(res, "iris_cluVsSpecies", paste("Contingency table of the kmeans clustering versus the", 
        "original labels of the Iris dataset."), c("result", 
        "iris", "validation", "clustering", "repodemo"), src, 
        c("myiris", "iris_5clu"), T)
    return(repo)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
