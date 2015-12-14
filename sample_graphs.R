source_https <- function(url, ...) {
    # load package
    require(RCurl)
    
    # parse and evaluate each .R script
    tmp <- sapply(c(url, ...), function(u) {
        eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
    })
    message(sprintf("%s loaded into global environment", paste(substr(names(tmp), max(grep("/", strsplit(names(tmp), "")[[1]]))+1, nchar(names(tmp))), collapse=", ")))
}

# Example
source_https("https://raw.githubusercontent.com/jachan1/plotfxn.bar/master/bar_bridges.R")


## example data -- long
ds1 <- data.frame(x_grp = sample(letters[1:3], 100, replace=T), leg_grp=sample(letters[1:2], 100, replace=T), 
                  rvalue=runif(100, 0, 100))

## example pre-processing -- means
pds1 <- ds1 %>% group_by(x_grp, leg_grp) %>% 
    summarise(value=mean(rvalue), plab="p-value=0.1", blab=sprintf("%1.0f%%", value))

## plot with proportions
plotfxn.bar(pds1, title="Plot", ylab="Prop Over 60", xlab="X label", legtitle="Adherence", blabcol="white")
plotfxn.bar(pds1, title="Plot", ylab="Prop Over 60", xlab="X label", legtitle="Adherence", blabcol="white", addblab = F)


## example pre-processing -- props
pds2 <- ds1 %>% group_by(x_grp, leg_grp) %>% 
    summarise(value=100*sum(rvalue > 60)/n(), plab="p-value=0.1", blab=sprintf("%1.0f%%", value))

## Plot with means
plotfxn.bar(pds2, title="Plot", ylab="Mean Value", xlab="X label", legtitle="Adherence", blabcol="white", max = 75)
