abundance <- function (censdata, type = "abund", alivecode = c("A"), mindbh = NULL, 
    dbhunit = "mm", split1 = NULL, split2 = NULL) 
{
    if (is.null(split1)) 
        split1 = rep("all", dim(censdata)[1])
    if (is.null(split2)) 
        split2 = rep("all", dim(censdata)[1])
    if (!is.null(mindbh)) 
        inc = censdata$dbh >= mindbh
    else inc = rep(TRUE, length(censdata$dbh))
    alive = rep(FALSE, length(censdata$dbh))
    for (i in 1:length(alivecode)) alive[censdata$status == alivecode[i]] = TRUE
    class1 = sort(unique(split1))
    class2 = sort(unique(split2))
    groupvar = list(split1[alive & inc], split2[alive & inc])
    if (type == "abund") 
        abund = tapply(censdata$dbh[alive & inc], groupvar, length)
    else if (type == "ba") 
        abund = tapply(censdata$dbh[alive & inc], groupvar, basum, 
            mindbh = mindbh, dbhunit = dbhunit)
    else if (type == "agb") 
        abund = tapply(censdata$agb[alive & inc], groupvar, sum, 
            na.rm = TRUE)
    meandate = tapply(censdata$date[alive & inc], groupvar, mean, 
        na.rm = TRUE)
    abund = fill.dimension(abund, class1, class2)
    meandate = fill.dimension(meandate, class1, class2, fill = NA)
    result = list(abund = abund, meandate = meandate)
    if (type == "ba") 
        names(result)[1] = "ba"
    else if (type == "agb") 
        names(result)[1] = "agb"
    return(result)
}