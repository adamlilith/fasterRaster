    srcIn <- src
    src <- .makeSourceName("v_category", "vector")
    rgrass::execGRASS("v.category", input = srcIn, output = src, option = "del", cat = -1, flags = c("quiet", "overwrite"))

    srcIn <- src
    src <- .makeSourceName("v_category", "vector")
    rgrass::execGRASS("v.category", input = srcIn, output = src, option = "add", flags = c("quiet", "overwrite"))

