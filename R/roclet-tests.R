register_tags(tests = parse.value)
tests_roclet <- function() {
    new_roclet(list, "tests")
}

roc_process.tests <- function(roclet, parsed, base_path,
                             options = list()) {
    have_tst <-  sapply(parsed[["blocks"]], function(x) "tests" %in% names(x))
    blocks <- mapply(function(prs, hastst) {
        if(hastst)
            prs
        else
            NULL
    }, SIMPLIFY=FALSE, prs = parsed[[2]], hastst = have_tst)

    blocks <- blocks[!sapply(blocks, is.null)]

    fils <-  sapply(blocks, function(x) x$srcref$filename)
    blocks <- lapply(blocks, function(x) x$tests)
    names(blocks) <- fils
    if(length(blocks) > 1)
        blocks <- tapply(blocks, fils, function(x) do.call(paste, c(sep="\n\n", x)))
    blocks
}

roc_output.tests <- function(roclet, results, base_path,
                            options = list(),
                            check = TRUE) {
    fil <- normalizePath(file.path(
        base_path, "tests", "tests_roxygen.R"))
    if(!dir.exists(file.path(base_path, "tests")))
        dir.create(file.path(base_path, "tests"))
    con = file(fil, open="w")
    on.exit(close(con))
    txt = c(made_by("#"),
            mapply(paste, x = sapply(names(results),
                                     test_file_ref),
                   y = results,
                   sep="\n\n"))
    writeLines(txt,
               con = con,
               sep="\n")
}

test_file_ref<- function( fil ) {
    paste("\n## Tests generated from roxygen comments in file:", file.path("R", basename(fil)))
}

        
