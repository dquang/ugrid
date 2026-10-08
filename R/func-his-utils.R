getVarLoc <- function(varTbl, idTbl, variables, locations) {
    vTbl <- varTbl[name %in% variables]
    iTbl <- idTbl[id %in% locations]
    vuTbl <- varTbl[name %in% variables, c("name", "location")] |> unique()
    # check whether selected variables and locations belong together
    ldiff <- setdiff(vuTbl$location, unique(iTbl$location))
    if (length(ldiff) > 0) {
        voLoc <- vTbl[location %in% ldiff, unique(long_name)]
        if (length(voLoc) > 0) {
            shiny::showNotification(paste0("Following variable(s) do not have associated locations: ",
                                           paste(voLoc, collapse=", ")))
        }
        loVar <- iTbl[location %in% ldiff, unique(id)]
        if (length(loVar) > 0) {
            shiny::showNotification(paste0("Following location(s) do not have associated variables: ",
                                           paste(loVar, collapse=", ")))
        }
        iTbl <- iTbl[!location %in% ldiff]
        vTbl <- vTbl[!location %in% ldiff]
        vuTbl <- vuTbl[!location %in% ldiff]
    }
    ret <- list(vTbl=vTbl, iTbl=iTbl, vuTbl=vuTbl)

    return(ret)
}
