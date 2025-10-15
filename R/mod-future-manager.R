#' Future Manager
#'
#' A class to manage future calls.
#' @keywords internal
FM <- R6::R6Class(
    "FM",
    public=list(
        #' @description
        #' Initialize the object and also a future plan.
        #' @param nCores Number of core to use.
        initialize=function() {
            pl <- future::plan()
            if (!is(pl, "multisession"))
                pl <- future::plan(future::multisession)
            self$plan <- pl
        },
        #' description
        #' Create a future by using `future::futureCall` and add it to the register.
        #' @param label a character string to name the call and label the future.
        #' @param fun a function to be evaluated.
        #' @param args a list of arguments passed to the function.
        #' @param force Logical value. Should the existing call be replaced?
        #' @param ... Additional arguments passed to `future::futureCall`.
        call=function(label, fun, args=list(), force=FALSE, ...) {
            chk <- (!label %in% self$labels)
            if (chk | force) {
                self$reg[[label]] <- future::futureCall(FUN=fun, args=args, label=label, ...)
                if (chk)
                    self$labels <- c(self$labels, label)
            } else {
                message("Call with label: ", label, "is already registered!. Use force=TRUE to replace it!")
            }
            invisible(self$reg[[label]])
        },
        #' description
        #' Check the status of a future call.
        #' @param label a character string named the call.
        result=function(label) {

            ret <- NULL
            if (!label %in% self$labels) {
                message("Future with name: ", label, " was not registered!")
            } else if (future::resolved(self$reg[[label]])){
                ret <- future::result(self$reg[[label]])
            }
            return(ret)
        },
        #' @field plan the future plan
        plan=NULL,
        #' @field reg the future register - a list of all called futures.
        reg=list(),
        #' @field labels a character vector of labels of the called futures.
        labels=character(0)
        )
    )
