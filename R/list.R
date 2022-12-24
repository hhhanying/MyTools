#' used to output the strings to construct a list that is the same as rmd file params, notice currently only works for numeric and string
#' @export
assign_param <- function(param, line_break = TRUE) {
  sep <- ifelse(line_break, ", \n", ", ")
  res <- seq_along(params) %>%
    sapply(FUN = function(i)
      paste(
        names(params)[i],
        " = ", 
        ifelse(is.character(params[[i]]), "'",""),
        params[[i]],
        ifelse(is.character(params[[i]]), "'","")
        )
      ) %>%
    paste0(collapse = sep)
  return (res)
}

#' used to "release" variables in a list to the global environment
#' @export
var_in_list <- function(l) {
  lapply(seq_along(l),
         FUN = function(i) assign(x = names(l)[i],
                                  value = l[[i]],
                                  envir = .GlobalEnv))
}
