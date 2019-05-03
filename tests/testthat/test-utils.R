create_response <- function(annotation_successful) {
  response <- list()

  if (!all(annotation_successful)) {
    errors <- purrr::map_if(
      annotation_successful, function(x) x == FALSE,
        ~{data.frame(code = 3, message = "Nice error message.", stringsAsFactors = FALSE)},
        .else = ~{data.frame(code = NA, message = NA)}
    ) %>% data.table::rbindlist() %>% as.data.frame
    response[["error"]] <- errors
  }

  if (any(annotation_successful)) {
    labelAnnotations <- purrr::map_if(
      annotation_successful,
      function(x) x == TRUE,
      ~{data.frame(mid = "foo", description = "bar", score = 0.987, stringsAsFactors = FALSE)},
      .else = ~{NULL}
    )
    response[["labelAnnotations"]] <- labelAnnotations
  }
  response
}
