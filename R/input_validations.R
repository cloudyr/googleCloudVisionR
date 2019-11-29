validate_image_paths <- function(imagePaths) {
    invalid_paths <- get_invalid_image_paths(imagePaths)
    if (length(invalid_paths) > 0) {
        stop(paste0("Invalid elements in imagePath: ", paste(invalid_paths, collapse = " ")))
    }
}

#' @title helper function to validate input image paths
#'
#' @param vec a vector of paths
#'
#' @return vector of invalid paths from @vec
#'
get_invalid_image_paths <- function(vec) {
    is_valid <- purrr::map_lgl(vec, ~{(grepl("^(http|https|gs)://", .x)) || file.exists(.x)})
    vec[!is_valid]
}

validate_feature <- function(feature) {
    if (!(feature %in% names(get_available_feature_types()))) {
        stop(paste0("Invalid feature: ", feature, " - it should be one of: ",
            paste0(paste0("'", names(get_available_feature_types()), "'"), collapse = ", ")))
    }
}

validate_feature_consistency_with_cache <- function(savePath, feature) {
    annotationsFromFile <- data.table::fread(savePath, nrows = 2)

    featureTypeInCache <- annotationsFromFile[1, feature]
    if (featureTypeInCache != feature) {
        stop(glue::glue(
            "{savePath} was already used for '{featureTypeInCache}' feature type,",
            "thus inconsistent with the now requested type ('{feature}')."
        ))
    }
}
