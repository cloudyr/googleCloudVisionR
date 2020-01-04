#' @title Get parsed results from the Vision API Product Search
#' @description Given an image, and search parameters, this functions calls the
#'   Google Cloud Vision API, and returns formatted response from the API.
#'   Results are not complete! If you are looking for full results, refer to the
#'   `gcv_get_product_search_raw_response()` function
#'
#' @param imagePath character, file path, URLs or Cloud Storage URI of the image
#' @param projectId character, GCP project id
#' @param locationId character, GCP location id
#' @param productSetId character, product set id for Product Search
#' @param productCategory character, roduct category for Product Search
#' @param maxNumResults integer, the maximum number of results (per image) to be returned.
#'
#' @return a data frame with product search results
#'
#' @export
#'
gcv_get_search_product_response <- function(imagePath,
                                            projectId,
                                            locationId,
                                            productSetId,
                                            productCategory,
                                            maxNumResults){

    feature <- "PRODUCT_SEARCH"
    rawResponse <- gcv_get_product_search_raw_response(
        imagePath,
        projectId, locationId, productSetId, productCategory,
        maxNumResults
    )
    extract_response(
        rawResponse[["content"]][["responses"]],
        imagePath, feature = feature
    ) %>% .[, feature := feature]
}

#' @title Get raw results from the Vision API Product Search
#' @description Given an image, and search parameters, this functions calls the
#'   Google Cloud Vision API, and returns an unformatted response (JSON) from the API.
#'   If you are looking for a friendlier result, refer to the
#'   `gcv_get_product_search_response()` function
#'
#' @inheritParams gcv_get_search_product_response
#'
#' @return a list with product search results
#'
#' @export
#'
gcv_get_product_search_raw_response <- function(imagePath,
                                                projectId,
                                                locationId,
                                                productSetId,
                                                productCategory,
                                                maxNumResults) {
    validate_image_paths(imagePath)

    body <- create_request_body_for_product_search(
        imagePath,
        projectId, locationId, productSetId, productCategory,
        maxNumResults
    )
    call_vision_api(body)
}

create_request_body_for_product_search <- function(imagePath,
                                                   projectId,
                                                   locationId,
                                                   productSetId,
                                                   productCategory,
                                                   maxNumResults) {
    imageRequests <- create_single_product_search_request(
        imagePath,
        projectId, locationId, productSetId, productCategory,
        maxNumResults
    )

    requests <- list(requests = imageRequests)
    jsonlite::toJSON(requests, auto_unbox = TRUE)
}

create_single_product_search_request <- function(imagePath,
                                                 projectId,
                                                 locationId,
                                                 productSetId,
                                                 productCategory,
                                                 maxNumResults) {
    if (grepl("^gs://", imagePath)) {
        image_in_request <- list(source = list(gcsImageUri = imagePath))
    } else {
        image_in_request <- list(content = as.character(encode_image(imagePath)))
    }

    features_in_request <- list(type = "PRODUCT_SEARCH")
    if (is.numeric(maxNumResults)) features_in_request[["maxResults"]] <- maxNumResults

    image_context_in_request <- list(
        productSearchParams = list(
            productSet = glue::glue(
                "projects/{projectId}/locations/{locationId}/productSets/{productSetId}"
            ),
            productCategories = list(productCategory)
            # , filter = "" #"style = womens"
        )
    )

    list(
        image        = image_in_request,
        features     = features_in_request,
        imageContext = image_context_in_request
    )
}

extract_annotations_for_product_search <- function(responses, imagePath) {
    data.table::data.table(
        responses[["productSearchResults"]][["results"]][[1]][["product"]]
    ) %>%
        .[, image_path := imagePath] %>%
        data.table::setcolorder("image_path")
}
