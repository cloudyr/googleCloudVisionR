gcv_get_product_sets <- function(projectId, locationId) {
    response <- call_vision_api(
        body = NULL,
        apiEndpoint = glue::glue(
            "projects/{projectId}/locations/{locationId}/productSets"
        ),
        httpRequestType = "GET"
    )

    data.table(response[["content"]][["productSets"]][1:3])
}

gcv_get_product_set <- function(projectId,
                                locationId,
                                productSetId) {
    response <- call_vision_api(
        body = NULL,
        apiEndpoint = glue::glue(
            "projects/{projectId}/locations/{locationId}/productSets/{productSetId}"
        ),
        httpRequestType = "GET"
    )

    as.data.table(response[["content"]])
}
