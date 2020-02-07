gcv_import_products_from_csv <- function(storagePath, projectId, locationId) {

    request <- list(inputConfig = list(gcsSource = list(csvFileUri = storagePath)))
    requestBody <- jsonlite::toJSON(request, auto_unbox = TRUE)

    response <- call_vision_api(
        body = requestBody,
        apiEndpoint = glue::glue(
            "projects/{projectId}/locations/{locationId}/productSets:import"
        )
    )

    responseStatusCode <- response[["status_code"]]
    if (responseStatusCode == 200) {
        message("Bulk product request successful. Status code 200")
    } else {
        message(glue::glue(
            "Bulk product request returned status code '{responseStatusCode}'"
        ))
    }

    operationId <- gsub(".*/([a-z0-9]+)$", "\\1", response[["content"]])
    operationId
}

gcv_get_operation_status <- function(locationId, operationId) {
    response <- call_vision_api(
        body = NULL,
        apiEndpoint = glue::glue("locations/{locationId}/operations/{operationId}"),
        httpRequestType = "GET"
    )

    response[["content"]][["metadata"]]
}

gcv_delete_product_set <- function(productSetId, projectId, locationId) {
    response <- call_vision_api(
        body = NULL,
        apiEndpoint = glue::glue(
            "projects/{projectId}/locations/{locationId}/productSets/{productSetId}"
        ),
        httpRequestType = "DELETE"
    )

    responseStatusCode <- response[["status_code"]]
    if (responseStatusCode == 200) {
        message("Product set deletion request successful. Status code 200")
    } else {
        message(glue::glue(
            "Product set deletion request returned status code '{responseStatusCode}'"
        ))
    }

    return(invisible())
}

gcv_purge_product_set <- function(productSetId, projectId, locationId) {
    request <- list(
        force = "true",
        productSetPurgeConfig = list(productSetId = productSetId)
    )
    requestBody <- jsonlite::toJSON(request, auto_unbox = TRUE)
    response <- call_vision_api(
        body = requestBody,
        apiEndpoint = glue::glue(
            "projects/{projectId}/locations/{locationId}/products:purge"
        )
    )

    responseStatusCode <- response[["status_code"]]
    if (responseStatusCode == 200) {
        message("Product set purge request successful. Status code 200")
    } else {
        message(glue::glue(
            "Product set purge request returned status code '{responseStatusCode}'"
        ))
    }

    operationId <- gsub(".*/([a-z0-9]+)$", "\\1", response[["content"]])
    operationId
}
