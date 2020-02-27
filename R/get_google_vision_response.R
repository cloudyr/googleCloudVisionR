#' @title Get parsed image annotations from the Google Cloud Vision API
#' @description Given a list of images, a feature type and the maximum number of responses,
#'   this functions calls the Google Cloud Vision API, and returns the image annotations in a data.table format.
#'
#' @param imagePaths character, file paths, URLs or Cloud Storage URIs of the images,
#'   can be a combination of all three
#' @param feature character, one out of: "LABEL_DETECTION", "FACE_DETECTION",
#'   "TEXT_DETECTION", "DOCUMENT_TEXT_DETECTION", "LOGO_DETECTION", "LANDMARK_DETECTION"
#' @param maxNumResults integer, the maximum number of results (per image) to be returned.
#' @param batchSize integer, the chunk size for batch processing
#' @param savePath character, if specified, results will be saved to this path (as .csv)
#'
#' @return a data frame with image annotation results
#'
#' @examples \dontrun{
#'     # Label Detection (default), with maximum 7 results returned per image
#'     imagePath <- system.file(
#'       "extdata", "golden_retriever_puppies.jpg", package = "googleCloudVisionR"
#'     )
#'     gcv_get_image_annotations(imagePaths = imagePath, maxNumResults = 7)
#'
#'     # Face detection
#'     imagePath <- system.file(
#'       "extdata", "arnold_wife.jpg", package = "googleCloudVisionR"
#'     )
#'     gcv_get_image_annotations(imagePaths = imagePath, feature = "FACE_DETECTION")
#'
#'     # Google Cloud Storage URI as input
#'     gcv_get_image_annotations("gs://vision-api-handwriting-ocr-bucket/handwriting_image.png")
#' }
#'
#' @export
#'
gcv_get_image_annotations <- function(imagePaths, feature = "LABEL_DETECTION",
                                      maxNumResults = NULL, batchSize = 64L,
                                      savePath = NULL) {
    validate_image_paths(imagePaths)
    validate_feature(feature)

    if (is.null(savePath) || !file.exists(savePath)) {
        imageAnnotations <- gcv_read_response(
            imagePaths, batchSize, feature, maxNumResults, savePath
        )
        return(imageAnnotations)  # Early return
    }

    validate_feature_consistency_with_cache(savePath, feature)

    annotationsFromFile <- data.table::fread(savePath)
    annotatedImagePaths <- unique(annotationsFromFile[["image_path"]])
    imagesToAnnotate <- unique(setdiff(imagePaths, annotatedImagePaths))

    if (length(imagesToAnnotate) > 0) {
        imageAnnotations <- gcv_read_response(
            imagesToAnnotate, batchSize, feature, maxNumResults, savePath
        )
        return(rbind(annotationsFromFile, imageAnnotations))
    } else {
        annotationsFromFile[image_path %in% imagePaths]
    }
}

gcv_read_response <- function(imagesToAnnotate,
                              batchSize,
                              feature,
                              maxNumResults,
                              savePath) {
    imagePathChunks <- split_to_chunks(imagesToAnnotate, batchSize)
    purrr::map(imagePathChunks, ~{
        gcvResponse <- gcv_get_response(.x, feature, maxNumResults) %>%
            .[, feature := feature]
        if (!is.null(savePath)) {
            data.table::fwrite(gcvResponse, savePath, append = TRUE)
        }
        gcvResponse
    }) %>% rbindlist()
}

#' @title helper function to split a vector to approximately equally sized chunks
#'
#' @param vec a vector
#' @param chunkSize integer, how long should the chunks be?
#'
#' @return a list of chunks
#'
split_to_chunks <- function(vec, chunkSize) {
    suppressWarnings(split(vec, ceiling(seq_along(vec) / chunkSize)))
}

#' @title Get parsed image annotations from the Google Cloud Vision API
#' @description Given a list of images, a feature type and the maximum number of responses,
#'   this functions calls the Google Cloud Vision API, and returns the image annotations in a data.table format.
#'
#' @inheritParams gcv_get_image_annotations
#'
#' @return a data frame with image annotation results
#'
#' @examples \dontrun{
#'     imagePath <- system.file(
#'       "extdata", "golden_retriever_puppies.jpg", package = "googleCloudVisionR"
#'     )
#'     gcv_get_response(imagePaths = imagePath, maxNumResults = 7)
#' }
#'
#' @export
#'
gcv_get_response <- function(imagePaths, feature, maxNumResults) {
    rawResponse <- gcv_get_raw_response(imagePaths, feature, maxNumResults)
    extract_response(
        rawResponse[["responses"]], imagePaths, feature
    )
}

#' @title Get raw API response from the Google Cloud Vision API
#' @description Given a list of images, a feature type and the maximum number of responses,
#'   this functions calls the Google Cloud Vision API, and returns the raw response from the API.
#'   For a friendlier response, refer to the `gcv_get_image_annotations` function, which returns
#'   results in a data.table format (however, the information returned is limited compared to the
#'   raw response).
#'
#' @inheritParams gcv_get_image_annotations
#'
#' @return a response object returned by the API. To get the image annotations, take the
#'   "content" element from the object
#'
#' @examples \dontrun{
#'     imagePath <- system.file(
#'       "extdata", "golden_retriever_puppies.jpg", package = "googleCloudVisionR"
#'     )
#'     raw_response <- gcv_get_raw_response(imagePaths = imagePath, maxNumResults = 7)
#'
#'     str(raw_response)
#'     raw_response
#' }
#'
#' @export
#'
gcv_get_raw_response <- function(imagePaths,
                                 feature = "LABEL_DETECTION",
                                 maxNumResults = NULL) {
    validate_image_paths(imagePaths)

    body <- create_request_body(imagePaths, feature, maxNumResults)
    call_vision_api(body)[["content"]]
}

#' @title helper function to create json for response request
#' @description creates a json output from the inputs
#'
#' @inheritParams gcv_get_image_annotations
#'
#' @return request body (payload), encoded as json
#'
create_request_body <- function(imagePaths, feature, maxNumResults) {
    imageRequests <- purrr::map(imagePaths, create_single_image_request, feature, maxNumResults)

    requests <- list(requests = imageRequests)
    jsonlite::toJSON(requests, auto_unbox = TRUE)
}

#' @title helper function to create a list of details of one image annotation request
#' @description creates a list output from the inputs
#'
#' @param imagePath character, file path, URL or Cloud Storage URI of the image
#' @inheritParams create_request_body
#'
#' @return list of request details for one image
#'
create_single_image_request <- function(imagePath, feature, maxNumResults) {
    if (grepl("^(http|https|gs)://", imagePath)) {
        image_in_request <- list(source = list(imageUri = imagePath))
    } else {
        image_in_request <- list(content = as.character(encode_image(imagePath)))
    }

    features_in_request <- list(type = feature)
    if (is.numeric(maxNumResults)) features_in_request[["maxResults"]] <- maxNumResults

    list(
        image    = image_in_request,
        features = features_in_request
    )
}

#' @title helper function to base64 encode the image file
#' @description base64 encodes an image file
#'
#' @param imagePath character, path to the image
#'
#' @return get the image back as encoded file
#'
encode_image <- function(imagePath) {
    jsonlite::base64_enc(
        readBin(imagePath, "raw", file.info(imagePath)[1, "size"])
    )
}

#' @title helper function to send POST request to the Google Vision API
#' @description sends the request defined in `body` to the API
#'
#' @param body, output of create_request_body()
#' @param apiEndpoint character, api endpoint
#' @param httpRequestType character, type of the http request
#'
#' @return API response in raw format
#'
call_vision_api <- function(body,
                            apiEndpoint = "images:annotate",
                            httpRequestType = "POST") {
    simple_call <- googleAuthR::gar_api_generator(
        baseURI = paste0("https://vision.googleapis.com/v1/", apiEndpoint),
        http_header = httpRequestType
    )
    simple_call(the_body = body)
}

#' @title helper function code to extract the response data.frame
#' @description a utility to extract features from the API response
#'
#' @param responses an API response object
#' @inheritParams gcv_get_image_annotations
#'
#' @return a data.table
#'
extract_response <- function(responses, imagePaths, feature) {
    featureType <- gcv_get_available_feature_types()[[feature]]
    errors <- data.table(image_path = imagePaths)
    annotations  <- data.table(image_path = imagePaths)

    if (!is.null(responses[["error"]])) {
        errors <- extract_error(responses, imagePaths)
    }
    if (feature == "PRODUCT_SEARCH") {
        annotations <- extract_annotations_for_product_search(
            responses, imagePaths
        )
    } else if (!is.null(responses[[featureType]])) {
        annotations <- extract_annotations(responses, imagePaths, featureType)
    }
    merge(annotations, errors, by = "image_path", sort = FALSE)
}

#' @title helper function code to extract the annotations
#' @description a utility to extract features from the API response
#'
#' @param responses an API response object
#' @param featureType the type of annotation as called in the response object
#' @inheritParams gcv_get_image_annotations
#'
#' @return a data.table
#'
extract_annotations <- function(responses, imagePaths, featureType) {
    purrr::map2(responses[[featureType]], imagePaths, ~{
        responseDT <- extractor(featureType)(.x)
        responseDT[["image_path"]] <- .y

        responseDT
    }) %>%
        data.table::rbindlist(fill = TRUE) %>%
        data.table::setcolorder("image_path")
}

#' @title helper function code to extract error from API response into a data.table
#'
#' @inheritParams gcv_get_image_annotations
#' @param responses an API response object
#'
#' @return a data.table
#'
extract_error <- function(responses, imagePaths) {
    data.table(
        image_path = imagePaths,
        error_code = responses[["error"]][["code"]],
        error_message = responses[["error"]][["message"]]
    )
}

#' @title helper function code to provide an extractor function for different feature types
#' @description a utility to provide functions to extract features from the API response
#'
#' @inheritParams extract_annotations
#'
#' @return a function
#'
extractor <- function(featureType) {
    if (featureType == "labelAnnotations") {
        label_detection_extractor
    } else if (featureType == "textAnnotations") {
        ocr_extractor
    } else if (featureType == "faceAnnotations") {
        face_detection_extractor
    } else if (featureType == "logoAnnotations") {
        logo_detection_extractor
    } else if (featureType == "landmarkAnnotations") {
        landmark_detection_extractor
    } else {
        stop("Unrecognized feature type")
    }
}

#' @title helper function code to extract API response into a data.table for given feature type
#'
#' @param response an element of the API response object
#'
#' @return a data.table
#'
label_detection_extractor <- function(response) {
    if (is.null(response)) {
        data.table::data.table(
            mid         = NA_character_,
            description = NA_character_,
            score       = NA,
            topicality  = NA
        )
    } else {
        data.table::as.data.table(response)[, c("mid", "description", "score", "topicality")]
    }
}

#' @title helper function code to extract API response into a data.table for given feature type
#'
#' @param response an element of the API response object
#'
#' @return a data.table
#'
ocr_extractor <- function(response) {
    # note: currently we only return the "detailed" results, that's why the subsetting
    nRows <- nrow(response)
    data.table::data.table(
        description = response[["description"]][2:nRows],
        get_bounding_boxes(response)[2:nRows]
    )
}

#' @title helper function code to extract API response into a data.table for given feature type
#'
#' @param response an element of the API response object
#'
#' @return a data.table
#'
face_detection_extractor <- function(response) {
    boundingBoxes <- get_bounding_boxes(response)

    cbind(
        boundingBoxes,
        data.table::as.data.table(response[,
            c("detectionConfidence", "landmarkingConfidence", "joyLikelihood",
              "sorrowLikelihood", "angerLikelihood", "surpriseLikelihood",
              "underExposedLikelihood", "blurredLikelihood", "headwearLikelihood")
        ]) %>%
        setnames(
            c("detection_confidence", "landmarking_confidence",
            "joy_likelihood", "sorrow_likelihood", "anger_likelihood", "surprise_likelihood",
            "under_exposed_likelihood", "blurred_likelihood", "headwear_likelihood")
        )
    )
}

#' @title helper function code to extract API response into a data.table for given feature type
#'
#' @param response an element of the API response object
#'
#' @return a data.table
#'
logo_detection_extractor <- function(response) {
    boundingBoxes <- get_bounding_boxes(response)

    cbind(
        data.table::as.data.table(response[, c("mid", "description", "score")]),
        boundingBoxes
    )
}

#' @title helper function code to extract API response into a data.table for given feature type
#'
#' @param response an element of the API response object
#'
#' @return a data.table
#'
landmark_detection_extractor <- function(response) {
    boundingBoxes <- get_bounding_boxes(response)

    geoCoordinates <- purrr::map(response[["locations"]], ~{
        as.data.table(.x[["latLng"]])
    }) %>% data.table::rbindlist()

    cbind(
        data.table::as.data.table(response)[, c("mid", "description", "score")],
        boundingBoxes,
        geoCoordinates
    )
}

#' @title helper function code to extract Bounding Box x,y coordinates for an API response element
#'
#' @param response an element of the API response object
#'
#' @return a data.table
#'
get_bounding_boxes <- function(response) {
    purrr::map(response[["boundingPoly"]]$vertices, ~{
        data.table(
            x = paste(.x[["x"]], collapse = ", "),
            y = paste(.x[["y"]], collapse = ", ")
        )
    }) %>% data.table::rbindlist()
}
