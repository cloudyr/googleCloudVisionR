#' @title Calling Google's Cloud Vision API
#' @description Given a list of images, a feature type and the maximum number of responses,
#'   this functions calls the Google Cloud Vision API, and returns the image annotations.
#'
#' @param imagePaths string, paths or urls to the images
#' @param feature string, one out of: "FACE_DETECTION", "LANDMARK_DETECTION",
#'   "LOGO_DETECTION", "LABEL_DETECTION", "TEXT_DETECTION"
#' @param maxNumResults integer, the maximum number of results (per image) to be returned.
#' @param batchSize integer, the chunk size for batch processing
#' @param savePath string, if specified, results will be saved to this path (as .csv)
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
#'     # Logo detection
#'     imagePath <- system.file(
#'       "extdata", "brandlogos.png", package = "googleCloudVisionR"
#'     )
#'     gcv_get_image_annotations(imagePaths = imagePath, feature = "LOGO_DETECTION")
#' }
#'
#' @export
#'
gcv_get_image_annotations <- function(imagePaths, feature = "LABEL_DETECTION",
                                      maxNumResults = NULL, batchSize = 64L,
                                      savePath = NULL) {

  annotatedImagePaths <- ""
  if(!is.null(savePath) && file.exists(savePath)) {
    annotationsFromFile <- data.table::fread(savePath)
    annotatedImagePaths <- unique(annotationsFromFile[["image_path"]])
  }
  imagesToAnnotate <- unique(setdiff(imagePaths, annotatedImagePaths))

  if(length(imagesToAnnotate) > 0) {
    imagePathChunks <- split_to_chunks(imagesToAnnotate, batchSize)
    imageAnnotations <- data.table::rbindlist(purrr::map(imagePathChunks, ~{
      gcvResponse <- gcv_get_response(.x, feature, maxNumResults)

      if(!is.null(savePath)) data.table::fwrite(gcvResponse, savePath, append = TRUE)

      gcvResponse
    }))
  }

  if(exists("annotationsFromFile") && exists("imageAnnotations")) {
    rbind(annotationsFromFile, imageAnnotations)
  } else if (exists("annotationsFromFile")) {
    annotationsFromFile
  } else {
    imageAnnotations
  }
}

#' @title helper function to call the API for one batch of images
#'
#' @inheritParams gcv_get_image_annotations
#'
#' @return a data frame with image annotation results
#'
gcv_get_response <- function(imagePaths, feature, maxNumResults){

  encodedImages <- sapply(imagePaths, image_to_text)
  body <- create_request_body(encodedImages, feature, maxNumResults)
  rawResponse <- call_vision_api(body)
  extract_response(
    rawResponse[["content"]][["responses"]],
    imagePaths,
    feature
  )
}

#' @title helper function to base64 encode the image file
#' @description base64 encodes an image file
#'
#' @param imagePath string, path or url to the image
#'
#' @return get the image back as encoded file
#'
image_to_text <- function(imagePath) {

  if (grepl("^http", imagePath)) {
    content <- RCurl::getBinaryURL(imagePath)
    encodedImage <- RCurl::base64Encode(content, "txt")
  } else {
    encodedImage <- RCurl::base64Encode(readBin(
      imagePath, "raw", file.info(imagePath)[1, "size"]), "txt"
    )
  }
  encodedImage 
}

#' @title helper function to create json for response request
#' @description creates a json output from the inputs
#'
#' @param encodedImages character, elements are outputs of image_to_text()
#' @inheritParams gcv_get_image_annotations
#'
#' @return request body (payload), encoded as json
#'
create_request_body <- function(encodedImages, feature, maxNumResults) {

  names(encodedImages) <- NULL
  requests <- list(
    requests = lapply(encodedImages, create_single_image_request, feature, maxNumResults)
  )
  jsonlite::toJSON(requests, auto_unbox = TRUE)
}

#' @title helper function to create a list of details of one image annotation request
#' @description creates a list output from the inputs
#'
#' @inheritParams create_request_body
#'
#' @return list of request details for one image
#'
create_single_image_request <- function(encodedImages, feature, maxNumResults) {

  feature_list <- list(type = feature)
  if(is.numeric(maxNumResults)) feature_list[["maxResults"]] <- maxNumResults

  list(
    image    = list(content = as.character(encodedImages)),
    features = feature_list
  )
}

#' @title helper function to send POST request to the Google Vision API
#' @description sends the request defined in `body` to the API
#'
#' @param body, output of create_request_body()
#'
#' @return API response in raw format
#'
call_vision_api <- function(body) {

  simple_call <- googleAuthR::gar_api_generator(
    baseURI = "https://vision.googleapis.com/v1/images:annotate",
    http_header = "POST"
  )
  simple_call(the_body = body)
}

#' @title helper function code to extract the response data.frame
#' @description a utility to extract features from the API response
#'
#' @param responses an API response object
#' @inheritParams gcv_get_image_annotations
#'
#' @return a data frame
#'
extract_response <- function(responses, imagePaths, feature){

  detection_type <- list(
    LABEL_DETECTION    = "labelAnnotations",
    FACE_DETECTION     = "faceAnnotations",
    LOGO_DETECTION     = "logoAnnotations",
    TEXT_DETECTION     = "textAnnotations",
    LANDMARK_DETECTION = "landmarkAnnotations"
  )

  responses_with_paths <- do.call("rbind",
    lapply(seq_along(imagePaths), function(x) {
      response_df <- responses[[detection_type[[feature]]]][[x]]
      response_df[["image_path"]] <- imagePaths[x]
      response_df
    })
  )

  responses_with_paths[, c("image_path", "description", "score")]
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
