#' @title Calling Google's Cloud Vision API
#' @description input an image, provide the feature type and maxNumResults of responses
#'
#' @import googleAuthR
#'
#' @param imagePaths string, paths or urls to the images
#' @param feature string, one out of: "FACE_DETECTION", "LANDMARK_DETECTION",
#'   "LOGO_DETECTION", "LABEL_DETECTION", "TEXT_DETECTION"
#' @param maxNumResults integer, the maximum number of results to return.
#'
#' @return a data frame with results
#'
#' @examples \dontrun{
#'     f <- system.file("extdata", "brandlogos.png", package = "googleCloudVisionR")
#'     gcv_get_response(imagePaths = f, feature = "LOGO_DETECTION")
#' }
#'
#' @export
gcv_get_response <- function(imagePaths, feature = "LABEL_DETECTION",
                             maxNumResults = NULL){

  txt  <- sapply(imagePaths, image_to_text)
  body <- create_request_body(txt, feature, maxNumResults)
  rawResponse <- call_vision_api(body)
  extract_response(
    rawResponse[["content"]][["responses"]],
    imagePaths,
    feature
  )
}

#' @title helper function base_encode code the image file
#' @description base64 encodes an image file
#'
#' @param imagePaths provide path/url to image
#' @return get the image back as encoded file
#'
image_to_text <- function(imagePaths) {

  if (stringr::str_count(imagePaths, "http") > 0) {### its a url!
    content <- RCurl::getBinaryURL(imagePaths)
    txt <- RCurl::base64Encode(content, "txt")
  } else {
    txt <- RCurl::base64Encode(readBin(
      imagePaths, "raw", file.info(imagePaths)[1, "size"]), "txt"
    )
  }
  return(txt)
}

#' @title helper function to create json for response request
#' @description creates a json output from the inputs
#'
#' @param txt, output of image_to_text()
#' @param feature string, one out of: "FACE_DETECTION", "LANDMARK_DETECTION",
#'   "LOGO_DETECTION", "LABEL_DETECTION", "TEXT_DETECTION"
#' @param maxNumResults numeric, the maximnum number of rows returned by the
#'   Google Vision API
#'
#' @return get the image back as encoded file
create_request_body <- function(txt, feature, maxNumResults) {
  names(txt) <- NULL
  requests <- list(
    requests = lapply(txt, create_single_image_request, feature, maxNumResults)
  )
  jsonlite::toJSON(requests, auto_unbox = TRUE)
}

create_single_image_request <- function(txt, feature, maxNumResults) {

  feature_list <- list(type = feature)
  if(is.numeric(maxNumResults)) feature_list[["maxResults"]] <- maxNumResults

  list(
    image    = list(content = as.character(txt)),
    features = feature_list
  )
}

#' @title helper function to send POST request to the Google Vision API
#' @description sends the request defined in `body` to the API
#'
#' @param body, output of create_request_body()
#'
#' @return API response in raw format
call_vision_api <- function(body) {

  simple_call <- gar_api_generator(
    baseURI = "https://vision.googleapis.com/v1/images:annotate",
    http_header = "POST"
  )
  simple_call(the_body = body)
}

#' @title helper function code to extract the response data.frame
#' @description a utility to extract features from the API response
#'
#' @param responses an API response object
#' @param imagePaths string, paths or urls to the images
#' @param feature the name of the feature to return
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



