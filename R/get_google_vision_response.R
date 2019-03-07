#' @title Calling Google's Cloud Vision API
#' @description input an image, provide the feature type and maxNumResults of responses
#'
#' @import googleAuthR
#'
#' @param imagePath string, path or url to the image
#' @param feature string, one out of: "FACE_DETECTION", "LANDMARK_DETECTION",
#'   "LOGO_DETECTION", "LABEL_DETECTION", "TEXT_DETECTION"
#' @param maxNumResults integer, the maximum number of results to return.
#'
#' @return a data frame with results
#'
#' @examples \dontrun{
#'     f <- system.file("extdata", "brandlogos.png", package = "RoogleVision")
#'     gcv_get_response(imagePath = f, feature = "LOGO_DETECTION")
#' }
#'
#' @export
gcv_get_response <- function(imagePath, feature = "LABEL_DETECTION",
                             maxNumResults = NULL){

  txt  <- image_to_text(imagePath)
  body <- create_request_body(txt, feature, maxNumResults)

  simpleCall <- gar_api_generator(
    baseURI = "https://vision.googleapis.com/v1/images:annotate",
    http_header = "POST"
  )
  pp <- simpleCall(the_body = body)

  if (ncol(pp$content$responses) > 0) {
    res <- extract_response(pp, feature)
  } else {
    res <- data.frame(error = "No features detected!")
  }

  return(res)
}

#' @title helper function base_encode code the image file
#' @description base64 encodes an image file
#'
#' @param imagePath provide path/url to image
#' @return get the image back as encoded file
#'
image_to_text <- function(imagePath) {

  if (stringr::str_count(imagePath, "http") > 0) {### its a url!
    content <- RCurl::getBinaryURL(imagePath)
    txt <- RCurl::base64Encode(content, "txt")
  } else {
    txt <- RCurl::base64Encode(readBin(
      imagePath, "raw", file.info(imagePath)[1, "size"]), "txt"
    )
  }
  return(txt)
}

#' @title helper function to create json for response request
#' @description creates a json output from the inputs
#'
#' @param txt, output of image_to_text
#' @param feature string, one out of: "FACE_DETECTION", "LANDMARK_DETECTION",
#'   "LOGO_DETECTION", "LABEL_DETECTION", "TEXT_DETECTION"
#' @param maxNumResults numeric, the maximnum number of rows returned by the
#'   Google Vision API
#'
#' @return get the image back as encoded file
create_request_body <- function(txt, feature, maxNumResults) {
  if(is.numeric(maxNumResults)) {
    request <- list(requests = list(
      image    = list(content = as.character(txt)),
      features = list(type = feature, maxResults = maxNumResults)
    ))
  } else {
    request <- list(requests = list(
      image    = list(content = as.character(txt)),
      features = list(type = feature)
    ))
  }

  jsonlite::toJSON(request, auto_unbox = TRUE)
}

#' @title helper function code to extract the response data.frame
#' @description a utility to extract features from the API response
#'
#' @param pp an API response object
#' @param feature the name of the feature to return
#' @return a data frame
#'
extract_response <- function(pp, feature){
  if (feature == "LABEL_DETECTION") {
    return(pp$content$responses$labelAnnotations[[1]])
  }
  if (feature == "FACE_DETECTION") {
    return(pp$content$responses$faceAnnotations[[1]])
  }
  if (feature == "LOGO_DETECTION") {
    return(pp$content$responses$logoAnnotations[[1]])
  }
  if (feature == "TEXT_DETECTION") {
    return(pp$content$responses$textAnnotations[[1]])
  }
  if (feature == "LANDMARK_DETECTION") {
    return(pp$content$responses$landmarkAnnotations[[1]])
  }
}
