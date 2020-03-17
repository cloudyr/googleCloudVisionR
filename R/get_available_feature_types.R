#' @title helper function code to record available feature types
#'
#' @return a list of available features names and their types (as returned by the API)
#'
#' @examples
#' gcv_get_available_feature_types()
#'
#' @export
gcv_get_available_feature_types <- function() {
    list(
        LABEL_DETECTION         = "labelAnnotations",
        TEXT_DETECTION          = "textAnnotations",
        DOCUMENT_TEXT_DETECTION = "textAnnotations",
        FACE_DETECTION          = "faceAnnotations",
        LOGO_DETECTION          = "logoAnnotations",
        LANDMARK_DETECTION      = "landmarkAnnotations",
        IMAGE_PROPERTIES        = "imagePropertiesAnnotation",
        OBJECT_LOCALIZATION     = "localizedObjectAnnotations"
    )
}
