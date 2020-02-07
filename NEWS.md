# googleCloudVisionR 0.2.0 (2020-02-07)

## NEW FEATURES
* New function `gcv_get_raw_response()` added that returns the raw response from the API as a list object
* The function `gcv_get_available_feature_types()` got exported, so users can know which features of the API are covered in the package
* Added bounding boxes for OCR results ("TEXT_DETECTION" and "DOCUMENT_TEXT_DETECTION" features)
* Added assertions for the `savePath` parameter of `gcv_get_raw_response()`

## FIXES
* Replaced base64 encoding from `{caTools}` with `{jsonlite}`

# googleCloudVisionR 0.1.0 (2019-06-21)

## NEW FEATURES
* New function `gcv_get_image_annotations()` added.
