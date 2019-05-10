context("integration")

test_that("returns correct answer for all features", {
    skip_on_cran()
    skip_on_travis()
    base_image_path <- "../../inst/extdata/"
    base_result_path <- "test_data/"
    cases <- data.table(feature = list("LABEL_DETECTION", "TEXT_DETECTION", "DOCUMENT_TEXT_DETECTION", "FACE_DETECTION", "LOGO_DETECTION", "LANDMARK_DETECTION"),
                        image_path = list("golden_retriever_puppies.jpg", "essex.jpg", "essex.jpg", "arnold_wife.jpg", "brandlogos.png", "notre-dame.jpg"),
                        result_data = list("label_detection.csv", "text_detection.csv", "document_text_detection.csv", "face_detection.csv", "logo_detection.csv", "landmark_detection.csv"))
    
    purrr::pwalk(cases, function(feature, image_path, result_data) {
        expect_equal(gcv_get_image_annotations(paste0(base_image_path, image_path), feature = feature),
                     data.table::fread(paste0(base_result_path, result_data)))
    })
})