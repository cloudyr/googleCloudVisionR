context("integration")

base_image_path <- "../../inst/extdata/"
base_result_path <- "test_data/"
cases <- rbind(
    data.table(f = "LABEL_DETECTION",         i = "golden_retriever_puppies.jpg", rd = "label_detection.csv"),
    data.table(f = "TEXT_DETECTION",          i = "essex.jpg",                    rd = "text_detection.csv"),
    data.table(f = "DOCUMENT_TEXT_DETECTION", i = "essex.jpg",                    rd = "document_text_detection.csv"),
    data.table(f = "FACE_DETECTION",          i = "arnold_wife.jpg",              rd = "face_detection.csv"),
    data.table(f = "LOGO_DETECTION",          i = "brandlogos.png",               rd = "logo_detection.csv"),
    data.table(f = "LANDMARK_DETECTION",      i = "notre-dame.jpg",               rd = "landmark_detection.csv")
) %>% setnames(c("feature", "image_path", "result_data"))


test_that("returns the right columns", {
    skip_on_cran()
    skip_on_travis()

    purrr::pwalk(cases, function(feature, image_path, result_data) {
        responseCols <- names(gcv_get_image_annotations(
            paste0(base_image_path, image_path), feature = feature
        ))
        expectedCols <- names(fread(paste0(base_result_path, result_data)))

        expect_equal(responseCols, expectedCols)
    })
})

test_that("returns the expected number of rows", {
    skip_on_cran()
    skip_on_travis()

    purrr::pwalk(cases, function(feature, image_path, result_data) {
        numRowResponse <- gcv_get_image_annotations(
            paste0(base_image_path, image_path), feature = feature
        )[, .N]
        numRowExpected <- data.table::fread(paste0(base_result_path, result_data))[, .N]
        expect_equal(numRowResponse, numRowExpected)
    })
})

test_that("returns the expected number of rows", {
    skip_on_cran()
    skip_on_travis()

    purrr::pwalk(cases, function(feature, image_path, result_data) {
        expect_equal(
            gcv_get_image_annotations(paste0(base_image_path, image_path), feature = feature)[, lapply(.SD, class)],
            data.table::fread(paste0(base_result_path, result_data))[, lapply(.SD, class)]
        )
    })
})
