context("integration")

baseImagePath <- "../../inst/extdata/"
baseResultPath <- "test_data/"
cases <- rbind(
    data.table(feature = "LABEL_DETECTION",         image = "golden_retriever_puppies.jpg"),
    data.table(feature = "TEXT_DETECTION",          image = "essex.jpg"),
    data.table(feature = "DOCUMENT_TEXT_DETECTION", image = "essex.jpg"),
    data.table(feature = "FACE_DETECTION",          image = "arnold_wife.jpg"),
    data.table(feature = "LOGO_DETECTION",          image = "brandlogos.png"),
    data.table(feature = "LANDMARK_DETECTION",      image = "notre-dame.jpg"),
    data.table(feature = "IMAGE_PROPERTIES",        image = "bali_small.jpeg")
)


test_that("returns the right columns", {
    skip_on_cran()
    skip_on_travis()

    expectedColumns <- list(
        "LABEL_DETECTION" = c("mid", "description", "score", "topicality"),
        "TEXT_DETECTION" = c("description", "x", "y"),
        "DOCUMENT_TEXT_DETECTION" = c("description", "x", "y"),
        "FACE_DETECTION" = c("x", "y", "detection_confidence", "landmarking_confidence",
            "joy_likelihood", "sorrow_likelihood", "anger_likelihood", "surprise_likelihood",
            "under_exposed_likelihood", "blurred_likelihood", "headwear_likelihood"
        ),
        "LOGO_DETECTION" = c("mid", "description", "score", "x", "y"),
        "LANDMARK_DETECTION" = c("mid", "description", "score", "x", "y", "latitude", "longitude"),
        "IMAGE_PROPERTIES" = c("red", "green", "blue", "score", "pixel_fraction")
    )

    purrr::walk(cases[["feature"]], ~{
        expectedCols <- c(expectedColumns[[.x]], c("image_path", "feature"))
        cat(getwd())
        response <- gcv_get_image_annotations(
            imagePaths = file.path("..", "..", "inst", "extdata", cases[feature == .x, image]),
            feature = .x,
            maxNumResults = 5
        )

        expect_setequal(names(response), expectedCols)
    })
})
