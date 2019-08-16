source("test-utils.R")

context("Input validation")

test_that("it throws informative exceptions for invalid imagePaths", {
    gcvGetAndCacheResponseMock <- mockery::mock()
    mockery::stub(gcv_get_image_annotations, "gcv_read_response", gcvGetAndCacheResponseMock)
    invalidInputs <- list("", "foo", c("bar", "baz"), c("https://bit.ly/2IhUzdE", "qux"), c("gs://vision-api-handwriting-ocr-bucket/handwriting_image.png", "quux"))
    expectedOutputs <- list("", "foo", c("bar", "baz"), "qux", "quux")

    purrr::walk2(invalidInputs, expectedOutputs, ~{
        expect_error(
            gcv_get_image_annotations(.x),
            paste0("Invalid elements in imagePath: ", paste(.y, collapse = " "))
        )
    })
    mockery::expect_called(gcvGetAndCacheResponseMock, 0)
})

test_that("it throws informative exceptions if invalid feature parameter is provided", {
    mockery::stub(gcv_get_image_annotations, "gcv_read_response", mockery::mock())
    invalidFeature <- "foobarbaz"
    expect_error(
        gcv_get_image_annotations("https://bit.ly/2IhUzdE", feature = invalidFeature),
        paste0("Invalid feature: ", invalidFeature, " - it should be one of: ",
               "'LABEL_DETECTION', 'TEXT_DETECTION', 'DOCUMENT_TEXT_DETECTION', ",
               "'FACE_DETECTION', 'LOGO_DETECTION', 'LANDMARK_DETECTION'")
        )
})

test_that("API request is sent w/o error if valid feature parameters are provided", {
    gcvGetResponseMock <- mockery::mock()
    mockery::stub(gcv_get_image_annotations, "gcv_read_response", gcvGetResponseMock)

    validFeatures <- c("LABEL_DETECTION", "TEXT_DETECTION", "DOCUMENT_TEXT_DETECTION",
                       "FACE_DETECTION", "LOGO_DETECTION", "LANDMARK_DETECTION")
    purrr::walk(validFeatures, ~{
        gcv_get_image_annotations("https://bit.ly/2IhUzdE", feature = .x)
    })
    call_args <- mockery::mock_args(gcvGetResponseMock)
    expect_equal(purrr::map_chr(call_args, 3), validFeatures)
})

context("Cached annotations")

test_that("it calls API only on not yet cached results", {
    savePath <- "annotated_images.csv"
    fwrite(
        data.table(image_path = "annotated_image.jpg", feature = "LABEL_DETECTION"),
        savePath
    )
    imagePaths <- list(
        "image_not_yet_annotated.jpg",
        c("annotated_image.jpg", "image_not_yet_annotated.jpg")
    )
    savePaths <- list(NULL, savePath)
    expectedCallArgs <- c(
        "image_not_yet_annotated.jpg",
        "image_not_yet_annotated.jpg"
    )

    gcvGetAndCacheResponseMock <- mockery::mock(data.table(), cycle = TRUE)
    mockery::stub(gcv_get_image_annotations, "gcv_read_response", gcvGetAndCacheResponseMock)

    purrr::walk2(imagePaths, savePaths, ~{
        suppressWarnings(gcv_get_image_annotations(.x, savePath = .y))
    })

    call_args <- mockery::mock_args(gcvGetAndCacheResponseMock)
    expect_equal(purrr::map_chr(call_args, 1), expectedCallArgs)
    file.remove(savePath)
})

test_that("it does not call API when cached result is available", {
    savePath <- "annotated_images.csv"
    fwrite(
        data.table(image_path = "annotated_image.jpg", feature = "LABEL_DETECTION"),
        savePath
    )
    gcvGetAndCacheResponseMock <- mockery::mock()
    mockery::stub(gcv_get_image_annotations, "gcv_read_response", gcvGetAndCacheResponseMock)

    gcv_get_image_annotations("annotated_image.jpg", savePath = savePath)

    mockery::expect_called(gcvGetAndCacheResponseMock, 0)
    file.remove(savePath)
})

test_that("it creates cache when savePath is provided and cache doesn't exist yet", {
    gcvGetResponseMock <- mockery::mock(data.table(column1 = 1, feature = "LABEL_DETECTION"))
    mockery::stub(gcv_read_response, "gcv_get_response", gcvGetResponseMock)

    gcv_read_response(
        "annotated_image.jpg", 1, feature = "LABEL_DETECTION", 1, "test.csv"
    )

    expect_equal(
        data.table(column1 = 1, feature = "LABEL_DETECTION"),
        fread("test.csv")
    )

    file.remove("test.csv")
})

test_that("it stores feature type in cache", {
    testSavePath <- "test.csv"
    # testing lower level function b/c mockery's depth param doesn't work with devtools::check
    gcvGetResponseMock <- mockery::mock(data.table(column1 = 1, feature = "LABEL_DETECTION"))
    mockery::stub(gcv_read_response, "gcv_get_response", gcvGetResponseMock)

    gcv_read_response(
        "annotated_image.jpg", 1, feature = "LABEL_DETECTION", 1, testSavePath
    )
    cache <- fread(testSavePath)

    expect_equal(names(cache), c("column1", "feature"))
    expect_equal(cache[1, feature], "LABEL_DETECTION")

    file.remove(testSavePath)
})

test_that("it throws error when provided feature type is inconsistent with existing cache", {
    testSavePath <- "test.csv"
    fwrite(data.table(feature = "LABEL_DETECTION"), testSavePath)

    expect_error(
        gcv_get_image_annotations(
            "https://bit.ly/2IhUzdE", feature = "FACE_DETECTION", savePath = testSavePath
        ),
        glue::glue("{testSavePath} was already used for 'LABEL_DETECTION'")
    )
    file.remove(testSavePath)
})

context("Google Vision API")

test_that("handles case of non-image URL", {
    expected_response <- create_response(FALSE)
    gcvGetRawResponseMock <- mockery::mock(
        list(content = list(responses = expected_response))
    )
    mockery::stub(gcv_get_response, "gcv_get_raw_response", gcvGetRawResponseMock)
    # depth = 2 fails on devtools::check(), so we are not mocking the public function

    expect_equal(
        gcv_get_response(
            "http://non-image-url", feature = "LABEL_DETECTION", maxNumResults = NULL
        )[, .(error_code, error_message)],
        data.table(error_code = 3, error_message = "Nice error message.")
    )
})

test_that("handles successful responses with LABEL_DETECTION", {
    expected_response <- create_response(TRUE)
    gcvGetRawResponseMock <- mockery::mock(
        list(content = list(responses = expected_response))
    )
    mockery::stub(gcv_get_response, "gcv_get_raw_response", gcvGetRawResponseMock)
    # depth = 2 fails on devtools::check(), so we are not mocking the public function

    expect_equal(
        gcv_get_response("http://image-url", feature = "LABEL_DETECTION", maxNumResults = NULL),
        data.table(
            image_path = "http://image-url",
            mid = "foo",
            description = "bar",
            score = 0.987,
            topicality = 0.987,
            feature = "LABEL_DETECTION"
        )
    )
})

test_that("handles mixed successes and errors", {
    expected_response <- create_response(c(TRUE, FALSE))
    gcvGetRawResponseMock <- mockery::mock(
        list(content = list(responses = expected_response))
    )
    mockery::stub(gcv_get_response, "gcv_get_raw_response", gcvGetRawResponseMock)
    # depth = 2 fails on devtools::check(), so we are not mocking the public function

    expect_equal(
        gcv_get_response(
            c("http://image-url", "http://non-image-url"),
            feature = "LABEL_DETECTION", maxNumResults = NULL
        ),
        data.table(
            image_path = c("http://image-url", "http://non-image-url"),
            mid = c("foo", NA),
            description = c("bar", NA),
            score = c(0.987, NA),
            topicality = c(0.987, NA),
            error_code = c(NA, 3),
            error_message = c(NA, "Nice error message."),
            feature = rep("LABEL_DETECTION", 2)
        )
    )
})
