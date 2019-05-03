source("test-utils.R")

context("Input validation")

test_that("it throws informative exceptions for invalid imagePaths", {
    mockery::stub(gcv_get_image_annotations, "gcv_get_response", mockery::mock())
    invalidInputs <- list("", "foo", c("bar", "baz"), c("https://bit.ly/2IhUzdE", "qux"), c("gs://vision-api-handwriting-ocr-bucket/handwriting_image.png", "quux"))
    expectedOutputs <- list("", "foo", c("bar", "baz"), "qux", "quux")

    purrr::walk2(invalidInputs, expectedOutputs, ~{
        expect_error(gcv_get_image_annotations(.x), paste0("Invalid elements in imagePath: ", paste(.y, collapse = " ")))
        }
    )
})

test_that("it throws informative exceptions for invalid feature", {
  mockery::stub(gcv_get_image_annotations, "gcv_get_response", mockery::mock())
  invalidFeature <- "foobarbaz"
  expect_error(
    gcv_get_image_annotations("https://bit.ly/2IhUzdE", feature = invalidFeature),
    paste0("Invalid feature: ", invalidFeature, " - it should be one of: ",
           "'LABEL_DETECTION', 'TEXT_DETECTION', 'DOCUMENT_TEXT_DETECTION', ",
           "'FACE_DETECTION', 'LOGO_DETECTION', 'LANDMARK_DETECTION'")
    )
})

test_that("API request is sent with all valid feature parameters", {
  gcvGetResponseMock <- mockery::mock()
  mockery::stub(gcv_get_image_annotations, "gcv_get_response", gcvGetResponseMock)

  validFeatures <- c("LABEL_DETECTION", "TEXT_DETECTION", "DOCUMENT_TEXT_DETECTION",
                     "FACE_DETECTION", "LOGO_DETECTION", "LANDMARK_DETECTION")
  purrr::walk(validFeatures, ~{
    gcv_get_image_annotations("https://bit.ly/2IhUzdE", feature = .x)
  })
  call_args <- mockery::mock_args(gcvGetResponseMock)
  expect_equal(purrr::map_chr(call_args, 2), validFeatures)
})

context("Cached annotations")

test_that("it calls API based on cached results", {
  imagePaths <- list(
    "image_not_yet_annotated.jpg",
    c("annotated_image.jpg", "image_not_yet_annotated.jpg")
  )
  savePaths <- list(NULL, "annotated_images.csv")
  expectedCallArgs <- c(
    "image_not_yet_annotated.jpg",
    "image_not_yet_annotated.jpg"
  )

  gcvGetResponseMock <- mockery::mock(data.table::data.table(), cycle = TRUE)
  mockery::stub(gcv_get_image_annotations, "gcv_get_response", gcvGetResponseMock)

  purrr::walk2(imagePaths, savePaths, ~{
    suppressWarnings(gcv_get_image_annotations(.x, savePath = .y))
  })

  call_args <- mockery::mock_args(gcvGetResponseMock)
  expect_equal(purrr::map_chr(call_args, 1), expectedCallArgs)
})

test_that("it does not call API when cached result is available", {
  gcvGetResponseMock <- mockery::mock()
  mockery::stub(gcv_get_image_annotations, "gcv_get_response", gcvGetResponseMock)

  gcv_get_image_annotations("annotated_image.jpg", savePath = "annotated_images.csv")

  mockery::expect_called(gcvGetResponseMock, 0)
})

context("Google Vision API")

test_that("handles case of non-image URL", {
  expected_response <- create_response(FALSE)
  callVisionAPIMock <- mockery::mock(
    list(content = list(responses = expected_response))
  )
  mockery::stub(gcv_get_response, "call_vision_api", callVisionAPIMock)
  # depth = 2 fails on devtools::check(), so we are not mocking the public function

  expect_equal(
    gcv_get_response("http://non-image-url", feature = "LABEL_DETECTION", maxNumResults = NULL)[, .(error_code, error_message)],
    data.table(error_code = 3, error_message = "Nice error message.")
  )
})

test_that("handles successful responses with LABEL_DETECTION", {
  expected_response <- create_response(TRUE)
  callVisionAPIMock <- mockery::mock(
    list(content = list(responses = expected_response))
  )
  mockery::stub(gcv_get_response, "call_vision_api", callVisionAPIMock)
  # depth = 2 fails on devtools::check(), so we are not mocking the public function

  expect_equal(
    gcv_get_response("http://image-url", feature = "LABEL_DETECTION", maxNumResults = NULL),
    data.table(
      image_path = "http://image-url",
      mid = "foo",
      description = "bar",
      score = 0.987
    )
  )
})

test_that("handles mixed successes and errors", {
  expected_response <- create_response(c(TRUE, FALSE))
  callVisionAPIMock <- mockery::mock(
    list(content = list(responses = expected_response))
  )
  mockery::stub(gcv_get_response, "call_vision_api", callVisionAPIMock)
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
      error_code = c(NA, 3),
      error_message = c(NA, "Nice error message.")
    )
  )
})
