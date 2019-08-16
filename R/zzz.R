.onLoad <- function(libname, pkgname) {
    op <- options()
    op.cloudVisionR <- list(
        googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/cloud-platform")
    )

    toset <- !(names(op.cloudVisionR) %in% names(op))

    if(any(toset)) options(op.cloudVisionR[toset])

    invisible()
}

.onAttach <- function(libname, pkgname) {
    attempt <- try(googleAuthR::gar_attach_auto_auth(
        "https://www.googleapis.com/auth/cloud-platform",
        environment_var = "GCV_AUTH_FILE"
    ))

    # if(inherits(attempt, "try-error")){
    #   warning("Problem using auto-authentication when loading from GCV_AUTH_FILE.
    #           Run googleAuthR::gar_auth() or googleAuthR::gar_auth_service() instead.")
    # }

    if(Sys.getenv("GCV_CLIENT_ID") != ""){
        options(googleAuthR.client_id = Sys.getenv("GCV_CLIENT_ID"))
    }

    if(Sys.getenv("GCV_CLIENT_SECRET") != ""){
        options(googleAuthR.client_secret = Sys.getenv("GCV_CLIENT_SECRET"))
    }

    if(Sys.getenv("GCV_WEB_CLIENT_ID") != ""){
        options(googleAuthR.webapp.client_id = Sys.getenv("GCV_WEB_CLIENT_ID"))
    }

    if(Sys.getenv("GCV_WEB_CLIENT_SECRET") != ""){
        options(googleAuthR.webapp.client_id = Sys.getenv("GCV_WEB_CLIENT_SECRET"))
    }

    if(Sys.getenv("GCV_DEFAULT_PROJECT_ID") != ""){
        .gcv_env$project <- Sys.getenv("GCV_DEFAULT_PROJECT_ID")
        packageStartupMessage("Set default project to '", Sys.getenv("GCV_DEFAULT_PROJECT_ID"),"'")
    }

    if(Sys.getenv("GCV_DEFAULT_DATASET") != ""){
        .gcv_env$dataset <- Sys.getenv("GCV_DEFAULT_DATASET")
        packageStartupMessage("Set default dataset to '", Sys.getenv("GCV_DEFAULT_DATASET"),"'")
    }

    invisible()
}
