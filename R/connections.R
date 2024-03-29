#' @include api_dataspace.R api_lpdaac.R api_usgs.R
connection <- setRefClass("connections",
  # Define the slots
  fields = list(
    ApiList = "list",
    useragent = "character"
  ),
  methods = list(
    username = function(user, credentials) {
      apicheck <- FALSE
      for (api in .self$ApiList) {
        if (credentials == api$credentials | credentials == "ALL") {
          api$username <- user
          apicheck <- TRUE
        }
      }
      if (!apicheck) warning("Api not supported.")
    },
    password = function(pass, credentials) {
      apicheck <- FALSE
      for (api in .self$ApiList) {
        if (credentials == api$credentials | credentials == "ALL") {
          api$password <- pass
          apicheck <- TRUE
        }
      }
      if (!apicheck) warning("Api not supported.")
    },
    getApiNames = function() {
      apis <- c()
      for (api in .self$ApiList) {
        apis <- c(apis, api$api_name)
      }
      apis
    },
    getApi = function(api_name) {
      .self$ApiList[[which(.self$getApiNames() %in% api_name)]]
    },
    getCredentialsNames = function() {
      apis <- c()
      for (api in .self$ApiList) {
        apis <- c(apis, api$credentials)
      }
      unique(apis)
    },
    getCredentials = function() {
      apis <- c()
      apinames <- c()
      for (api in .self$ApiList) {
        apis <- rbind(apis, c(api$api_name,
                              api$credentials,
                              api$getCredentials()))
      }
      colnames(apis) <- c("Api_name", "Portal", "Username", "Password")
      apis
    },
    printProducts=function(){
      print("-------------------------------------------------------------------------")
      print("    rsat supported products")
      for (api in .self$ApiList) {
        print("-------------------------------------------------------------------------")
        products <- api$getProducts()
        for(x in seq(length(products))){
          print(paste0(names(products)[x],": ",paste0(products[[x]],collapse=", ")))
        }
      }
      print("-------------------------------------------------------------------------")
    }
  )
)$new(
  ApiList = list(
    lpdaac = api_lpdaac("lpdaac"),
    usgs = api_usgs("usgs"),
    dataspace = api_dataspace("dataspace")
  ),
  useragent = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:115.0)",
                     " Gecko/20100101 Firefox/115.0")
)



setGeneric("set_user", function(user, credential) {
  standardGeneric("set_user")
})
setMethod("set_user",
  signature = c("character", "missing"),
  function(user) {
    connection$username(user, "ALL")
  }
)
setMethod("set_user",
  signature = c("character", "character"),
  function(user, credential) {
    connection$username(user, credential)
  }
)

setGeneric("set_pass", function(pass, credential) {
  standardGeneric("set_pass")
})
setMethod("set_pass",
  signature = c("character", "missing"),
  function(pass) {
    connection$password(pass, "ALL")
  }
)
setMethod("set_pass",
  signature = c("character", "character"),
  function(pass, credential) {
    connection$password(pass, credential)
  }
)

#' Saves the credentials for the web services
#'
#' @param user character argument. Defines the username of an api platform
#'  to search or download images
#' @param pass character argument. Defines the password of an api platform
#'  to search and download images
#' @param credential optional argument to specify the name of the platform.
#' Valid names are earthdata, scihub, scihubs5p, or ALL
#' @return nothing. set the credentials in the package environment variable
#' @export
#' @examples
#' print_credentials()
#' set_credentials("example", "example")
#' print_credentials()
#' set_credentials("example", "example", "earthdata")
#' print_credentials()
setGeneric("set_credentials", function(user, pass, credential) {
  standardGeneric("set_credentials")
})

#' @rdname set_credentials
#' @aliases set_credentials,character,character,missing
setMethod("set_credentials",
  signature = c("character", "character", "missing"),
  function(user, pass) {
    connection$username(user, "ALL")
    connection$password(pass, "ALL")
  }
)

#' @rdname set_credentials
#' @aliases set_credentials,character,character,character
setMethod("set_credentials",
  signature = c("character", "character", "character"),
  function(user, pass, credential) {
    connection$username(user, credential)
    connection$password(pass, credential)
  }
)

#' Prints the credentials for the web services
#'
#' @param ... additional arguments.
#' @return print the credentials asigned in the package environment variable
#' @export
#' @examples
#' print_credentials()
#' set_credentials("example", "example", "earthdata")
#' print_credentials()
setGeneric("print_credentials", function(...) {
  standardGeneric("print_credentials")
})

#' @rdname print_credentials
#' @aliases print_credentials
setMethod("print_credentials",
  signature = c(),
  function() {
    connection$getCredentials()
  }
)



#' Show the products accepted by the services
#'
#' @param ... additional arguments.
#' @return prints a list of products
#' @export
#' @examples
#' rsat_products()
setGeneric("rsat_products", function(...) {
  standardGeneric("rsat_products")
})

#' @rdname rsat_products
#' @aliases rsat_products
setMethod("rsat_products",
          signature = c(),
          function() {
            connection$printProducts()
          }
)
