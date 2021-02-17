#' @include api.R
connection<-setRefClass("connections",
                         # Define the slots
                         fields = list(
                           ApiList = "list",
                           useragent = "character"
                         ),
                         methods = list(
                           username = function(user,credentials){
                             apicheck<-FALSE
                             for(api in .self$ApiList){
                               if(credentials==api$credentials|credentials=="ALL"){
                                 api$username=user
                                 apicheck=TRUE
                               }
                             }
                             if(!apicheck)warning("Api not supported.")
                           },
                           password = function(pass,credentials){
                             apicheck<-FALSE
                             for(api in .self$ApiList){
                               if(credentials==api$credentials|credentials=="ALL"){
                                 api$password=pass
                                 apicheck=TRUE
                               }
                             }
                             if(!apicheck)warning("Api not supported.")
                           },
                           getApiNames=function(){
                             apis<-c()
                             for(api in .self$ApiList){
                                apis<-c(apis,api$api_name)
                             }
                             apis
                           },
                           getApi = function(api_name){
                             .self$ApiList[[which(.self$getApiNames()%in%api_name)]]
                           },
                           getCredentialsNames=function(){
                             apis<-c()
                             for(api in .self$ApiList){
                               apis<-c(apis,api$credentials)
                             }
                             unique(apis)
                           },
                           getCredentials = function(){
                             apis<-c()
                             apinames<-c()
                             for(api in .self$ApiList){
                               apis<-rbind(apis,c(api$api_name,api$credentials,api$getCredentials()))
                             }
                             colnames(apis)<-c("Api_name","Portal","Username","Password")
                             apis
                           }

                         )
)$new(ApiList=list(eeapi=api("earthexplorer","https://earthexplorer.usgs.gov","https://m2m.cr.usgs.gov/api/api/json/stable","earthdata"),
                   espa=api("ESPA","https://espa.cr.usgs.gov","https://espa.cr.usgs.gov/api/v1","earthdata"),
                   nasainv=api("nasa_inventory","https://lpdaacsvc.cr.usgs.gov","https://lpdaacsvc.cr.usgs.gov/services/inventory","earthdata"),
                   scihub=api("scihub","https://scihub.copernicus.eu","https://scihub.copernicus.eu/apihub","scihub"),
                   scihubs5p=api("scihubs5p","https://s5phub.copernicus.eu","https://s5phub.copernicus.eu/apihub","scihubs5p")),
      useragent="Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:58.0) Gecko/20100101 Firefox/58.0")



setGeneric("set_user",function(user,credential){standardGeneric("set_user")})
setMethod("set_user",
          signature = c("character","missing"),
          function(user){
              connection$username(user,"ALL")
          })
setMethod("set_user",
          signature = c("character","character"),
          function(user,credential){
            connection$username(user,credential)
          })

setGeneric("set_pass",function(pass,credential){standardGeneric("set_pass")})
setMethod("set_pass",
          signature = c("character","missing"),
          function(pass){
            connection$password(pass,"ALL")
          })
setMethod("set_pass",
          signature = c("character","character"),
          function(pass,credential){
            connection$password(pass,credential)
          })

#' Saves the credentials for the web services
#'
#' @param user character argument. Defines the username of an api platform
#'  to search or download images
#' @param pass character argument. Defines the password of an api platform
#'  to search and download images
#' @param credential optional argument to specify the name of the platform.
#' Valid names are earthdata, scihub, scihubs5p, or ALL
#' @export
#' @examples
#' print_credentials()
#' set_credentials("example","example")
#' print_credentials()
#' set_credentials("example","example","earthdata")
#' print_credentials()
setGeneric("set_credentials",function(user,pass,credential){standardGeneric("set_credentials")})

#' @rdname set_credentials
#' @aliases set_credentials,character,character,missing
setMethod("set_credentials",
          signature = c("character","character","missing"),
          function(user,pass){
            connection$username(user,"ALL")
            connection$password(pass,"ALL")
          })

#' @rdname set_credentials
#' @aliases set_credentials,character,character,character
setMethod("set_credentials",
          signature = c("character","character","character"),
          function(user,pass,credential){
            connection$username(user,credential)
            connection$password(pass,credential)
          })

#' Prints the credentials for the web services
#'
#' @param ... additional arguments.
#'
#' @export
#' @examples
#' print_credentials()
#' set_credentials("example","example","earthdata")
#' print_credentials()
setGeneric("print_credentials",function(...){standardGeneric("print_credentials")})

#' @rdname print_credentials
#' @aliases print_credentials
setMethod("print_credentials",
signature = c(),
function(){
  connection$getCredentials()
})


