#' Map user-provided NCA settings to NCA engine (e.g. PKNCA)
#' 
#' @param settings list of settings, potentially to be remapped
#' @param mapping name of mapping to use, default is `pknca`. Mappings are
#' stored within package in folder `md/nca`.
#' 
#' @export
map_nca_settings <- function(
  settings = NULL,
  mapping = "pknca"
) {
  
  ## Check arguments
  if(is.null(settings)) {
    return(NULL)
  }
  if(is.null(mapping)) {
    stop("`mapping` argument cannot be NULL.")
  }
  
  ## Load options and remap metadata
  md_file <- system.file(package = "PKNCA.extra", file.path("nca", paste0(mapping, ".json")))
  if(md_file == "") {
    stop("Metadata file with mapping of NCA settings not found. Please check your arguments.")
  }
  md <- jsonlite::read_json(md_file)
  if(is.null(md)) {
    stop("Metadata file with mapping of NCA settings not found. Please check your arguments.")
  }
  if(is.null(md$mapping)) {
    return(settings)
  }
  
  ## remap the settings recognized by the map
  new_settings <- md$defaults
  for(m in names(md$mapping$names)) {
    if(inherits(md$mapping$names[[m]], "list")) {
      new_settings[[m]] <- list()
      for(k in names(md$mapping$names[[m]])) {
        if(!is.null(settings[[m]])) {
          new_settings[[m]][[k]] <- remap_core(settings[[m]], k, md$mapping$names[[m]][[k]], md$mapping$options[[m]][[k]])  
        } else {
          new_settings[[m]][[k]] <- remap_core(settings, k, md$mapping$names[[m]][[k]], md$mapping$options[[m]][[k]])  
        }
      }
    } else {
      new_settings[[m]] <- remap_core(settings, m, md$mapping$names[[m]], md$mapping$options[[m]])
    }
  }
  new_settings
}

#' Core remapping function
#' 
#' @param settings TODO
#' @param m TODO
#' @param mapped_name TODO
#' @param mapped_option TODO
remap_core <- function(settings, m, mapped_name,  mapped_option) {
  if(!is.null(mapped_name)) { # check if available as mapped value
    if(!is.null(settings[[m]])) { 
      idx <- m                     # if original name exists, use that
    } else {
      idx <- mapped_name # else use mapped name
    }
    out <- settings[[idx]]
  } else {
    out <- NULL
  }
  ## Remap option values (if relevant)
  if(!is.null(mapped_option)) {
    if(out %in% names(mapped_option)) {
      out <- mapped_option[[out]]
    }
  }
  out
}
