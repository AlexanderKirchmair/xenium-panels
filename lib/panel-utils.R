

### Wrapper function to get all panels
get_panels <- function(panel_names = NULL, dir = "panels", format = "csv", method = "curl", overwrite = TRUE){
  
  panels_table <- get_panels_table()
  
  if (!is.null(panel_names)){
    if (!all(panel_names %in% panels_table$Panel)){
      warning("Panels not found!")
    }
    panels_table <- subset(panels_table, Panel %in% panel_names)
  }
  
  panels_files <- download_panels(panels_table, dir = dir, format = format, method = method, overwrite = overwrite)
  
  panels <- read_panels(files = setNames(panels_files[[format]], panels_files$Panel))
  panels
}


### Get all available panels
get_panels_table <- function(url = "https://www.10xgenomics.com/support/in-situ-gene-expression/documentation/steps/panel-design/pre-designed-xenium-gene-expression-panels"){
  
  stopifnot(requireNamespace("rvest"))
  
  html <- rvest::read_html(url)
  tables <- rvest::html_nodes(html, "table")
  panel_table <- tables[[1]]
  
  all_panels <- rvest::html_table(panel_table, fill = TRUE) |> as.data.frame()
  all_links <- html |> rvest::html_nodes(xpath = "//td/a") |> rvest::html_attr("href")
  all_links <- grep("xenium.*panels", all_links, value = TRUE, ignore.case = TRUE)
  
  if (nrow(all_panels) * 4 != length(all_links)){
    stop("Html parsing error!")
  }
  
  panel_links <- all_panels |> dplyr::select("Panel", "Genes targeted")
  panel_links$csv <- NA
  panel_links$json <- NA
  panel_links$fasta <- NA
  panel_links$bed <- NA
  for (i in 1:nrow(panel_links)){
    tmp_links <- all_links[rev(4*i - 0:3)]
    panel_links[i,"csv"] <- grep("\\.csv$", tmp_links, value = TRUE)
    panel_links[i,"json"] <- grep("\\.json$", tmp_links, value = TRUE)
    panel_links[i,"fasta"] <- grep("\\.fasta$", tmp_links, value = TRUE)
    panel_links[i,"bed"] <- grep("\\.bed$", tmp_links, value = TRUE)
  }
  
  panel_links
}



### Download panels
download_panels <- function(panels_table, dir = "panels", format = c("csv", "json"), method = "curl", overwrite = TRUE){
  
  dir.create(dir, showWarnings = FALSE)
  
  panels_table <- panels_table |> dplyr::select("Panel", "Genes targeted", !!format)
  
  for (i in 1:nrow(panels_table)){
    subdir <-  file.path(dir, panels_table$Panel[i])
    dir.create(subdir, showWarnings = FALSE)
    files <- panels_table[i, format] |> as.character()
    destfiles <- file.path(subdir, basename(files))
    
    for (j in 1:length(files)){
      if (!file.exists(destfiles[j]) & overwrite){
        download.file(url = files[j], destfile = destfiles[j], method = method)
      }
    }
    panels_table[i, format] <- destfiles
    
  }
  
  invisible(panels_table)
}



### Read panels
read_panels <- function(files){
  
  if (is.null(names(files))){
    names(files) <- sub("\\..*$", "", basename(files))
  }
  
  ext <- gsub(basename(files), pattern = ".*\\.", replacement = "")
  format <- unique(ext)
  if (length(format) > 1){
    stop("Error: Please use only one file format at once!")
  }
  
  if (format == "csv"){
    panels <- lapply(files, read.csv)
    
  } else if (format == "json"){
    panels <- lapply(files, function(file) rjson::fromJSON(file = file)$payload )
    
  } else {
    
    stop("Format currently not supported.")
  }
  
  
  panels
}




