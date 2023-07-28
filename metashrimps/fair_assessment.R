##21/07/2023
##Genthon Tanguy
### FAIR Asessment


#load arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0)
{
  stop("This tool needs at least one argument")
}else{
  path_checks <- args[1]
  path_files <- args[2]
  metadata <- args[4]
}

#load packages

library(xml2)
library(metadig)
library(ggplot2)
library(dplyr)
library(gridExtra)

#Check function


#' url_exists
#' Check if the URL is resolvable
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
#'
#' @return a boolean
#' @export
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = TRUE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(FALSE) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}

#Representation functions

#' Fair_scores
#'Function rendering a Fair Quality Score
#'
#' @param Res Result of a test suite
#' @param dir directories where are situated checks
#'
#' @return a barplot with scores
#' @export
#'
Fair_scores<-function(Res,dir= directory){
  checks <- list.files(dir)
  checks<-paste0(dir,"/",checks)
  all <- lapply(checks, read_xml)
  names(all) <- checks
  all <- lapply(all, xml_find_all, "type")
  all <- lapply(all, xml_text)
  score=0
  scoreF=0
  countf=0
  scoreA=0
  counta=0
  scoreI=0
  counti=0
  scoreR=0
  countr=0
  for (i in 1:length(Res)){
    if (all[i][[1]]=="findable"){
      countf=countf+1
      if(Res[i][[1]]$value$status=="SUCCESS"){
        score=score+1
        scoreF=scoreF+1
      }
    }
    if (all[i][[1]]=="accessible"){
      counta=counta+1
      if(Res[i][[1]]$value$status=="SUCCESS"){
        score=score+1
        scoreA=scoreA+1
      }
    }
    if (all[i][[1]]=="interoperable"){
      counti=counti+1
      if(Res[i][[1]]$value$status=="SUCCESS"){
        score=score+1
        scoreI=scoreI+1
      }
    }
    if (all[i][[1]]=="reusable"){
      
      countr=countr+1
      if(Res[i][[1]]$value$status=="SUCCESS"){
        score=score+1
        scoreR=scoreR+1
      }
    }
  }
  scoreF=(scoreF/countf)*100
  scoreA=(scoreA/counta)*100
  scoreI=(scoreI/counti)*100
  scoreR=(scoreR/countr)*100
  score=(score/length(Res))*100
  data=c(scoreF,scoreA,scoreI,scoreR,score)
  barplot(data,names.arg=c("Findable","Accessible","Interoperable","Reusable","Mean Fair Score"),col=c("#3bb08f","#ffcfab" ,"#0d98ba" ,"#ffbcd9","#e3256b"),ylim=c(0,100))
}



#' Fair_Table
#'Make a table of messages for checks
#' @param Suite_results results of a test suite
#' @param dir directories where are situated checks
#'
#' @return table of checks descriptions
#' @export
Fair_table<-function(Suite_results,dir=directory){
  tab=c()
  checks <- list.files(dir)
  checks<-paste0(dir,"/",checks)
  all <- lapply(checks, read_xml)
  names(all) <- checks
  all <- lapply(all, xml_find_all, "type")
  all <- lapply(all, xml_text)
  
  for (i in 1:length(Suite_results)){
    status=Suite_results[[i]]$value$status[[1]]
    message=Suite_results[[i]]$value$output[[1]][[1]]
    if (status=="FAILURE"){
      tab=rbind(tab,c("Failure",message,all[[i]]))
    }
    if (status=="WARNING"){
      tab=rbind(tab,c("Warning",message,all[[i]]))
    }
    if (status=="SUCCESS"){
      tab=rbind(tab,c("Success",message,all[[i]]))
    }
  }
  tab=data.frame(tab)
  colnames(tab)=c("Status","Message","FAIR")
  tab<-tab[order(tab$Status),]#reorder for visualisation
  return(tab)
}

#' Fair_pie
#' Make a pie chart describing the results
#' @param Suite_results results of the suite of test
#'
#' @return a pie chart
#' @export
Fair_pie<-function(Suite_results){
  tab=Fair_table(Suite_results,dir=directory)
  
  tab$Status <- factor(tab$Status,levels = c("Success", "Failure","Warning")) #reorder for visualisation
  
  # Modify data to use for a graph
  data <- data.frame(
    group=c("Success","Failure","Warning"),
    value=c(table(tab$Status)[['Success']],table(tab$Status)[['Failure']],table(tab$Status)[['Warning']])
  )
  data$group <- factor(data$group,levels = c("Success", "Failure","Warning")) #reorder for visualisation
  # Compute percentages
  data$fraction <- data$value / sum(data$value)
  
  # Compute the cumulative percentages
  data$ymax <- cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  data$label <- paste0(data$group, "\n ", data$value)
  
  # Make the plot
  return(ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
           geom_rect(color="white") +
           geom_text( x=2, aes(y=labelPosition, label=label, color=group), size=6) + # x here controls label position (inner / outer)
           scale_fill_manual(values=c("#38c535", "#f70e00", "#ff840a"))+
           scale_color_manual(values=c("#38c535", "#f70e00", "#ff840a"))+
           coord_polar(theta="y") +
           xlim(c(-1, 4)) +
           theme_void() +
           theme(legend.position = "none"))
}


# Make checks
directory= paste0(path_checks)
res=metadig::runSuite(paste0(path_files,"/Suite.xml"),directory,metadata)

pdf(file = "./Fair_Quality_Report.pdf")
Fair_pie(res)
Fair_scores(res)
dev.off()
