concatenate_multiple_r_files <- function(output, input) {
  
  sink(output)
  
  concatenate <- function(files) {
    
    current_file = readLines(files)
    # cat("\n\n#### Current file:",files,"\n\n")
    cat("\n\n")
    cat(current_file, sep ="\n")
    
  }
  
  fim <- map(input, concatenate)
  
  fimfim <- do.call(c, fim)
  
  sink()
  
}


# files <-  c("teste_spin.R", "teste_spin2.R")
# 
# concatenate_multiple_r_files("oooi.R", files)
  