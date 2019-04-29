# set the seqkit path for different OS
set_seqkit_path <- function(){
  
  sys_info <- Sys.info()
  sysname <- sys_info[['sysname']]
  if (sysname == "Windows"){
    seqkit_path <- "tools/seqkit_windows.exe"
  } else if ( sysname == "Linux"){
    seqkit_path <- "tools/seqkit_linux"
  } else if ( sysname == "Darwin"){
    seqkit_path <- "tools/seqkit_darwin"
  } else{
    stop(paste0( sysname, " is not supported" ))
  }
  
  return( seqkit_path)
}

seqkit_get_name <- function(ref){
  
  
  seqkit_path <- set_seqkit_path()
  cmd <- sprintf("%s seq -n %s", seqkit_path, ref)
  chroms <- system(cmd, intern = TRUE)
  
  return(chroms)
  
}


# extract
seqkit_extract_genomic <- function(ref, chr, start, end, ...){
  
  seqkit_path <- set_seqkit_path()
  
  # if start is larger than end, 
  # return reverse complement seq
  
  build_idx <- sprintf("%s faidx %s", seqkit_path, ref)
  system(build_idx)
  
  if (start <= end){
    cmd <- sprintf("%s faidx %s %s:%d-%d", seqkit_path, ref, chr, start, end)
    #print(cmd)
    seq_result <- system(cmd, intern = TRUE)
  } else{
    
    tmp_file <- file.path(tempdir(), "tmp.fa")
    cmd1 <- sprintf("%s faidx %s %s:%d-%d -o %s", 
                    seqkit_path, ref, chr, end, start, tmp_file)
    #print(cmd1)
    system(cmd1)
    
    cmd2 <- sprintf("%s seq -rp %s", seqkit_path, tmp_file)
    #print(cmd2)
    seq_result <- system(cmd2, intern = TRUE)
    
  }
  seq <- paste0( seq_result[1], "\n",
                 paste(seq_result[-1], collapse = ""), collapse = "" )
  #print("\n")
  #print(seq)
  return(seq)
  
}

seqkit_extract_transcript <- function(ref, id){
  
  seqkit_path <- set_seqkit_path()
  cmd <- sprintf("%s faidx %s ", seqkit_path, ref, id)
  seq_result <- system(cmd)
  
}


# ref <- "Athaliana.fa"
# chr <- "Chr1"
# start <- 1
# end <- 200
# cmd <- sprintf("%s faidx %s %s:%d-%d", seqkit_path, ref, chr, start, end)
# seq_result <- system(cmd, intern = TRUE)
# print(seq_result)


