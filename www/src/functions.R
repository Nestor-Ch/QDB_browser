numbers_to_string <- function(vec) {
  
  if (length(vec)==1){
    return(as.character(vec))
  }else{
    
    ranges <- c()
    start_range <- vec[1]
    for (i in 2:length(vec)) {
      if (vec[i] != vec[i - 1] + 1) {
        if (start_range != vec[i - 1]) {
          ranges <- append(ranges, paste(start_range, vec[i - 1], sep = "-"))
        } else {
          ranges <- append(ranges, as.character(start_range))
        }
        start_range <- vec[i]
      }
    }
    # Handle the last range
    if (start_range != vec[length(vec)]) {
      ranges <- append(ranges, paste(start_range, vec[length(vec)], sep = "-"))
    } else {
      ranges <- append(ranges, as.character(start_range))
    }
    result <- paste(ranges, collapse = ",")
    return(result)}
}