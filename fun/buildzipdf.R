build.zip.df <- function(x) {

  # For testing
  # x <- "http://zipatlas.com/us/wi/zip-code-comparison/population-below-poverty-level.htm"

  tmp <- read_html(x) %>%
    html_node("table") %>%
    html_table(fill=TRUE)

  # Clean up junk in the table
  tmp <- as_tibble(tmp[1:7])
  tmp <- tmp[-c(1:12), ]
  names(tmp) <- tmp[1, ]
  tmp <- tmp[-1, ]
  tmp <- tmp[-c(101:105), ]

  return(tmp)
}