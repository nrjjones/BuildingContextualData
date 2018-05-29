
get20results <- function(x) {

  thingtosearch <- gsub(" ", "+", x)

  Gurl <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=",
               thingtosearch,
               "&radius=50000",
               "&key=",
               Gkey)

  tmp <- GET(Gurl)
  tmp2 <- content(tmp, "text")
  tmp3 <- fromJSON(tmp2)

  #print(as.data.frame(tmp3$results$name))

  tmp4 <- bind_cols(
      name = tmp3$results$name,
      lon = tmp3$results$geometry$location$lng,
      lat = tmp3$results$geometry$location$lat,
      address = tmp3$results$formatted_address
    )

  return(tmp4)
}


