# This file contains a set of functions useful for analyzing eo4water weather

#' gets data for a given coordinates from a given data source
#' @param src dplyr database connection
#' @param x x coordinate of the point of interest
#' @param y y coordinate of the point of interest
#' @param source data source name (boku/bolam/metgis/openweather)
get_meteo_data = function(src, x, y, source) {
  query = "
  SELECT
  point_id,
  st_x(point) AS x,
  st_y(point) AS y,
  st_distance(ST_GeographyFromText('SRID=4326;POINT(%f %f)'), point::geography) AS dist
  FROM swb.%s_points
  ORDER BY 4
  LIMIT 1"
  closestPoint = tbl(src, sql(sprintf(query, x, y, source)))

  meteoData = suppressMessages(
    tbl(src, sql(sprintf("SELECT * FROM swb.%s_data_daily", source))) %>%
      semi_join(closestPoint) %>%
      collect(n = Inf)
  )

  return(meteoData)
}

#' gets data for a given coordinates from all data sources
#' and merges them into a wide format
#' @param src dplyr database connection
#' @param x x coordinate of the point of interest
#' @param y y coordinate of the point of interest
#' @param from beginning of the time period
#' @param to ending of the time period
get_data_wide = function(src, x, y, from, to) {
  data = data_frame(date = as.Date(character()))
  prefixes = c(boku = 'bk_', bolam = 'bl_', metgis = 'mg_', openweather = 'ow_')
  for (source in c('boku', 'bolam', 'metgis', 'openweather')) {
    tmpData = get_meteo_data(src, x, y, source) %>%
      filter(date >= as.Date(from) & date <= as.Date(to)) %>%
      arrange(date)
    names(tmpData) = sub('^.*date$', 'date', paste0(prefixes[source], names(tmpData)))
    data = suppressMessages(
      data %>%
        full_join(tmpData)
    )
  }
  return(data)
}

#' gets data for a given coordinates from all data sources
#' and merges them into a long format
#' @param src dplyr database connection
#' @param x x coordinate of the point of interest
#' @param y y coordinate of the point of interest
#' @param from beginning of the time period
#' @param to ending of the time period
get_data_long = function(src, x, y, from, to) {
  data = list()
  codes = c(boku = 'bk', bolam = 'bl', metgis = 'mg', openweather = 'ow')
  for (source in c('boku', 'bolam', 'metgis', 'openweather')) {
    tmpData = get_meteo_data(src, x, y, source) %>%
      filter(date >= as.Date(from) & date <= as.Date(to)) %>%
      arrange(date) %>%
      mutate(source = codes[source])
    data[[length(data) + 1]] = tmpData
  }
  data = bind_rows(data)
  return(data)
}
