GeomTimeLine <- ggplot2::ggproto(
  "GeomTimeLine",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    shape = 19,
    colour = "black",
    size = 1.5,
    fill = NA,
    alpha = 0.5,
    stroke = 0.5,
    date = NULL
  ),
  setup_data = function(data, params) {
    data <-
      subset(data, data$date > params$xmin &
               data$date < params$xmax)
    data
  },
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        xmin,
                        xmax) {
    coords <- coord$transform(data, panel_params)
    grid::pointsGrob(
      coords$x,
      coords$y,
      pch = coords$shape,
      gp = grid::gpar(
        col = ggplot2::alpha(coords$colour, coords$alpha),
        fill = ggplot2::alpha(coords$fill, coords$alpha),
        # Stroke is added around the outside of the point
        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      )
    )
  }
)

GeomTimeLineLabel <- ggplot2::ggproto(
  "GeomTimeLineLabel",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    countries = NULL,
    size = 1.5,
    location_name = NULL
  ),
  setup_data = function(data, params) {
    data <-
      subset(data, data$date > params$xmin &
               data$date < params$xmax)
    print(data)
    data <- data[order(data$countries,-data$size), ]
    data <- by(data, data["countries"], head, n = params$n_max)
    data <- Reduce(rbind, data)
    data
  },
  draw_panel = function(data,
                        panel_scales,
                        coord,
                        xmin,
                        xmax,
                        n_max) {
    coords <- coord$transform(data, panel_scales)
    coords$y2 = coords$y + 0.1
    grid::grobTree(
      grid::segmentsGrob(
        x0 = coords$x,
        y0 = coords$y,
        x1 = coords$x,
        y1 = coords$y2,
        gp = grid::gpar(col = "grey")
      ),
      grid::textGrob(
        x = coords$x,
        y = coords$y2,
        label = coords$location_name,
        hjust = -0.1,
        vjust = -0.1,
        rot = 45,
        gp = grid::gpar(col = coords$colour, fontsize = 10)
      )
    )
  }
)

#' This geom will plot a time line of earthquakes ranging from xmin to xmaxdates with a point for
#' each earthquake.
#'
#' Plot a time line of earthquakes ranging from xmin to xmaxdates with a point for
#' each earthquake. Optional aesthetics include color, size, and alpha (for transparency).
#' The xaesthetic is a date and an optional y aesthetic is a factor indicating
#' some stratification in which case multiple time lines will be plotted for each level
#' of the factor (e.g. country).
#'
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If \code{NULL}, the default, the data is inherited from the plot
#'    data as specified in the call to \code{\link{ggplot}}.
#'
#'    A \code{data.frame}, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    \code{\link{fortify}} for which variables will be created.
#'
#'    A \code{function} will be called with a single argument,
#'    the plot data. The return value must be a \code{data.frame.}, and
#'    will be used as the layer data.
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply \code{mapping} if there is no plot mapping.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param xmin Minimum X Axis limit, Start date year
#' @param xmax Maximum X Axis limit, End date year
#' @param na.rm If \code{FALSE}, the default, missing values are removed with
#'   a warning. If \code{TRUE}, missing values are silently removed.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#'
#' @examples
#' \dontrun{
#'  ggplot(
#'   data = merged_date_data,
#'   aes(
#'     x = DATE,
#'     y = COUNTRY,
#'     colour = merged_date_data$TOTAL_DEATHS,
#'     size = merged_date_data$EQ_PRIMARY,
#'     date = merged_date_data$DATE,
#'     countries = merged_date_data$COUNTRY,
#'     location_name = merged_date_data$LOCATION_NAME
#'   )
#' ) +
#'   geom_timeLine(xmin = START_DATE,
#'                 xmax = END_DATE)
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeLine <-
  function(data = NULL,
           mapping = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           xmin = NULL ,
           xmax = NULL,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomTimeLine,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        xmin = xmin ,
        xmax = xmax,
        ...
      )
    )
  }

#' This geom adds a vertical line to each data point with a text annotation
#'
#' This geom adds a vertical line to each data point with a text
#' annotation (e.g. the location of the earthquake) attached to each line with
#' option to subset to n_max number of earthquakes, where we take the n_max
#' largest (by magnitude) earthquakes.
#' Aesthetics are x, which is the date of the earthquake and label which takes
#' the column name from which annotations will be obtained.
#'
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If \code{NULL}, the default, the data is inherited from the plot
#'    data as specified in the call to \code{\link{ggplot}}.
#'
#'    A \code{data.frame}, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    \code{\link{fortify}} for which variables will be created.
#'
#'    A \code{function} will be called with a single argument,
#'    the plot data. The return value must be a \code{data.frame.}, and
#'    will be used as the layer data.
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply \code{mapping} if there is no plot mapping.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param xmin Minimum X Axis limit, Start date year
#' @param xmax Maximum X Axis limit, End date year
#' @param n_max Number of Label to plot
#' @param na.rm If \code{FALSE}, the default, missing values are removed with
#'   a warning. If \code{TRUE}, missing values are silently removed.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#'
#' @examples
#' \dontrun{
#'  ggplot(
#'   data = merged_date_data,
#'   aes(
#'     x = DATE,
#'     y = COUNTRY,
#'     colour = merged_date_data$TOTAL_DEATHS,
#'     size = merged_date_data$EQ_PRIMARY,
#'     date = merged_date_data$DATE,
#'     countries = merged_date_data$COUNTRY,
#'     location_name = merged_date_data$LOCATION_NAME
#'   )
#' ) +
#'   geom_timeLine(xmin = START_DATE,
#'                 xmax = END_DATE) +
#   geom_timeline_label(xmin = START_DATE,
#                       xmax = END_DATE,
#                       n_max = N_MAX)
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label <-
  function(data = NULL,
           mapping = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = FALSE,
           inherit.aes = TRUE,
           xmin = NULL ,
           xmax = NULL,
           n_max = 5,
           ...) {
    ggplot2::layer(
      geom = GeomTimeLineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        xmin = xmin,
        xmax = xmax,
        n_max = n_max,
        ...
      )
    )
  }

#' Timeline Them to represent earthquakes on yeary timeframe
#'
#' @return Plot them of White background with X aes as timeline
#'
#' @examples
#' \dontrun{
#'  them_timeline()
#' }
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#'
#' @export
them_timeline <-  function() {
  ggplot2::theme(
    legend.position = "bottom",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      colour = "gray",
      size = 1,
      linetype = "solid"
    ),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(
      colour = "black",
      size = 1 ,
      linetype = "solid"
    )
  )
}

#' Visualize earthquakes data on leaflet Map.
#'
#' This function takes an argument data containing the filtered data frame with earthquakes
#' to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates
#' each point with in pop up window containing annotation data stored in a column of
#' the data frame.
#'
#' @param data_eq Filtered dataframe of NOAA earthquakes dataset
#' @param annot_col Column name from dataset used for the annotation in the pop-up
#'
#' @return leaflet MAP shown with a circle markers, and the radius of the circle marker is
#'          proportional to the earthquake's magnitude.
#' @examples
#' \dontrun{
#'  eq_map(data_eq, annot_col = "DATE")
#'
#'  data_eq %>%
#'        dplyr::filter(COUNTRY %in% COUNTRIES & lubridate::year(DATE) >= 2000) %>%
#'        eq_map(annot_col = "DATE")
#' }
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @export
eq_map <- function(data_eq, annot_col) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      data = data_eq,
      radius = data_eq$EQ_PRIMARY ,
      stroke = FALSE,
      fillOpacity = 0.5,
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      popup  = data_eq[[annot_col]]
    )
}

#' Creates an HTML label to use in Popup text for Location, Magnitude and Total number of Deaths.
#'
#' Creates an HTML label that can be used as the annotation text in the leaflet map.
#' This function put together a character string for each earthquake that will show
#' the cleaned location , the magnitude (EQ_PRIMARY), and
#' the total number of deaths (TOTAL_DEATHS),
#' with boldface labels for each ("Location", "Total deaths", and "Magnitude").
#' If an earthquake is missing values for any of these, both the label and the
#' value will be skipped for that element of the tag.
#'
#' @param data_eq Filtered dataframe of NOAA earthquakes dataset
#'
#' @return Popup text in Html lable for Location, Magnitude and Total number of Deaths
#'
#' @examples
#' \dontrun{
#'  eq_create_label(data_eq)
#'
#'  data_eq %>%
#'        dplyr::filter(COUNTRY %in% COUNTRIES & lubridate::year(DATE) >= 2000) %>%
#'        dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'        eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data_eq) {
  location_name <-
    ifelse(
      is.na(data_eq$LOCATION_NAME),
      "",
      paste("<b>Location:</b>", data_eq$LOCATION_NAME)
    )
  magnitude <-
    ifelse(
      is.na(data_eq$EQ_PRIMARY),
      "",
      paste("<br/><b>Magnitude:</b>", data_eq$EQ_PRIMARY)
    )
  deaths <-
    ifelse(
      is.na(data_eq$TOTAL_DEATHS),
      "",
      paste("<br/><b>Total deaths:</b>", data_eq$TOTAL_DEATHS)
    )
  paste(location_name, magnitude, deaths)
}

#' Reads NOAA earthquakes data set from .txt file and convert to clean datafrmae object
#'
#' This function takes raw NOAA data frame and returns a clean data frame-
#'  1) A date column created by uniting the year, month, day
#'      and converting it to the Date class
#'  2) LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @param raw_data_file  raw NOAA dataframe .txt file location
#' @return clean dataframe with Date, latitude, longitude conversion.
#'
#' @examples
#' \dontrun{
#'  eq_clean_data("signif.txt")
#' }
#'
#' @importFrom readr read_delim
#' @importFrom tidyr unite
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom lubridate ymd
#'
#' @export
eq_clean_data <- function(raw_data_file) {
  raw_data <- readr::read_delim(file = raw_data_file, delim = "\t")
  eq_cl_data <- raw_data  %>%
    #Remove values less than 1000 year as they are giving warnings in unite parsing
    dplyr::filter(YEAR > 1000) %>%
    tidyr::drop_na(YEAR, MONTH, DAY) %>%
    tidyr::unite(DATE, YEAR, MONTH, DAY, remove = FALSE) %>%
    tidyr::drop_na(DATE) %>%
    dplyr::mutate(DATE = as.Date(lubridate::ymd(DATE))) %>%
    dplyr::mutate(LATITUDE  = as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
    dplyr::mutate(EQ_PRIMARY = as.double(EQ_PRIMARY))
  return(eq_cl_data)
}

#' Converts Location name to Title case
#'
#' cleans the LOCATION_NAME column by stripping out the country name (including the colon) and
#'  converts names to title case (as opposed to all caps). This will be needed later for
#'  annotating visualizations.
#'
#' @param eq_dataset_location NOAA earthquakes dataset
#'
#' @return Location name with Title case
#'
#' @examples
#' \dontrun{
#'  eq_data$LOCATION_NAME <- eq_location_clean(eq_data)
#' }
#'
#' @importFrom tools toTitleCase
#'
#' @export
eq_location_clean <- function(eq_dataset_location) {
  eq_dataset_location$LOCATION_NAME <-
    tools::toTitleCase(tolower(trimws(
      gsub(".*:", "", eq_dataset_location$LOCATION_NAME)
    )))
}
