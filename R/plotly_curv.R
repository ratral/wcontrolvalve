
#' Plottly the Valve flow coefficient
#'
#' @param valves tibble with the valves factors
#' @param i cylinder order
#' @import plotly
#' @return plot in gglot format
#' @export
#'
  plotly_kv_kvs <- function( valves, i ){
    # https://plotly-r.com/index.html
    # https://plotly.com/r/axes/
    # https://plotly.com/r/hover-text-and-formatting/

    data_points <- valves$data[[i]] %>%
      select( .data$position, .data$kv_kvs) %>%
      mutate( kv_kvs = .data$kv_kvs*100)

    data.kv_kvs <- tibble(position = 0:100) %>%
      mutate( kv_kvs = map_dbl( .x = .data$position,
                                ~drm_LL3( .x,
                                          valves$kv_b[i],
                                          valves$kv_d[i],
                                          valves$kv_e[i]))*100)

    h.template <- paste( "%{yaxis.title.text}: %{y:.1f}<br>",
                         "%{xaxis.title.text}: %{x:.1f}",
                         "<extra></extra>")

    p <-  plot_ly() %>%
      add_trace( data = data.kv_kvs, x = ~position, y = ~kv_kvs,
                 type = "scatter", mode = "lines",
                 hovertemplate = h.template) %>%
      add_trace( data = data_points, x = ~position, y = ~kv_kvs,
                 type = "scatter", mode = "markers",
                 hovertemplate = h.template, color = I("black")) %>%
      layout( title = paste("Valve typ:", valves$name[i]),
              xaxis = list( title = "Valve Position (%)"),
              yaxis = list( title = "Kv/Kvs (%)"),
              showlegend = FALSE) %>%
      config( displayModeBar = FALSE )

    return(p)

  }


#' Plottly Flow Coefficient (Kv) of the valve
#'
#' @param valves tibble with the valves factors
#' @param i cylinder order
#' @import plotly
#' @return plotly graph
#' @export
#'
  plotly_kv <- function( valves, i ){

    data_points <- valves$data[[i]] %>%
      select( .data$position, .data$kv)

    data.kv <- tibble(position = 0:100) %>%
      mutate( kv = map_dbl( .x = .data$position,
                                ~drm_LL3( .x,
                                          valves$kv_b[i],
                                          valves$kv_d[i],
                                          valves$kv_e[i]))*valves$kvs[i])

    h.template <- paste( "%{yaxis.title.text}: %{y:,.1f}<br>",
                         "%{xaxis.title.text}: %{x:.1f}",
                         "<extra></extra>")

    p <-  plot_ly() %>%
      add_trace( data = data.kv, x = ~position, y = ~kv,
                 type = "scatter", mode = "lines",
                 hovertemplate = h.template) %>%
      add_trace( data = data_points, x = ~position, y = ~kv,
                 type = "scatter", mode = "markers",
                 hovertemplate = h.template, color = I("black")) %>%
      layout( title = paste("Valve typ:", valves$name[i]),
              xaxis = list( title = "Valve Position (%)"),
              yaxis = list( title = "Kv (m^3/h)"),
              showlegend = FALSE) %>%
      config( displayModeBar = FALSE )

    return(p)

  }

#' Plottly Cavitation Coefficients
#'
#' @param valves tibble with the valves factors
#' @param i cylinder order
#' @import plotly
#' @return plotly graph
#' @export
#'
  plotly_sigma <- function( valves, i ){

    data_points <- valves$data[[i]] %>%
      select( .data$position, .data$sig_1, .data$regime)

    data.sigma <- tibble(position = 0:100)%>%
      mutate( sigma_i = map_dbl(.x = .data$position,
                                ~Sigma_i( .x, valves$kv_b[i], valves$kv_d[i],
                                          valves$kv_e[i], valves$fls[i])),
              sigma_c = map_dbl(.x = .data$position,
                                ~Sigma_c( .x, valves$kv_b[i], valves$kv_d[i],
                                          valves$kv_e[i], valves$fls[i])),
              sigma_mv = map_dbl(.x = .data$position,
                                 ~Sigma_mv( .x, valves$kv_b[i], valves$kv_d[i],
                                            valves$kv_e[i], valves$fls[i])))

    i.template <- paste( "<b>Incipient Cavitation</b> <br>",
                         "%{yaxis.title.text}: %{y:.2f}<br>",
                         "%{xaxis.title.text}: %{x:.1f}",
                         "<extra></extra>")

    c.template <- paste( "<b>Constant Cavitation</b> <br>",
                         "%{yaxis.title.text}: %{y:.2f}<br>",
                         "%{xaxis.title.text}: %{x:.1f}",
                         "<extra></extra>")

    m.template <- paste( "<b>Maximum Vibration Cavitation:</b> <br>",
                         "%{yaxis.title.text}: %{y:.2f}<br>",
                         "%{xaxis.title.text}: %{x:.1f}",
                         "<extra></extra>")

    p.template <- paste( "<b>Sigma Value:</b> <br>",
                         "%{yaxis.title.text}: %{y:.2f}<br>",
                         "%{xaxis.title.text}: %{x:.1f}",
                         "<extra></extra>")

    p <-  plot_ly() %>%
      add_trace( data = data.sigma, x = ~position, y = ~sigma_i, type = "scatter",
                 mode = "lines", hovertemplate = i.template, name = "Incipient C.",
                 color = I("green")) %>%
      add_trace( data = data.sigma, x = ~position, y = ~sigma_c, type = "scatter",
                 mode = "lines", hovertemplate = c.template, name = "Constant C.",
                 color = I("orange")) %>%
      add_trace( data = data.sigma, x = ~position, y = ~sigma_mv, type = "scatter",
                 mode = "lines", hovertemplate = m.template, name = "Maximum C.",
                 color = I("red")) %>%
      add_trace( data = data_points, x = ~position, y = ~sig_1, type = "scatter",
                 mode = "markers", hovertemplate = p.template, name = "Sigma Values",
                 color = I("black")) %>%
      layout( title = paste("Valve typ:", valves$name[i]),
              xaxis = list( title = "Valve Position (%)"),
              yaxis = list( title = "Sigma Value"),
              showlegend = TRUE,
              legend = list(x = 0.05, y = 0.95)) %>%
      config( displayModeBar = FALSE )

    return(p)

  }

