#' HIV status summaries
#'
#' @description
#' Graphs cd4 counts, viral loads in millions, and medication names, over time.
#'
#' @usage
#' s_hiv(lab_df, lab_date, cd4, vl, med_df, med_date)
#'
#' @param lab_df a table of lab data.
#' @param lab_date a column of dates.
#' @param cd4 a column of cd4 counts.
#' @param vl a column of viral loads.
#' @param med_df a column of drug names.
#' @param med_date a column of dates.
#'
#' @return A graph.
#'
#' @import ggplot2 ggrepel
#'
#' @export

s_hiv <- function(lab_df,
                  lab_date,
                  cd4,
                  vl,
                  med_df,
                  med_date) {

  lab_date <- lab_df[, lab_date]

  cd4 <- lab_df[, cd4]

  vl <- lab_df[, vl]

  med_date <- med_df[, med_date]

  y_max_cd4 <- max(cd4,
                   na.rm = TRUE)

  y_max_vl <- max(vl / 0.003,
                  na.rm = TRUE)

  y_max <- max(y_max_cd4,
               y_max_vl,
               na.rm = TRUE)

  y_max <- y_max + 350

  med_df$txt <- rep(x = y_max - 100,
                    times = nrow(x = med_df))

  ggplot2::ggplot() +
    geom_line(data = lab_df,
              mapping = aes(x = lab_date,
                            y = cd4,
                            color = "cd4 count"),
              na.rm = FALSE,
              size = 1) +
    geom_point(data = lab_df,
               mapping = aes(x = lab_date,
                             y = cd4),
               size = 2,
               shape = 21,
               fill = "white") +
    geom_line(data = lab_df,
              mapping = aes(x = lab_date,
                            y = vl / 0.003,
                            color = "viral load"),
              na.rm = FALSE,
              size = 1) +
    geom_point(data = lab_df,
               mapping = aes(x = lab_date,
                             y = vl / 0.003),
               size = 2,
               shape = 21,
               fill = "white") +
    geom_line(data = med_df,
              mapping = aes(x = med_date,
                            y = txt),
              na.rm = FALSE,
              size = 1) +
    geom_point(data = med_df,
               mapping = aes(x = med_date,
                             y = txt),
               size = 2,
               shape = 21,
               fill = "white") +
    geom_hline(data = lab_df,
               mapping = aes(yintercept = 200),
               color = "#D55E00",
               linetype = "dashed") +
    scale_y_continuous(data = lab_df,
                       breaks = seq(from = 0,
                                    to = y_max,
                                    by = 100),
                       sec.axis = sec_axis(trans = ~. * 0.003,
                                           breaks = seq(from = 0,
                                                        to = y_max * 0.003,
                                                        by = 0.5),
                                           name = "viral load (millions)")) +
    coord_cartesian(ylim = c(0,
                             y_max)) +
    labs(x = "date",
         y = "cd4 count") +
    theme_bw(base_size = 12) +
    theme(axis.text.x  = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(face = "bold",
                                      size = 12),
          axis.title.y = element_text(face = "bold",
                                      size = 12),
          legend.background = element_rect(fill = "white",
                                           size = 0.5,
                                           linetype = "dotted"),
          legend.position = "top",
          legend.text = element_text(size = 12,
                                     face = "bold"),
          legend.title = element_blank()) +
    scale_color_manual(values = c("#56B4E9",
                                  "#009E73"))

}