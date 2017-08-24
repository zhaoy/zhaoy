#' HIV status summaries
#'
#' @description
#' Graphs cd4 counts, viral loads in millions, and drug names, over time.
#'
#' @usage
#' s_hiv(df, date, cd4, vl, med)
#'
#' @param df a table for which a summary is desired.
#' @param date a column of dates
#' @param cd4 a column of cd4 counts
#' @param vl a column of viral loads
#' @param med a column of drug names
#'
#' @return A graph.
#'
#' @import ggplot2 ggrepel
#'
#' @export

s_hiv <- function(df, date, cd4, vl, med) {

  ggplot2::ggplot(data = df,
                  mapping = aes(x = date,
                                y = cd4)) +
    geom_line(mapping = aes(y = cd4,
                            color = "cd4 count"),
              na.rm = FALSE,
              size = 1) +
    geom_point(mapping = aes(y = cd4),
               size = 2,
               shape = 21,
               fill = "white") +
    geom_line(mapping = aes(y = vl / 0.003,
                            color = "viral load"),
              na.rm = FALSE,
              size = 1) +
    geom_point(mapping = aes(y = vl / 0.003),
               size = 2,
               shape = 21,
               fill = "white") +
    geom_hline(mapping = aes(yintercept = 200),
               color = "#D55E00",
               linetype = "dashed") +
    coord_cartesian(ylim = c(0,
                             1000)) +
    scale_y_continuous(breaks = seq(from = 0,
                                    to = 1000,
                                    by = 100),
                       sec.axis = sec_axis(trans = ~. * 0.003,
                                           breaks = seq(from = 0,
                                                        to = 3,
                                                        by = 0.5),
                                           name = "viral load (millions)")) +
    labs(x = "year",
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
                                  "#009E73")) +
    ggrepel::geom_label_repel(mapping = aes(y = cd4,
                                            label = med),
                              color = "black",
                              size = 4) +
    ggrepel::geom_label_repel(mapping = aes(y = vl / 0.003,
                                            label = med),
                              color = "black",
                              size = 4)

}