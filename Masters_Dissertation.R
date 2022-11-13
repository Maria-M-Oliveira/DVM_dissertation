

library(tidyverse)
library(ggplot2)
library(ggforce)
library(grid)
library(readxl)

Temp <- read_excel("DB_Limpo.xlsx", sheet="Temperaturas")%>%
  as.data.frame() %>% 
  rename(temperatura=`Temperatura Receção Logística (ºC)`)

Hum <- read_excel("DB_Limpo.xlsx", sheet="HumidadeBSS")%>%
  as.data.frame() %>% 
  rename(hum = `Humidade (%)`,
         conf = `Conformidade legal`)

NC <- read_excel("DB_Limpo.xlsx", sheet="NC_Rececao") %>%
  as.data.frame() %>% 
  rename(razao= `Razão Não Conformidade`,
         tipo = `Tipo Não Conformidade`,
         rejeitado = `Rejeição (S/N)`)

NC_sem_peso <- subset(NC, razao != "Peso")

df <- table(NC$tipo, NC$rejeitado) %>% 
  as.data.frame.matrix()
  
# Statistical tests for association between variables
  
# Fisher's exact test to test for association between rejection and NC; rejection adn typer of NC
asso_NC_rej <- fisher.test(NC$razao,NC$rejeitado, simulate.p.value=TRUE)

asso_TNC_rej<- fisher.test(NC$tipo,NC$rejeitado, simulate.p.value=TRUE)

  
# --> Gráfico de barras de NC:    
# Functions that allow rotating NC bar graph
  
# https://stackoverflow.com/questions/52665619/how-to-change-the-position-of-the-zoomed-area-from-facet-zoom 
  
facet_zoom2 <- function(x, y, xy, zoom.data, xlim = NULL, ylim = NULL, 
                        split = FALSE, horizontal = TRUE, zoom.size = 2, 
                        show.area = TRUE, shrink = TRUE) {
  x <- if (missing(x)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(x)
  y <- if (missing(y)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(y)
  zoom.data <- if (missing(zoom.data)) NULL else lazyeval::lazy(zoom.data)
  if (is.null(x) && is.null(y) && is.null(xlim) && is.null(ylim)) {
    stop("Either x- or y-zoom must be given", call. = FALSE)
  }
  if (!is.null(xlim)) x <- NULL
  if (!is.null(ylim)) y <- NULL
  ggproto(NULL, FacetZoom2,
          shrink = shrink,
          params = list(
            x = x, y = y, xlim = xlim, ylim = ylim, split = split, zoom.data = zoom.data,
            zoom.size = zoom.size, show.area = show.area,
            horizontal = horizontal
          )
  )
}

FacetZoom2 <- ggproto(
  "FacetZoom2",
  ggforce::FacetZoom,
  
  compute_layout = function(data, params) {
    layout <- rbind( # has both x & y dimension
      data.frame(name = 'orig', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'x', SCALE_X = 2L, SCALE_Y = 1L),
      data.frame(name = 'y', SCALE_X = 1L, SCALE_Y = 2L),
      data.frame(name = 'full', SCALE_X = 2L, SCALE_Y = 2L),
      data.frame(name = 'orig_true', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'zoom_true', SCALE_X = 1L, SCALE_Y = 1L)
    )
    if (is.null(params$y) && is.null(params$ylim)) { # no y dimension
      layout <- layout[c(1,2, 5:6),]
    } else if (is.null(params$x) && is.null(params$xlim)) { # no x dimension
      layout <- layout[c(1,3, 5:6),]
    }
    layout$PANEL <- seq_len(nrow(layout))
    layout
  },
  
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params) {
    
    if (is.null(params$x) && is.null(params$xlim)) {
      params$horizontal <- TRUE
    } else if (is.null(params$y) && is.null(params$ylim)) {
      params$horizontal <- FALSE
    }
    if (is.null(theme[['zoom']])) {
      theme$zoom <- theme$strip.background
    }
    if (is.null(theme$zoom.x)) {
      theme$zoom.x <- theme$zoom
    }
    if (is.null(theme$zoom.y)) {
      theme$zoom.y <- theme$zoom
    }
    axes <- render_axes(ranges, ranges, coord, theme, FALSE)
    panelGrobs <- ggforce:::create_panels(panels, axes$x, axes$y)
    panelGrobs <- panelGrobs[seq_len(length(panelGrobs) - 2)]
    if ('full' %in% layout$name && !params$split) {
      panelGrobs <- panelGrobs[c(1, 4)]
    }
    
    # changed coordinates in indicator / lines to zoom from 
    # the opposite horizontal direction
    if ('y' %in% layout$name) {
      if (!inherits(theme$zoom.y, 'element_blank')) {
        zoom_prop <- scales::rescale(
          y_scales[[2]]$dimension(ggforce:::expansion(y_scales[[2]])),
          from = y_scales[[1]]$dimension(ggforce:::expansion(y_scales[[1]])))
        indicator <- polygonGrob(
          x = c(0, 0, 1, 1), # was x = c(1, 1, 0, 0), 
          y = c(zoom_prop, 1, 0), 
          gp = gpar(col = NA, fill = alpha(theme$zoom.y$fill, 0.5)))
        lines <- segmentsGrob(
          x0 = c(1, 1), x1 = c(0, 0), # was x0 = c(0, 0), x1 = c(1, 1)
          y0 = c(0, 1), y1 = zoom_prop,
          gp = gpar(col = theme$zoom.y$colour,
                    lty = theme$zoom.y$linetype,
                    lwd = theme$zoom.y$size,
                    lineend = 'round'))
        indicator_h <- grobTree(indicator, lines)
      } else {
        indicator_h <- zeroGrob()
      }
    }
    
    if ('x' %in% layout$name) {
      if (!inherits(theme$zoom.x, 'element_blank')) {
        zoom_prop <- scales::rescale(x_scales[[2]]$dimension(ggforce:::expansion(x_scales[[2]])),
                                     from = x_scales[[1]]$dimension(ggforce:::expansion(x_scales[[1]])))
        indicator <- polygonGrob(c(zoom_prop, 1, 0), c(1, 1, 0, 0), 
                                 gp = gpar(col = NA, fill = alpha(theme$zoom.x$fill, 0.5)))
        lines <- segmentsGrob(x0 = c(0, 1), y0 = c(0, 0), x1 = zoom_prop, y1 = c(1, 1), 
                              gp = gpar(col = theme$zoom.x$colour,
                                        lty = theme$zoom.x$linetype,
                                        lwd = theme$zoom.x$size,
                                        lineend = 'round'))
        indicator_v <- grobTree(indicator, lines)
      } else {
        indicator_v <- zeroGrob()
      }
    }
    
    if ('full' %in% layout$name && params$split) {
      space.x <- theme$panel.spacing.x
      if (is.null(space.x)) space.x <- theme$panel.spacing
      space.x <- unit(5 * as.numeric(convertUnit(space.x, 'cm')), 'cm')
      space.y <- theme$panel.spacing.y
      if (is.null(space.y)) space.y <- theme$panel.spacing
      space.y <- unit(5 * as.numeric(convertUnit(space.y, 'cm')), 'cm')
      
      # change horizontal order of panels from [zoom, original] to [original, zoom]
      # final <- gtable::gtable_add_cols(panelGrobs[[3]], space.x)
      # final <- cbind(final, panelGrobs[[1]], size = 'first')
      # final_tmp <- gtable::gtable_add_cols(panelGrobs[[4]], space.x)
      # final_tmp <- cbind(final_tmp, panelGrobs[[2]], size = 'first')
      final <- gtable::gtable_add_cols(panelGrobs[[1]], space.x)
      final <- cbind(final, panelGrobs[[3]], size = 'first')
      final_tmp <- gtable::gtable_add_cols(panelGrobs[[2]], space.x)
      final_tmp <- cbind(final_tmp, panelGrobs[[4]], size = 'first')
      
      final <- gtable::gtable_add_rows(final, space.y)
      final <- rbind(final, final_tmp, size = 'first')
      final <- gtable::gtable_add_grob(final, list(indicator_h, indicator_h),
                                       c(2, 6), 3, c(2, 6), 5,
                                       z = -Inf, name = "zoom-indicator")
      final <- gtable::gtable_add_grob(final, list(indicator_v, indicator_v), 
                                       3, c(2, 6), 5, 
                                       z = -Inf, name = "zoom-indicator")
      heights <- unit.c(
        unit(max_height(list(axes$x[[1]]$top, axes$x[[3]]$top)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$x[[1]]$bottom, axes$x[[3]]$bottom)), 'cm'),
        space.y,
        unit(max_height(list(axes$x[[2]]$top, axes$x[[4]]$top)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$x[[2]]$bottom, axes$x[[4]]$bottom)), 'cm')
      )
      
      # swop panel width specifications according to the new horizontal order
      widths <- unit.c(
        # unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        # unit(params$zoom.size, 'null'),
        # unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm'),
        # space.x,
        # unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        # unit(1, 'null'),
        # unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')        
        unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm'),
        space.x,
        unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm')
        
      )
      final$heights <- heights
      final$widths <- widths
    } else {
      if (params$horizontal) {
        space <- theme$panel.spacing.x
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        heights <- unit.c(
          unit(max_height(list(axes$x[[1]]$top, axes$x[[2]]$top)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$x[[1]]$bottom, axes$x[[2]]$bottom)), 'cm')
        )
        
        # change horizontal order of panels from [zoom, original] to [original, zoom]
        # first <- gtable::gtable_add_cols(panelGrobs[[2]], space)
        # first <- cbind(final, panelGrobs[[1]], size = 'first')
        final <- gtable::gtable_add_cols(panelGrobs[[1]], space) 
        final <- cbind(final, panelGrobs[[2]], size = "first") 
        
        final$heights <- heights
        
        # swop panel width specifications according to the new horizontal order
        # unit(c(params$zoom.size, 1), 'null')
        final$widths[panel_cols(final)$l] <- unit(c(1, params$zoom.size), 'null') 
        
        final <- gtable::gtable_add_grob(final, indicator_h, 2, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      } else {
        space <- theme$panel.spacing.y
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        widths <- unit.c(
          unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')
        )
        final <- gtable::gtable_add_rows(panelGrobs[[1]], space)
        final <- rbind(final, panelGrobs[[2]], size = 'first')
        final$widths <- widths
        final$heights[panel_rows(final)$t] <- unit(c(1, params$zoom.size), 'null')
        final <- gtable::gtable_add_grob(final, indicator_v, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      }
    }
    final
  }
)


bar_graph_NC <- ggplot(NC, aes(razao, fill=razao))+ 
  geom_bar() + facet_zoom2(ylim=c(0,70)) + 
  scale_x_discrete("Non Conformity", labels= NULL) + 
  scale_fill_manual(values= c("#90C796", "#E68AA9", "#EDAA98", "#C8566B", "#C7E38D", "#83B3E0", "#FEB07C", "#B8D1C0", "#9D75BF", "#FFF0AB", "#AEEBDC", "#6A75BA"), name="Non Conformities", labels= c("Packaging defects","Splitting defects", "Dun", "Classification Error", "Excessive humidity", "Foul odor", "Presence of parasites", "Physical hazard", "Net weight defects", "Labelling flaws", "Rouge", "High temperature")) + 
  labs( y="n", title= "Non Conformities Jan 2017-Jan 2022") + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.6) +
  theme_bw()

bar_graph_NC_sem_peso <- ggplot(NC_sem_peso, aes(razao, fill=razao))+ 
  geom_bar() + 
  scale_x_discrete("Non Conformity", labels= NULL) + 
  scale_fill_manual(values= c("#90C796", "#E68AA9", "#EDAA98", "#C8566B", "#C7E38D", "#83B3E0", "#FEB07C", "#B8D1C0", "#FFF0AB", "#AEEBDC", "#6A75BA"), name="Non Conformities", labels= c("Packaging defects","Splitting defects", "Dun", "Classification Error", "Excessive humidity", "Foul odor", "Presence of parasites", "Physical hazard", "Labelling flaws", "Rouge", "High temperature")) + 
  labs( y="n", title= "Non Conformities (without considering weight defects) Jan 2017-Jan 2022") + 
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.6) +
  theme_bw()

# --> Gráfico pontos Humidade com linhas horizontais de média e limite
Hum_graph <- ggplot(Hum) + 
  geom_point(aes(Hdata, humidade))+ 
  theme_classic() + 
  geom_hline(aes(yintercept = 47, colour="Legal Limit (47%)")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +		  
  scale_colour_manual(values= c("red")) +
  labs(x="Year", 
       y="Relative Humidity (%)", 
       title="Relative humidity (%) 2017-2022",
       color=NULL)

# --> Gráfico extremos e quartis Humidade com linha horizontal limite legal
Hum_boxplot <- ggplot(Hum, aes(Hum$`Conformidade legal`, Hum$`Humidade (%)`)) +
  geom_boxplot()+
  geom_hline(aes(yintercept=47, color="Legal Limit (47%)")) +
  scale_colour_manual(values=c("Green")) +
  labs(x= "Legal Conformity",
       y= "Relative Humidity",
       title="Box-plot Relative Humidity 2017-2022",
       color=NULL)

# --> Histograma de temperaturas receção
Temp_hist<- ggplot(Temp) +
  geom_histogram(aes(temperatura), col="grey") +
  geom_vline(aes(xintercept=7, colour="Legal Limit (7ºC)")) +
  scale_color_manual(values=c("Red"))+
  labs(x="Temperature at Reception",
       y="n",
       title="Temperature at Reception 2017-2022",
       color=NULL) +
  theme_classic()
