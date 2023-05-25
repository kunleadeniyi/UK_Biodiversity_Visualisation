library(maps)
library(ggplot2)
library(gridExtra)
library(dplyr)

# for images
library(magick)
library(grDevices)

# for layout
library(patchwork)

# graphics device
#options(device="quartz")
#quartz( width=11.5, height=8.5)

# azure color
azure_color <- '#007fff'
gold_color <- 'gold'
text_color <- 'navy'

# set font
#theme_set(theme_bw(base_family = 'Avenir Black' ))
theme_set(theme_void(base_family = 'Avenir'))

#library(dplyr)
# plot image in the graphics device
  dragonfly_image <- image_read("data/Aeshna_caerulea.jpg")
  dragonfly_image_lateral <- image_read("data/Aeshna_caerulea_lateral_flipped_orig.png")

  # and extract useful quantities such as the height and width 
  info_dragonfly_image <- image_info(dragonfly_image)
  info_dragonfly_image$width
  info_dragonfly_image$height
  # we can use this information to specify a plot or items in a plot
  
  # to plot the image we can simply write...  
    #image_plot <- plot( dragonfly_image )
  
# read nbn data for azure hawker species (Aeshna caerulea)
  nbn_aeshna_caerulea <- read.csv('data/aeshna caerulea azure hawker dragonfly/records.csv')
  
  # 2015 sample
  #nbn_2015 <- nbn_aeshna_caerulea[nbn_aeshna_caerulea$Start.date.year == 2015, ]
  
  # 2016
  #nbn_2016 <- nbn_aeshna_caerulea[nbn_aeshna_caerulea$Start.date.year == 2016, ]
  
  # take only important columns
  nbn_plot_data <- nbn_aeshna_caerulea[c("Start.date.year", "Longitude..WGS84.", "Latitude..WGS84.", "Verification") ]
  
  # during CEH
  nbn_plot_data_1 <- nbn_plot_data[nbn_plot_data$Start.date.year >= 1976, ]
  nbn_plot_data_1 <- nbn_plot_data[nbn_plot_data$Start.date.year <= 2015, ]
  
  # before and after CEH data
  before_1976_nbn_plot_data <- nbn_plot_data[nbn_plot_data$Start.date.year < 1976, ]
  after_2015_nbn_plot_data <- nbn_plot_data[nbn_plot_data$Start.date.year > 2015, ]
  
  agg_nbn_plot_data <- aggregate(nbn_plot_data$Start.date.year,
                  by = list(nbn_plot_data$Start.date.year),
                  FUN = length)
  names(agg_nbn_plot_data) <- c('year', 'occurence_count')
    
  
    UK <- map_data("world") %>% filter(region=="UK")
    
    # image
    image_ggplot_1 <- image_ggplot(dragonfly_image) 
    image_ggplot_2 <- image_ggplot(dragonfly_image_lateral)
  
    
    # before 1976
    left_plot <- ggplot() +
      geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)  +
      #geom_point( data=data, aes(x=long, y=lat)) +
      #theme_void() + # commented out to allow each small multiple map inherit the font set using theme_set()
      ylim(50,59) + coord_map() +
      geom_point(
        data=before_1976_nbn_plot_data, 
        aes(x=Longitude..WGS84., y=Latitude..WGS84., color=Verification),
        size=0.5, #colour=azure_color #'red'
        show.legend = F
      ) +
      scale_color_manual(values = c(azure_color, gold_color)) +
      ggtitle(paste0("Before 1976")) +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 3, family = "Avenir Black"),
      ) +
      facet_wrap(~ Start.date.year,  ncol = 8) 
    
    # after 2015
    right_plot <- ggplot() +
      geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)  +
      #geom_point( data=data, aes(x=long, y=lat)) +
      #theme_void() + 
      ylim(50,59) + coord_map() +
      geom_point(
        data=after_2015_nbn_plot_data, 
        aes(x=Longitude..WGS84., y=Latitude..WGS84., color=Verification),
        #show.legend = TRUE,
        size=1, #colour=azure_color #'red'
      ) +
      scale_color_manual(values = c(azure_color, gold_color)) +
      ggtitle(paste0("After 2015")) +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 3, family = "Avenir Black"),
      ) +
      facet_wrap(~ Start.date.year,  ncol = 3)
   
    # big middle map
    big_plot <- ggplot() +
      geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)  +
      #geom_point( data=data, aes(x=long, y=lat)) +
      #theme_void() + 
      ylim(50,59) + coord_map() +
      geom_point(
        data=nbn_plot_data_1, 
        aes(x=Longitude..WGS84., y=Latitude..WGS84., color=Verification),
        size=1, show.legend = F, 
        #colour=azure_color #'red'
      ) +
      scale_color_manual(values = c(azure_color, gold_color)) +
      ggtitle(paste0("1976-2015"), subtitle = "During the CEH study" ) +
      theme(
        plot.title = element_text(hjust = 0.5, family = "Avenir Black"),
        plot.subtitle = element_text(hjust = 0.5, family = "Avenir"),
        )
      
            #library(ggpubr)
            #multiplot(left_plot, big_plot, right_plot, cols=2)
            #ggarrange(left_plot, big_plot, right_plot, ncol=3)
  
    # empty plot with all annotation and main visualisation title
    plot_title <- paste("Where are the Azure Hawkers over the years?")
    plot_subtitle <- paste("before, during, and after the CEH study")
    specie_info <- paste("Class: Dragonfly \nScientific name: Aeshna caerulea \nCommon name: Azure Hawker \nHabitat: boggy pools in moorlands,\ntree trunks and rocks in \ncool conditions")
    footer <- paste("Sources: \nData: CEH, NBN Atlas \nImage: Wikimedia Commons")
      
    annotation <- data.frame(
      x = c(2.7, 0),
      y = c(9.0, 0.3),
      label = c(specie_info, footer)
    )
    
    background <- ggplot()+
      xlim(0, 10) + ylim(0,10) +
      geom_blank()+theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            text = element_text( family = "Avenir" ),
            plot.title = element_text(
                hjust = 0.6, family = "Avenir Black",
                size = 20
              ),
            plot.subtitle = element_text(hjust = 0.6, size = 20)
            ) + # remove boundary box
      ggtitle(plot_title, subtitle = plot_subtitle) +
      geom_text( data=annotation, aes( x=x, y=y, label=label), lineheight=1,
              color=text_color, family = "Avenir",
              size=2.5, hjust = 0, vjust = 1 ) 
      
    #background
    
    # area(t, l, b = t, r = l) # works by creating grid boxes and placing the plots on those grids
    layout <- c(
      #area(1,1,9,12),
      area(1, 1, 100, 100),# background
      area(5, 5, 25, 25), # top left image
      area(10, 40, 100, 75), # big middle
      area(25, 1, 85, 40), # left middle
      area(60, 80, 90, 100), # bottom image
      area(20, 80, 70, 100) # right plot
    ) 
    #plot(layout)
    background + image_ggplot_2 + big_plot + left_plot + image_ggplot_1 + 
      right_plot + 
    plot_layout(design = layout)
    
   
    