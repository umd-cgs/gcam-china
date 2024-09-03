# The GCAM R header file
# Ben Bond-Lamberty August 2011, updated February 2012

# This file should be source'd by any R script involved in the processing of GCAM input data
# It provides various facilities, including logging, file I/O and definition of common settings

# -----------------------------------------------------------------------------
# Load required libraries
libs <- c( "reshape2", "stringr", "tidyr","assertr","dplyr" )
for( i in libs ) {
	if( !require( i, character.only=T ) ) {
		cat( "Couldn't load", i, "; trying to download it...\n" )
		install.packages( i )
	}
	library( i, character.only=T )
}


# -----------------------------------------------------------------------------
#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )


# -----------------------------------------------------------------------------
# repeat_and_add_vector: function for repeating a dataframe in order to add a new vector
repeat_and_add_vector <- function( data, vector, vector_values ) {
     data_new <- data[ rep( 1:nrow( data ), times = length( vector_values ) ), ]
     data_new[[vector]] <- sort( rep( vector_values, length.out = nrow( data_new ) ) )
     return( data_new )
	 }
# -----------------------------------------------------------------------------
#vecpaste: this is a function for pasting together any number of variables to be used as unique identifiers in a lookup
vecpaste <- function (x) {
     y <- x[[1]]
     if (length(x) > 1) {
         for (i in 2:length(x)) {
             y <- paste(y, x[[i]] )
         }
     }
     y
 }


# check whether a data frame contains NaN.
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


# add a row of US total
add_USA <- function(x) {
  x1 <- x
  x2 <- x %>%
    summarise_if(is.numeric, sum, na.rm = T) %>%
    mutate(region = "USA")
  result <- rbind(x1, x2)
  
  return(result)
}

is_outlier <- function(x) {
  return((x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x)))
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# repeat_and_add_vector: function for repeating a dataframe in order to add a new vector
repeat_and_add_vector <- function( data, vector, vector_values ) {
  data_new <- data[ rep( 1:nrow( data ), times = length( vector_values ) ), ]
  data_new[[vector]] <- sort( rep( vector_values, length.out = nrow( data_new ) ) )
  return( data_new )
}


bar_plots <- function(fig_data, fill_var = "Fuel", color_pal, title, y_unit = "EJ", fig_path, plot_name,
                      xyears = c(2005, 2105), xbreaks = c(seq(2010, 2100, by = 10))) {
  
  for (r in unique(fig_data$region)){
    p <- ggplot() +
      geom_col(data=filter(fig_data, region == r),
               aes(x = year, y = value, fill = Fuel)) +
      scale_fill_manual(name = fill_var, values=color_pal) +
      facet_grid(case ~ model) +
      scale_x_continuous(limits = xyears, breaks=xbreaks) +
      # scale_x_continuous(breaks = c(seq(2010, 2100, 10))) +
      # scale_x_continuous(limits = c(2000, 2020), breaks=c(seq(2005, 2015, by = 5))) +
      # expand_limits(y = 0) +
      ggtitle( paste0(r, " - ", title)) +
      xlab("Year") +
      ylab(y_unit) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line( size=.1, color="gray"),
            panel.grid.major.y = element_line( size=.1, color="gray"),
            plot.title = element_text(face="bold", size=18, hjust = 0.5),
            plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),
            axis.title.x = element_text(margin = margin(t = 12), size = 16),
            axis.title.y = element_text(margin = margin(r = 12), size = 16),
            axis.text.x  = element_text(size=14, hjust = 1, vjust = 1, angle = 45),
            axis.text.y  = element_text(size=14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 11),
            legend.key.height = unit(2, "line"),
            legend.key.width = unit(3, "line"),
            strip.text = element_text(size = 16, face = "bold", margin = margin(0,0.3,0,0.3, "cm")))
    ggsave(paste0(fig_path, r, plot_name, ".png"), dpi=300/2, width=3300/300, height=1860/300)
  }
  
}


line_plots <- function(fig_data, col_var = "Model", color_pal, title, y_unit = "EJ", fig_path, plot_name,
                       xyears = c(2005, 2105), xbreaks = c(seq(2010, 2100, by = 10)), facet_sector = F) {
  
  for (r in unique(fig_data$region)){
    if(facet_sector == T) {
      p <- ggplot() +
        geom_line(data = filter(fig_data, region == r),
                  aes(x = year, y = value, color = agg_sector, linetype = model), size = 1.5) +
        scale_color_manual(name = "Sector", values = sector_palette) +
        scale_linetype_manual(name = col_var, values = model_lines) +
        scale_x_continuous(limits = xyears, breaks=xbreaks) +
        # scale_x_continuous(breaks = c(seq(2010, 2100, 10))) +
        expand_limits(y = 0) +
        facet_wrap(. ~ case, ncol = 2) +
        ggtitle( paste0(r, " - ", title )) +
        xlab("Year") +
        ylab(y_unit) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=18, hjust = 0.5),
              plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),
              axis.title.x = element_text(margin = margin(t = 12), size = 16),
              axis.title.y = element_text(margin = margin(r = 12), size = 16),
              axis.text.x  = element_text(size=14, hjust = 1, vjust = 1, angle = 45),
              axis.text.y  = element_text(size=14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 11),
              legend.key.height = unit(2, "line"),
              legend.key.width = unit(3, "line"),
              strip.text = element_text(size = 10, face = "bold", margin = margin(0,0.3,0,0.3, "cm")))
      ggsave(paste0(fig_path, r, plot_name, ".png"), dpi=300/2, width=3300/300, height=1860/300)
    } else {
      p <- ggplot() +
        geom_line(data=filter(fig_data, region == r),
                  aes(x = year, y = value, color = model), size = 1.5) +
        scale_color_manual(name = "Model", values = color_pal) +
        scale_x_continuous(breaks=c(seq(2010, 2100, 10))) +
        expand_limits(y = 0) +
        facet_wrap(. ~ case, ncol = 2) +
        ggtitle( paste0(r, " - ", title )) +
        xlab("Year") +
        ylab(y_unit) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=18, hjust = 0.5),
              plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),
              axis.title.x = element_text(margin = margin(t = 12), size = 16),
              axis.title.y = element_text(margin = margin(r = 12), size = 16),
              axis.text.x  = element_text(size=14, hjust = 1, vjust = 1, angle = 45),
              axis.text.y  = element_text(size=14),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 11),
              legend.key.height = unit(2, "line"),
              legend.key.width = unit(3, "line"),
              strip.text = element_text(size = 10, face = "bold", margin = margin(0,0.3,0,0.3, "cm")))
      ggsave(paste0(fig_path, r, plot_name, ".png"), dpi=300/2, width=3300/300, height=1860/300)
    }
  }
}


