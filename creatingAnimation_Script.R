############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
############################################### Slide 11	
############################################### Slide 12	
############################################### Slide 13	
############################################### Slide 14	
############################################### Slide 15	
############################################### Slide 16	
############################################### Slide 17	
# define a list of packages	
reqpkg <- c("ggplot2", "gganimate", "tweenr", "animation", "tidyverse")	
	
# install and load each package defined above	
sapply(reqpkg, function(pkgi) {	
  if (!pkgi %in% installed.packages()) {	
    install.packages(pkgi, repos = "http://cran.us.r-project.org")	
  }	
  library(pkgi, character.only = TRUE)	
})	
############################################### Slide 18	
#read in pre-processed data file	
df <- readRDS(paste0(getwd(), "/ePIRLSProcessedDataFrameSelectedCountries.rds"))	
#inspect `df`	
df	
############################################### Slide 19	
# Define other plot aesthetics	
cols <- c("#982F3A", "#B3B3B3", "#3D3629", "#BA8752", "#143875")	
	
plotCaption <- ("SOURCE: International Association for the Evaluation of Educational Achievement (IEA), \nProgress in International Reading Literacy Study (PIRLS), 2016.")	
	
plotTitle <- c("Percentage of fourth-grade students reaching the ePIRLS \ninternational benchmarks in online informational reading, \nby education system: 2016")	
############################################### Slide 20	
# Create a static plot (base plot)	
basePlot <- ggplot(data = df, mapping = aes(x = Country, y = Percent, fill = Level)) +	
  geom_col(width = 0.7)  +	
  geom_text(aes(label = round(Percent)), position = position_stack(vjust = 0.5), color = "white") +	
  scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) +	
  coord_flip() +	
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),	
                     labels = c(0, 20, 40, 60, 80, 100),	
                     expand = c(0, 0, 0, 0)) +	
  labs(x = "", y = "Percent", title = plotTitle, caption = plotCaption) +	
  theme_minimal() 	
basePlot	
############################################### Slide 21	
############################################### Slide 22	
# Create a gganimate object	
anim <- basePlot +	
  # set `states = Country` to reveal one stacked bar (for each jurisdiciton) at a time	
  {{transition_states(states = Country)}}	
	
# View animation	
anim	
############################################### Slide 23	
# Create a gganimate object	
anim <- anim +	
  # keep earlier frames while revealing new frames	
  {{shadow_trail()}}	
	
# View animation	
anim	
############################################### Slide 24	
# Create a gganimate object	
anim <- anim +	
  # specify the rate of change of values between frames (default to be `linear`)	
  {{ease_aes('cubic-in-out')}}	
# View animation	
anim	
############################################### Slide 25	
# use `animate` to achieve end pause	
anim_endpause <-	
  {{animate(anim, nframes = 100, fps = 20, end_pause = 20, rewind = FALSE)}}	
	
# View animation	
anim_endpause	
############################################### Slide 26	
# define output path	
outputPath <- "Your output path"	
	
# export the animation	
anim_save(filename = "epirls_gganimate.gif", animation = anim_endpause, path = outputPath)	
############################################### Slide 27	
basePlot +	
  {{transition_states(states = ___)}}	
############################################### Slide 28	
experiment <- basePlot +	
  # try `states = Percent`	
  transition_states(states = Percent)	
	
experiment	
############################################### Slide 29	
# Create 6 data frames that stores the bar chart in 6 "stagies"	
df1 <- df %>% mutate(Percent = 0, df_id = "1")	
df2 <- df1 %>% mutate(Percent = ifelse(Level %in% c("Advanced"), df$Percent, df1$Percent), df_id = "2")	
df3 <- df2 %>% mutate(Percent = ifelse(Level %in% c("High"), df$Percent, df2$Percent), df_id = "3")	
df4 <- df3 %>% mutate(Percent = ifelse(Level %in% c("Intermediate"), df$Percent, df3$Percent), df_id = "4")	
df5 <- df4 %>% mutate(Percent = ifelse(Level %in% c("Low"), df$Percent, df4$Percent), df_id = "5")	
df6 <- df4 %>% mutate(Percent = ifelse(Level %in% c("Below Low"), df$Percent, df5$Percent), df_id ="6" )	
	
# Combine all 6 data frames into 1 list	
ls <- list(df1, df2, df3, df4, df5, df6)	
	
# Inspect `ls`	
ls	
############################################### Slide 30	
# Use the `tween_states` function from the `tweenr` package to interpolate data in between each state/stage	
tf <- tween_states(ls, tweenlength= 1, statelength=0, ease='cubic-in-out', nframes=100)	
	
# Inspect `tf`	
tf	
############################################### Slide 31	
# define where we want the animation to "pause"	
pause_frames <- c(21, 41, 61, 81, 100)	
	
# define output path	
outputPath <- "Change to your output path"	
	
# use `saveGIF` from `animation` to create and save GIF	
saveGIF({	
  for (i in 1:max(tf$.frame)) {	
    # print out which frame it is working on	
    print(paste0("working on the ", i, "th frame"))	
	
    # create plot in each frame, saved as `g`	
    g <- ggplot(data = subset(tf, .frame == i), mapping = aes(x = Country, y = Percent, fill = Level, .frame = i)) +	
      geom_col(width = 0.7)  +	
      geom_text(data = subset(tf, .frame == max(tf$.frame)), aes(label = round(Percent)), position = position_stack(vjust = 0.5), color = "white") +	
      scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) +	
      coord_flip() +	
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),	
                         labels = c(0, 20, 40, 60, 80, 100),	
                         expand = c(0, 0, 0, 0),	
                         limits = c(0,100)) +	
      labs(x = "", y = "Percent", title = plotTitle, caption = plotCaption) +	
      theme_minimal()	
	
    # draw `g` once for each frame	
    grid::grid.draw(g)	
	
    # if i is a pause frame, draw a few more	
    if (i %in% pause_frames){	
      replicate(15,grid::grid.draw(g))}	
	
    # if i is the last frame, draw even more	
    if (i == max(tf$.frame)){	
      replicate(100,grid::grid.draw(g))	
    }	
  }	
},	
# specify the pathway and name of the gif output, as well as the interval, width, and height	
movie.name = paste0(outputPath,"/epirls_tweenr.gif"), interval = .02, ani.width = 600, ani.height = 400)	
############################################### Slide 32	
############################################### Slide 33	
############################################### Slide 34	
############################################### Slide 35	
	
# Define a function to compress any GIF file	
# sourced from https://stla.github.io/stlapblog/posts/AnimatedGifs.html	
# Need to install the command-line tool `gifsicle` first	
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){	
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)	
  system.fun <- if (.Platform$OS.type == "windows") shell else system	
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))	
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))	
}	
	
gif_compress(ingif = "Path/to/the/input/gif/file/timss_tweenr.gif",	
             outgif = "Path/to/the/output/gif/file/timss_tweenr_compressed.gif", 	
             extra.opts = "--colors 256")	
