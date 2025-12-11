#Base NFL Field
library(ggplot2)
library(extrafont)

# font_import()

baseNFLField <- function(){
  ytop = 53.3
  ybot = 0
  field.lines <- data.frame(x1 = c(0,0,0,120,110,10),
                            x2 = c(120,120,0,120,110,10),
                            y1 = c(ybot,ytop,ytop,ytop,ytop,ytop),
                            y2 = c(ybot,ytop,ybot,ybot,ybot,ybot))

  ids = factor(c('endDef','fieldPlay','endOff'))

  values <- data.frame(
    id = ids,
    color = c('darkgreen','darkgreen','darkgreen')
  )

  field.shape <- data.frame(id = unlist(matrix(t(matrix(rep(ids,4),nrow=3)),nrow=1)[1,]),
                            xg = c(0,0,10,10,10,10,110,110,110,110,120,120),
                            yg = c(ybot,ytop,ytop,ybot,ybot,ytop,ytop,ybot,ybot,ytop,ytop,ybot))
  datapoly <- merge(values, field.shape, by = c("id"))

  fieldLines <- geom_segment(data=field.lines,aes(x=x1,y=y1,xend=x2,yend=y2,
                                                  linetype='a'),colour='white',
                             size = c(2,2,2,2,1,1))

  minor.lines.df <- data.frame(xvals = seq(15,105,length.out = 19),
                               ybot = rep(ybot,19),
                               ytop = rep(ytop,19))

  minor.field.lines <- geom_segment(data = minor.lines.df,aes(x = xvals,y = ybot,xend = xvals,yend = ytop),colour = 'white')

  hash.marks.df <- data.frame(xvals = rep(11:109,4),
                              ybot = c(rep(ybot + 1/3,99),rep(22.91,99),rep(29.73,99),rep(ytop - 1,99)),
                              ytop = c(rep(ybot + 1 ,99),rep(23.57,99),rep(30.39,99),rep(ytop - 1/3,99)))

  hash.marks <- geom_segment(data = hash.marks.df,aes(x = xvals,y = ybot,xend = xvals, yend = ytop),colour = 'white')

  field.numbers.bot.df <- data.frame(textVals = unlist(lapply(c(1,0,2,0,3,0,4,0,5,0,4,0,3,0,2,0,1,0),toString)),
                                     #x_text = c(8,12,18,22,28,32,38,42,48,52,58,62,68,72,78,82,88,92),
                                     x_text = c(8.5,11.5,18.5,21.5,28.5,31.5,38.5,41.5,48.5,51.5,58.5,61.5,68.5,71.5,78.5,81.5,88.5,91.5)+10,
                                     y_text = rep(ybot + 9,18))

  show.bot.numbers <- geom_text(data=field.numbers.bot.df, aes(x = x_text, y = y_text, label = textVals),
                                size = 6,col='white',family = ifelse('Century' %in% fonts(),windowsFont('Century'),'Impact'))

  field.numbers.top.df <- data.frame(textVals = unlist(lapply(c(0,1,0,2,0,3,0,4,0,5,0,4,0,3,0,2,0,1),toString)),
                                     #x_text = c(8,12,18,22,28,32,38,42,48,52,58,62,68,72,78,82,88,92),
                                     x_text = c(8.5,11.5,18.5,21.5,28.5,31.5,38.5,41.5,48.5,51.5,58.5,61.5,68.5,71.5,78.5,81.5,88.5,91.5)+10,
                                     y_text = rep(ytop - 9,18))

  show.top.numbers <- geom_text(data=field.numbers.top.df, aes(x = x_text, y = y_text, label = textVals),
                                size = 6,col='white',family = ifelse('Century' %in% fonts(),windowsFont('Century'),'Impact'),angle = 180)

  left.endzone.df <- data.frame(textVals = 'T E A M', x_left = 5, y_left = ytop/2)

  show.left.endzone <- geom_text(data = left.endzone.df, aes(x = x_left, y = y_left,label = textVals),
                                 size = 10, col = 'white', family = ifelse('Copperplate Gothic Bold' %in% fonts(),windowsFont('Copperplate Gothic Bold'),'Georgia'),angle = 90)

  right.endzone.df <- data.frame(textVals = 'T E A M', x_right = 115, y_right = ytop/2)

  show.right.endzone <- geom_text(data = right.endzone.df, aes(x = x_right, y = y_right,label = textVals),
                                  size = 10, col = 'white', family = ifelse('Copperplate Gothic Bold' %in% fonts(),windowsFont('Copperplate Gothic Bold'),'Georgia'),angle = -90)

  big_box <- data.frame(x_big_box_s = c(-1,-1,-1,121),
                        x_big_box_e = c(121,121,-1,121),
                        y_big_box_s = c(ytop+.25,ybot-.2,ybot-.2,ybot-.2),
                        y_big_box_e = c(ytop+.25,ybot-.2,ytop+.25,ytop+.25))

  baseNFL <- ggplot(datapoly, aes(x = xg, y = yg)) +
    geom_polygon(aes(fill = color, group = id)) +
    scale_fill_identity() +
    labs(x='',y='') +
    minor.field.lines +
    hash.marks +
    show.bot.numbers +  show.top.numbers +
    show.left.endzone + show.right.endzone +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.position = 'none') +
    fieldLines +
    geom_segment(data=big_box,aes(x=x_big_box_s,y=y_big_box_s,xend=x_big_box_e,yend=y_big_box_e),colour='black')

  return(baseNFL)
}

noColor_baseNFLField <- function(){
  ytop = 53.3
  ybot = 0
  field.lines <- data.frame(x1 = c(0,0,0,120,110,10),
                            x2 = c(120,120,0,120,110,10),
                            y1 = c(ybot,ytop,ytop,ytop,ytop,ytop),
                            y2 = c(ybot,ytop,ybot,ybot,ybot,ybot))

  ids = factor(c('endDef','fieldPlay','endOff'))

  values <- data.frame(
    id = ids,
    color = c('white','white','white')
  )
  field.shape <- data.frame(id = unlist(matrix(t(matrix(rep(ids,4),nrow=3)),nrow=1)[1,]),
                            xg = c(0,0,10,10,10,10,110,110,110,110,120,120),
                            yg = c(ybot,ytop,ytop,ybot,ybot,ytop,ytop,ybot,ybot,ytop,ytop,ybot))
  datapoly <- merge(values, field.shape, by = c("id"))

  fieldLines <- geom_segment(data=field.lines,aes(x=x1,y=y1,xend=x2,yend=y2,
                                                  linetype='a'),colour='white',
                             size = c(2,2,2,2,1,1))

  minor.lines.df <- data.frame(xvals = seq(15,105,length.out = 19),
                               ybot = rep(ybot,19),
                               ytop = rep(ytop,19))

  minor.field.lines <- geom_segment(data = minor.lines.df,aes(x = xvals,y = ybot,xend = xvals,yend = ytop),colour = 'white')

  hash.marks.df <- data.frame(xvals = rep(11:109,4),
                              ybot = c(rep(ybot + 1/3,99),rep(22.91,99),rep(29.73,99),rep(ytop - 1,99)),
                              ytop = c(rep(ybot + 1 ,99),rep(23.57,99),rep(30.39,99),rep(ytop - 1/3,99)))

  hash.marks <- geom_segment(data = hash.marks.df,aes(x = xvals,y = ybot,xend = xvals, yend = ytop),colour = 'white')

  field.numbers.bot.df <- data.frame(textVals = unlist(lapply(c(1,0,2,0,3,0,4,0,5,0,4,0,3,0,2,0,1,0),toString)),
                                     #x_text = c(8,12,18,22,28,32,38,42,48,52,58,62,68,72,78,82,88,92),
                                     x_text = c(8.5,11.5,18.5,21.5,28.5,31.5,38.5,41.5,48.5,51.5,58.5,61.5,68.5,71.5,78.5,81.5,88.5,91.5)+10,
                                     y_text = rep(ybot + 9,18))

  show.bot.numbers <- geom_text(data=field.numbers.bot.df, aes(x = x_text, y = y_text, label = textVals),
                                size = 6,col='white',family = ifelse('Century' %in% fonts(),windowsFont('Century'),'Impact'))

  field.numbers.top.df <- data.frame(textVals = unlist(lapply(c(0,1,0,2,0,3,0,4,0,5,0,4,0,3,0,2,0,1),toString)),
                                     #x_text = c(8,12,18,22,28,32,38,42,48,52,58,62,68,72,78,82,88,92),
                                     x_text = c(8.5,11.5,18.5,21.5,28.5,31.5,38.5,41.5,48.5,51.5,58.5,61.5,68.5,71.5,78.5,81.5,88.5,91.5)+10,
                                     y_text = rep(ytop - 9,18))

  show.top.numbers <- geom_text(data=field.numbers.top.df, aes(x = x_text, y = y_text, label = textVals),
                                size = 6,col='white',family = ifelse('Century' %in% fonts(),windowsFont('Century'),'Impact'),angle = 180)

  left.endzone.df <- data.frame(textVals = 'T E A M', x_left = 5, y_left = ytop/2)

  show.left.endzone <- geom_text(data = left.endzone.df, aes(x = x_left, y = y_left,label = textVals),
                                 size = 10, col = 'white', family = ifelse('Copperplate Gothic Bold' %in% fonts(),windowsFont('Copperplate Gothic Bold'),'Georgia'),angle = 90)

  right.endzone.df <- data.frame(textVals = 'T E A M', x_right = 115, y_right = ytop/2)

  show.right.endzone <- geom_text(data = right.endzone.df, aes(x = x_right, y = y_right,label = textVals),
                                  size = 10, col = 'white', family = ifelse('Copperplate Gothic Bold' %in% fonts(),windowsFont('Copperplate Gothic Bold'),'Georgia'),angle = -90)

  big_box <- data.frame(x_big_box_s = c(-1,-1,-1,121),
                        x_big_box_e = c(121,121,-1,121),
                        y_big_box_s = c(ytop+.25,ybot-.2,ybot-.2,ybot-.2),
                        y_big_box_e = c(ytop+.25,ybot-.2,ytop+.25,ytop+.25))

  baseNFL <- ggplot(datapoly, aes(x = xg, y = yg)) +
    geom_polygon(aes(group = id)) +
    scale_fill_identity() +
    labs(x='',y='') +
    minor.field.lines +
    hash.marks +
    show.bot.numbers +  show.top.numbers +
    show.left.endzone + show.right.endzone +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.position = 'none') +
    fieldLines +
    geom_segment(data=big_box,aes(x=x_big_box_s,y=y_big_box_s,xend=x_big_box_e,yend=y_big_box_e),colour='black')

  return(baseNFL)
}

