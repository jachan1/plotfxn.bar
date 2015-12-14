## function
plotfxn.bar <- function(ds, brks, cols,  min=0, max=100, title="", ylab="", xlab="", legtitle="", addblab=T, blabcol="black", base_font=12, fliplegend=F){
    ## cols are added from IRCDA template colors
    rgb255 <- function(r,g,b){
        rgb(r/255, g/255, b/255)
    }
    if(missing(cols)){
        cols <- c(rgb255(166, 206, 227), rgb255(31, 120, 180), rgb255(178, 223, 138), rgb255(51, 160, 44), rgb255(251, 154, 153), rgb255(227, 26, 28), rgb255(253, 191, 111), rgb255(255, 127, 0), rgb255(202, 178, 214), rgb255(106, 61, 154))
    }
    ## need min and max
    grpm_fxn <- function(x) {
        x %>% mutate(semax=max(value) + 0.05*(max-min), 
                     pmax=max(value) + 0.055*(max-min), 
                     bstart=value + 0.001*(max-min),
                     plaby=value-0.025*(max-min))
    }
    pds <- ds %>% ungroup() %>% group_by(x_grp) %>% do(grpm_fxn(.))
    pds$plab[duplicated(pds$x_grp)] <- NA
    pds$x_grpn <- as.numeric(pds$x_grp)
    tsize = 6
    dodgn <- 0.7
    dodge <- position_dodge(width=-dodgn)
    
    if(missing(brks)){
        brks <- floor(seq(min, max, length.out = 5))
    }
    
    cvals = levels(pds$leg_grp)
    if(fliplegend){
        cvals = cvals[length(cvals):1]
    }
    
    g <- ggplot(pds, aes(x_grp, value, fill=leg_grp)) + geom_bar(stat="identity", position=dodge, width=0.6) +
        # geom_errorbar(aes(ymax=upper, ymin=lower), position=dodge, width=0.25) + 
        geom_linerange(aes(x_grpn, ymin=bstart, ymax=semax), position=dodge, size=0.25) +
        geom_segment(aes(xend=x_grpn, x=x_grpn, y=semax, yend=semax), position=dodge, size=0.25) +
        geom_text(aes(label=plab, x=x_grpn, y=pmax), colour="black", vjust=-1/2, size=tsize) +
        scale_y_continuous(limits=c(min, max*1.1), breaks=brks) + theme_bw() +
        theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(),
              panel.border = element_blank(), axis.line = element_line(colour = "grey"), axis.line.x = element_blank(),
              axis.ticks.x=element_blank(), axis.title.x=element_text(vjust=-0.15, lineheight=100),
              axis.text=element_text(size=base_font), axis.title=element_text(size=base_font+2, face="bold"),
              legend.text=element_text(size=base_font), legend.title=element_text(size=base_font, face="bold"),
              plot.title=element_text(size=base_font+4, face="bold")) +
        scale_fill_manual(name=legtitle, values=cols[1:length(cvals)], breaks=cvals) +
        ggtitle(title) +
        ylab(ylab) + 
        xlab(xlab)
    if(addblab) {
        g <- g + geom_text(aes(label=blab, x=x_grpn, y=plaby), position=dodge, colour=blabcol, size=tsize)
    }
    g
}