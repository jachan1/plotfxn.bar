## Synopsis

Function provides a barplot with bridges to show the p value. ggplot is used and the return object can be further manipulated as a ggplot object. The 'identity' stat is used in geom_bar so calculation of bar heights should be done prior to making the graph.

## Code Example

sample_graphs.R gives some working examples.

## Motivation

Expedited commonly used reporting object

## Installation

sample_graphs.R provides instructions to source the function from github, or just download the file

## parameters
x_grp - factor that gives the groups on the x axis       
leg_grp - factor that gives the groups for the legend       
value - gives the height of each bar      
plab - label for the top of the bridge      
blab - label for the bar       
        
## Notes
* brks control the breaks on the y axis. these may or may not be good
* cols gives the colours for the bars. The order of the factor leg_grp will be the same as the order for the col
* min and max give the limits for the y axis
* ylab, xlab, title, and legtitle are titles for the graph
* addblab controls whether or not the bars are labeled (this defaults to true)
* blabcol will set the color of the bar label (default to black)
