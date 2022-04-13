# first run this line of code:
source("https://raw.githubusercontent.com/Ratey-AtUWA/spatial/main/afr_map_function.R", echo = FALSE)

# then run this line to draw the map
# try the function with no options first (empty parentheses)

afrMap()

# the possible options are:
#   afrMap(proj = ..., xlim = ..., ylim = ..., maplabels = ...)
#   proj can be either "utm" (the default) or "longlat"
#   xlim is a vector of upper and lower x-axis (E-W or horizontal) limits
#   ylim is a vector of upper and lower y-axis (S-N or vertical) limits
#   maplabels can either be TRUE (default) or FALSE and 
#     controls if selected map features are labelled
#   For example, try:
#   afrMap(proj = "longlat", maplabels = T)
