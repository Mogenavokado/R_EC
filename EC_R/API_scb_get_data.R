install.packages('pxweb')
library(pxweb)

# Tutorial included
vignette(topic="pxweb")

# Recommend setting the UTF-8 encoding since each API may have local specific letters.
Sys.setlocale(locale = "UTF-8")

# Get data from swedish SCB
my_statistics_dataset <- interactive_pxweb("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel")



