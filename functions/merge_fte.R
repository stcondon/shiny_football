library(stringdist)
fte <- fread("data/five_thirty_eight/spi_matches.csv")
distance <- stringdistmatrix(espn,xpert,method = 'lcs')
match <- apply(distance, 1, which.min)
cbind(espn,xpert[match])

## ATTACH THE BOUND VECTORS, MERGE ON THE XPERT
## AND THEN MERGE AGAIN ON DATE AND HOMETEAM