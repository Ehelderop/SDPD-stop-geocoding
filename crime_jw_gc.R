library(foreign)
library(stringr)
library(stringdist)

df = read.csv('path_to_stop_csv') #point to SDPD stop csv

#stops are split between those that occur at an address and 
#those that occur at an intersection of two roads
#use this to identify intersections. Depending on data format may 
#need to check for 'AT' '@' '&' '/' etc.
df$int = ifelse(grepl('&',df$address, fixed=T),1,0)

#split into address df and intersection df
dfint = df[df$int==1,]
dfadd = df[df$int==0,]

###address GC

#master SD address file
adds = read.dbf('path_to_master_SD_address_dbf')

#clean fields you care about
adds = adds[complete.cases(adds$ADDRNAME),]
adds = adds[complete.cases(adds$ADDRNMBR),]
adds = adds[adds$ADDRNMBR != 0,]
adds = adds[complete.cases(adds$COMMUNITY),]

adds$COMMUNITY = toupper(adds$COMMUNITY)

adds$GEOID = as.character(adds$GEOID)

#turn e.g. 01st into 1st
adds$ADDRNAME = as.character(adds$ADDRNAME)
adds$ADDRNAME = ifelse(substr(adds$ADDRNAME,1,1)=='0',substr(adds$ADDRNAME,2,nchar(adds$ADDRNAME)),adds$ADDRNAME)

adds$comp_add = paste(adds$ADDRNMBR, adds$ADDRNAME, sep=' ')

adds$nameSfx = paste(adds$ADDRNAME, adds$ADDRSFX, sep=' ')

#unique suffixes
usuff = unique(adds[complete.cases(adds$ADDRSFX),]$ADDRSFX)

##using the $addrst col in df for street names and $addrblk for number
#next combine suffix and street name in master file

adds$nameSfx = ifelse(is.na(adds$ADDRSFX), adds$ADDRNAME, paste(adds$ADDRNAME,adds$ADDRSFX,sep=' '))

###now actually run the geocoder
dfadd$conf = 0
dfadd$low_conf = 0
dfadd$matchadd = 0
dfadd$matchnum = 0
dfadd$lat = 0
dfadd$lon = 0

for (i in 1:nrow(dfadd)) {
  tname = dfadd$addrst[i]
  tnum = dfadd$addrblk[i]
  
  if(is.na(tnum)) {
    next
  }
  
  if (substr(tname,1,1)=='0') {
    tname = substr(tname,2,nchar(tname))
  }
  
  #identifies the closest matching street name using Jaro-Winkler distance
  tdf = adds[which(stringdist(tname, adds$nameSfx, 'jw') == min(stringdist(tname, adds$nameSfx, 'jw'))),]
  
  ttdf = tdf[which.min(abs(tdf$ADDRNMBR-as.numeric(tnum))),]
  
  dfadd$lat[i] = ttdf$LAT[1]
  dfadd$lon[i] = ttdf$LON[1]
  
  dfadd$conf[i] = stringdist(tname, ttdf$nameSfx[1], 'jw')
  
  dfadd$matchadd[i] = ttdf$nameSfx[1]
  dfadd$matchnum[i] = ttdf$ADDRNMBR[1]
}

#low confidence threshold
dfadd$low_conf = ifelse(dfadd$conf > 0.2,1,0)

#write results
write.csv(dfadd, 'address_results.csv') #manually check the low confidence results

### intersection GC

#master intersection file
inter = read.csv('D:\\Work\\CrimeGeocoding\\intersection_lookup.csv')

#need to duplicate it so that e.g. both of the following intersections
#result in a correct GC: '3rd and 5th' and '5th and 3rd'
dupInter = inter
dupInter$tempRD1 = dupInter$RD1_clean
dupInter$tempRD2 = dupInter$RD2_clean
dupInter$RD1_clean = dupInter$tempRD2
dupInter$RD2_clean = dupInter$tempRD1
dupInter$tempRD1 = NULL
dupInter$tempRD2 = NULL

inter = rbind(inter,dupInter)

# $intersn is one street, $addrst is another

dfint$conf1 = 0
dfint$conf2 = 0
dfint$low_conf = 0
dfint$matchadd = 0
dfint$matchnum = 0
dfint$lat = 0
dfint$lon = 0
dfint$matchRD1 = '0'
dfint$matchRD2 = '0'

for (i in 1:nrow(dfint)) {
  tdf = NULL
  
  tRD1 = dfint$intersn[i]
  tRD2 = dfint$addrst[i]
  
  if (substr(tRD1,1,1)=='0') {
    tRD1 = substr(tRD1,2,nchar(tRD1))
  }
  if (substr(tRD2,1,1)=='0') {
    tRD2 = substr(tRD2,2,nchar(tRD2))
  }
  
  #this block identifies the closest matching intersection that actually exists
  tdnorm = min(stringdist(tRD1, inter$RD1_clean, 'jw')) + min(stringdist(tRD2, inter$RD2_clean, 'jw'))
  tdrerv = min(stringdist(tRD1, inter$RD2_clean, 'jw')) + min(stringdist(tRD2, inter$RD1_clean, 'jw'))
  
  if (tdnorm <= tdrerv) {
    tdf = inter[which.min(stringdist(tRD1, inter$RD1_clean, 'jw') + stringdist(tRD2, inter$RD2_clean, 'jw')),]
    dfint$conf1[i] = stringdist(tRD1, tdf$RD1_clean[1], 'jw')
    dfint$conf2[i] = stringdist(tRD2, tdf$RD2_clean[1], 'jw')
    if (tdnorm > 0.38) { #threshold for low_conf
      dfint$low_conf[i] = 1
    }
  }
  if (tdnorm > tdrerv) {
    tdf = inter[which.min(stringdist(tRD1, inter$RD2_clean, 'jw') + stringdist(tRD2, inter$RD1_clean, 'jw')),]
    dfint$conf1[i] = stringdist(tRD1, tdf$RD2_clean[1], 'jw')
    dfint$conf2[i] = stringdist(tRD2, tdf$RD1_clean[1], 'jw')
    if (tdrerv > 0.38) { #threshold for low_conf
      dfint$low_conf[i] =  1
    }
  }
  
  dfint$lat[i] = tdf$LAT[1]
  dfint$lon[i] = tdf$LON[1]
  dfint$matchRD1[i] = tdf$RD1_clean[1]
  dfint$matchRD2[i] = tdf$RD2_clean[1]
}

write.csv(dfint, 'intersection_results.csv')



