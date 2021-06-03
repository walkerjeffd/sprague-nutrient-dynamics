#!/bin/bash

TODAY=$(date +%Y-%m-%d)

STATION=11495800
START=1993-05-01
END=2012-10-10
URL="http://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no="$STATION"&referred_module=sw&period=&begin_date="$START"&end_date="$END""
FILE="$STATION"_sprague_nf_"$START"_"$END".txt
if [ ! -f $FILE ]; then
  wget -O $FILE $URL  
fi

STATION=11497500
START=1953-10-01
END=1991-09-30
URL="http://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no="$STATION"&referred_module=sw&period=&begin_date="$START"&end_date="$END""
FILE="$STATION"_sprague_beatty_"$START"_"$END".txt
if [ ! -f $FILE ]; then
  wget -O $FILE $URL  
fi

STATION=11501000
START=1921-03-01
END=$TODAY
URL="http://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no="$STATION"&referred_module=sw&period=&begin_date="$START"&end_date="$END""
FILE="$STATION"_sprague_chiloquin_"$START"_"$END".txt
if [ ! -f $FILE ]; then
  wget -O $FILE $URL
fi

STATION=11502500
START=1917-10-01
END=$TODAY
URL="http://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no="$STATION"&referred_module=sw&period=&begin_date="$START"&end_date="$END""
FILE="$STATION"_williamson_chiloquin_"$START"_"$END".txt
if [ ! -f $FILE ]; then
  wget -O $FILE $URL
fi
