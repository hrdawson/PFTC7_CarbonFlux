# PFTC7_CarbonFlux
Repo for Group 3: Carbon Fluxes as part of PFTC7 in South Africa

# Data dictionaries
## Fluxes
### LI-7500 Ecosystem fluxes
| Variable name    | Description                                                                               | Variable type | Variable range or levels                                   | Units     | How measured |
|------------------|-------------------------------------------------------------------------------------------|---------------|------------------------------------------------------------|-----------|--------------|
| filename         | File name of original file                                                                | categorical   | 1_2000_east_1_day_photo.txt - 5_2800_west_5_night_resp.txt | NA        | defined      |
| tstart           | Time start (trimmed from raw flux)                                                        | numeric       | 20 - 20                                                    | seconds   | defined      |
| tfinish          | Time end (trimmed from raw flux)                                                          | numeric       | 80 - 80                                                    | seconds   | defined      |
| camb             | Ambient carbon, measured from ambient curve or from first five observations of flux curve | numeric       | 403.886 - 484.652                                          |           | measured     |
| tav              | Average ambient temperature, calculated from associated ambient curve                     | numeric       | 9.927 - 32.236                                             | degrees C | calculated   |
| pav              | Average ambient pressure, calculated from raw flux data                                   | numeric       | 72.971 - 80.405                                            |           | calculated   |
| nee_lm           | Net ecosystem exchange, linear model fit                                                  | numeric       | -88.934                                                    |           |              |
| nee_exp          | Net ecosystem exchange, non-linear model fit                                              | numeric       | -209.301                                                   |           |              |
| lm_rsqd          | R-squared value of the linear model fit                                                   | numeric       | 0.001 - 0.997                                              |           |              |
| non_linear_sigma | Sigma value of the non-linear model fit                                                   | numeric       | 0.206 - 15.849                                             |           |              |
| aic_lm           | AIC score of the linear model fit                                                         | numeric       | -591.69                                                    |           |              |
| aic_nlm          | AIC score of the non-linear model fit                                                     | numeric       | -512.241                                                   |           |              |
| c_prime_min      | Minimum value of CO2 flux, adjusted                                                       | numeric       | 296.297 - 482.23                                           |           |              |
| c_prime_max      | Maximum value of CO2 flux, adjusted                                                       | numeric       | 388.48 - 548.468                                           |           |              |
| flagged          | Quality flag                                                                              | categorical   | NA                                                         | NA        | defined      |
### LI-8100A Soil respiration
| Variable name | Description                                                                                   | Variable type | Variable range or levels                          | Units               | How measured |
|---------------|-----------------------------------------------------------------------------------------------|---------------|---------------------------------------------------|---------------------|--------------|
| File          | Original measurement file name                                                                | categorical   | SR_1_2000_east - SR_5_2800_west                   |                     | defined      |
| flux          | Abbreviation of soil respiration                                                              | categorical   | SR - SR                                           |                     | defined      |
| siteID        | Site number                                                                                   | categorical   | Site_1 - Site_5                                   |                     | defined      |
| elevation     | Site elevation, assigned                                                                      | numeric       | 2000 - 2800                                       |                     | defined      |
| aspect        | Transect aspect, assigned                                                                     | categorical   | east - west                                       |                     | defined      |
| iChunk        | Which round of measurements this is (used to define plot ID)                                  | numeric       | 1 - 5                                             |                     | defined      |
| Type          | Type of record. See the LI8100A manual page 6-4 Table 1 for more information. 1 = raw record. | numeric       | 1 - 1                                             |                     | defined      |
| Etime         | Elapsed time of measurement                                                                   | numeric       | 120 - 179                                         |                     | measured     |
| Date          | Date and time of measurement                                                                  | categorical   | 1970-01-01T00:05:43Z - 2023-12-14T13:45:28Z       | yyyy-mm-dd hh:mm:ss | measured     |
| Tcham         | Temperature within the chamber                                                                | numeric       | 20.07 - 28.79                                     | degrees C           | measured     |
| Pressure      | Pressure within the chamber                                                                   | numeric       | 72.73 - 79.84                                     | kPa                 | measured     |
| H2O           | Water concentration in chamber                                                                | numeric       | 9.232 - 29.685                                    | mmol/m              | measured     |
| CO2           | CO2 concentration in the chamber                                                              | numeric       | 391.46 - 543.13                                   | umol/m              | measured     |
| Cdry          | CO2 concentration in the chamber, corrected for water vapour dilution                         | numeric       | 397.93 - 551.33                                   | umol/m              | measured     |
| T1            | Soil temperature (but this is more often wildly wrong than anywhere close to correct)         | numeric       | -187.23 - 191.06                                  | degrees C           | measured     |
| T2            | Not used                                                                                      | numeric       | -187.2 - -173.05                                  |                     |              |
| T3            | Not used                                                                                      | numeric       | -187.16 - -173.03                                 |                     |              |
| T4            | Not used                                                                                      | numeric       | -187.18 - -173.03                                 |                     |              |
| V1            | Voltage at auxilary channel 1 (multiplexer flow)                                              | numeric       | 0.137 - 0.227                                     | V                   | measured     |
| V2            | Not used                                                                                      | numeric       | 0 - 0                                             |                     |              |
| V3            | Voltage at auxilary channel 3 (maybe the soil moisture probe?)                                | numeric       | 0.009 - 0.032                                     | V                   | measured     |
| V4            | Not used                                                                                      | numeric       | 0 - 0                                             |                     |              |
| RH            | Relative humidity in the chamber                                                              | numeric       | 21.29 - 69.78                                     | percent             | measured     |
| Tbench        | Temperature of the optical bench                                                              | numeric       | 51.23 - 51.33                                     | degrees C           | measured     |
| Tboard        | Temperature of the analyser control unit board                                                | numeric       | 23.84 - 34.28                                     | degrees C           | measured     |
| Vin           | Battery input voltage                                                                         | numeric       | 11.82 - 12.59                                     | V                   | measured     |
| CO2ABS        | CO2 absorption of photons in optical bench                                                    | numeric       | 0.081 - 0.103                                     |                     |              |
| H2OABS        | Water absorption of photons in the optical bench                                              | numeric       | 0.064 - 0.144                                     |                     |              |
| Hour          | Hour of the day                                                                               | numeric       | 0.089 - 16.955                                    |                     | measured     |
| DOY           | Day of year                                                                                   | numeric       | 1.004 - 348.573                                   |                     | measured     |
| RAWCO2        | CO2 raw signal                                                                                | numeric       | 2219717 - 2254389                                 |                     | measured     |
| RAWCO2REF     | A measure of the optical amplitude of the CO2 channel, primarily for diagnostic purposes      | numeric       | 2545659 - 2549632                                 |                     | measured     |
| RAWH2O        | Water raw signal                                                                              | numeric       | 1637369 - 1725515                                 |                     | measured     |
| RAWH2OREF     | A measure of the optical amplitude of the H2O channel, primarily for diagnostic purposes      | numeric       | 1703751 - 1705599                                 |                     | measured     |
| label         | Unique aspect label                                                                           | categorical   | SR_1_2000_East - SR_5_2800_west                   |                     | defined      |
| plotID        | Plot number                                                                                   | categorical   | Plot_1 - Plot_5                                   |                     | defined      |
| uniqueID      | Unique ID per plot                                                                            | categorical   | Site_1_2000_east_Plot_1 - Site_5_2800_west_Plot_5 |                     | defined      |

## Microclimate
### Tomst data loggers
|    | Variable.name | Description                                                   | Variable type | Variable range or levels | Units       | How.measured |
|----|---------------|---------------------------------------------------------------|---------------|--------------------------|-------------|--------------|
| 1  | X             | Unique numbers generated by CSV export in R. Not reliable.    | numeric       | 1 - 25220                | numeric     | defined      |
| 2  | plot_id       | Plot ID that is site number, aspect initial, and plot number. | categorical   | 1E1 - 5W5                | categorical | defined      |
| 3  | tomst_id      | Unique serial number for individual Tomst unit                | numeric       | 94212206 - 95224159      | numeric     | defined      |
| 4  | site          | Site number                                                   | numeric       | 1 - 5                    | numeric     | defined      |
| 5  | aspect        | Plot aspect                                                   | categorical   | east - west              | categorical | defined      |
| 6  | plot          | Plot number                                                   | numeric       | 1 - 5                    | numeric     | defined      |
| 7  | datetime      | Date and time of measurement                                  | categorical   | 2023-12-07 - 2023-12-16  | categorical | defined      |
| 8  | zone          | We don't know what this does                                  | numeric       | 4 - 8                    | numeric     | defined      |
| 9  | temp_soil_C   | Air temperature, 15 cm above ground                           | numeric       | 8.375 - 27.375           | numeric     | measured     |
| 10 | temp_ground_C | Temperature at ground level                                   | numeric       | 4.438 - 36.625           | numeric     | measured     |
| 11 | temp_air_C    | Soil temperature, 8 cm belowground                            | numeric       | 2.75 - 36.75             | numeric     | measured     |
| 12 | moist         | Raw moisture reading                                          | numeric       | 1180 - 3529              | numeric     | measured     |
| 13 | moist_vol     | Converted soil moisture content                               | numeric       | 11.834 - 55.737          | numeric     | measured     |

### Handheld FLIR images
| Variable name     | Description                                                                        | Variable type | Variable range or levels    | Units     | How measured |
|-------------------|------------------------------------------------------------------------------------|---------------|-----------------------------|-----------|--------------|
| X                 | Unique numbers generated by CSV export in R. Not reliable.                         | numeric       | 1 - 3875269                 |           | defined      |
| file_number       | Numbers in the file name. Used to join with metadata.                              | numeric       | 5607 - 7113                 |           | defined      |
| temp_C            | Temperature of pixel in image                                                      | numeric       | 0.029 - 72.944              | degrees C | measured     |
| day.night         | Whether measured during day or night fluxes                                        | categorical   | day - night                 |           | defined      |
| day..NOT.DATE.... | Day in Dec 2023 that image was taken                                               | numeric       | 16-Aug                      |           | defined      |
| time              | Time of image capture as recorded on metadata sheet.                               | categorical   |  - 22:42                    | hh:mm     | defined      |
| siteID            | Site number                                                                        | numeric       | 6-Jan                       |           | defined      |
| elevation_m_asl   | Site elevation                                                                     | numeric       | 2000 - 3000                 | m         | defined      |
| aspect            | Transect aspect assignment                                                         | NA            | NA                          |           | defined      |
| plotID            | Number of plot on transect                                                         | numeric       | 1 - 10.6                    |           | defined      |
| Remarks           | Comments on image                                                                  | categorical   |  - some subplots doubled    |           | defined      |
| Note.taker        | Who took the notes                                                                 | categorical   |  - PN                       |           | defined      |
| Flag              | Flag generated through R code for analysing FLIR images                            | categorical   | NA                          |           | defined      |
| start             | First file number in the sequence. Safe to ignore.                                 | numeric       | 5607 - 7107                 |           | defined      |
| end               | Final file number in the sequence. Safe to ignore.                                 | numeric       | 5614 - 57986                |           | defined      |
| file_name         | File name generated by concatenating 'FLIR', file_number' and '.jpg'               | categorical   | FLIR5607.jpg - FLIR7113.jpg |           | defined      |
| flag              | Flag generated through R code cleaning FLIR data. Filter to 'okay' for clean data. | categorical   | okay - okay                 |           | defined      |
| dataset           | Set to 'FLIR' for comparison across other microclimate metrics                     | categorical   | FLIR - FLIR                 |           | defined      |
| elevation         | Duplicate of elevation_m_asl                                                       | numeric       | 2000 - 3000                 | m         | defined      |
