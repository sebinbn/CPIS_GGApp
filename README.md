# CPIS_GGApp

This repository has code that runs the Shiny for R app called CPIS_GG_App hosted on shinyapps.io The app can be found [here](https://sebin-b-nidhiri.shinyapps.io/CPIS_GG_App/).

The app currently uses data in CPIS_GG.rds included in the repository. This file is created using CPIS_RDSCreate.R. To replicate the data creation one will need a copy of the .csv of the CPIS dataset.The data can be bulk downloaded from the IMF portal [here](https://data.imf.org/?sk=b981b4e3-4e58-467e-9b90-9de0c3367363). This requires signing up for an account on the IMF data portal. However, to replicate the app locally, you can use the data given here and need not create the data.

IMF has restrictions on the number and rate of API requests. Creating each picture in the app requires data from around 240 countries which translates to 240 API requests. This is why API queries are not used in generating images in the app.
