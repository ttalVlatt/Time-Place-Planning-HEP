This GitHub repo contains everything you need to reproduce all the analysis from Capaldi et al.'s (2026)
"Time and Place for (Sustainable) Campus Planning" in Higher Education Policy

The most valuable item for future research is Master-Plan-Text.csv
- This contains plain text of all available campus master plans at large public US universities,
along with the institution's IPEDS UNITID and other variables used in the paper

The needed code is all in 01-STM.R
- Providing you have installed all the libraries, this should run out of the box

There are 2 .Rdata files in the `R-Objects/` folder, `Many-Topics.Rdata` and `Search-K.Rdata`
- These are pre-calculated outputs of the two longest run-time processes that identify the underlying topic model
(running each can range from a few hours to exceeding capacity depending on your machine)
- The script is set up to read them if they are there, or calculate them if not, so if you want to change something 
in them or truly reproduce everything, simply delete the 2 .Rdata files
    - They will be re-written as the code runs