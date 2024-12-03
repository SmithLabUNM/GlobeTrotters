# GlobeTrotters

This repository holds the scripts and data needed to recreate the results for this project. Also are methods documents for how we extracted certain datasets.

They are organized the following:

## Data

We used the updated Body Mass of Late Quaternary Mammals dataset ((Smith et al. 2003)[https://doi.org/10.1890/02-9003]) to (version 11.1)[https://github.com/SmithLabUNM/MOM-Database/tree/MOMv11.1]. We additionally collected contitnet of family origin ("familyOrigin.xlsx"). We also added in generic first appearance from the (PaleoBioDB)[paleobiodb.org] and (Faurby et al. 2018 (PHYLACINE)[http://doi.org/10.5281/zenodo.1250504]). We also combined data about geographic range, home range, and age of dispersal from (Jones et al. 2009 (PanTHERIA)[https://doi.org/10.1890/08-1494.1]), current and natural ranges from (Faurby et al. 2018 (PHYLACINE)[http://doi.org/10.5281/zenodo.1250504]), as well as generation length from (Pacifici et al. 2013)[https://doi.org/10.3897/natureconservation.5.5734]. We do not republish existing datasets here.

## Scripts

The script is divided into different sections: 
- Loading packages for the environment
- Creating the template for figures
- Loading the data (with instructions on how to do that)
- Data manipulation
  - fixing diet categories
  - filling in missing data
  - trimming the data
  - adding in species ages (also see file in Metadata folder)
  - combining data
  - creating binning and summary statistics
 - Exploring the data
 - Q1: Number of species on each continent
 - H1: Continental connectivity
 - H2: Family origin continent
 - H3: Family richness
 - H4: Body size
 - H5: Diet
 - H6: Habitat mode
 - Putting these hypotheses together:
   - Decision tree
   - Log-odds ratio
  -  Sensitivity testing of Geographic range
