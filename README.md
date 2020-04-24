# GlobeTrotters

This repository holds the scripts and data needed to recreate the results for this project. Also are methods documents for how we extracted certain datasets.

They are organized the following:

Data
- ranges.csv for natural ranges extracted from <a href="https://onlinelibrary.wiley.com/doi/pdf/10.1111/ddi.12369">Faurby & Svenning 2015</a>
- age.csv for fossil ages of species extracted from the <a href="https://paleobiodb.org/#/">PBDB</a> and <a href="https://www.sciencedirect.com/science/article/pii/S1055790314003844?casa_token=z-aihi0A1bEAAAAA:hKwGauYa23y7lp3yKZvnWds0OrmpsdoIWxV5ywBT8InkrpXGBbCOPI-0wfrRcIKvjUpI-EiHFRk">Faurby & Svenning 2015</a> (see Methods below)

Scripts
- Init.R for generating the file used for analyses here 
- DataExploration.R to understand data structure
- H1-AgeOfClades.R to investigate how the age of the lineage may influence geographic range
- H2-DiversityOfClades.R
- H3-BodySize.R
- H4-Diet.R for diet-related analyses, included diet type and diet breadth
- H5-GeographicRange.R, including the Sørenson Index

Methods
- Age for how we extracted two different age metrics
- The types of methods used for majority of our analyses
