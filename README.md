# melanoma-project
--
Authors: Dylan Reher & Patrick Murphy
Date Created: October 15th, 2025
Last Modified: 10 November 2025
Name: Biology-185 Shiny Project
Location: WL-Biol185-ShinyProjects

--

PROJECT TITLE AND DESCRIPTION:

This repository is a website that we designed for our BIOL-185 class at 
Washington & Lee University in Lexington, VA. For this project, we were prompted 
to use the coding resources and knowledge that we have acquired throughout the 
class to build our own website showcasing and visualizing a data set of our 
choice. The appropriate data sources can be found in the "Acknowledgments & 
Resources" section.

The website, when loaded, contains four primary pages: "Home", "Visualizations", 
"Data Explorer", and "About Us". When the website it loaded, Home is the default 
page. The Home page provides a brief description of the content of our website, 
how to use it, and why we were interested in this data. 

The visualizations page is where most of the coding work comes into play - here,
the user can select between 5 different visualizations, and also the state in 
which they wish to examine. 

--

TABLE OF CONTENTS:

Installation Instructions and Version History
Usage Instructions
Contribution Guidelines and Bug Reporting
License Information
Acknowledgements and Resources
Contact Information

--

INSTALLATION INSTRUCTIONS AND VERSION HISTORY:

Beta v0.6.1 (unreleased) - modifying several counties without correct 
  data mapping in leaflet, fixing bivarate visualizations 
  
  --
  
Previous versions:
v0.6.0 - added all the remaining tables in the data explorer tab, and added 
  download buttons to the tables. Also removed unnecessary options that the 
  user would never intend to select on the visualizations page. 
v0.5.2 - Added "Data explorer tabs" and Streamlined Bivarate Data to correct 
  critical logical errors 
v0.5.1 - Modified Bivarate Data to correct logical errors 
v0.5.0 - Added Bivarate Data to visualize "Risk-Adjusted: UV x Melanoma (White 
  Pop Weighted)"
v0.4.1 - Corrected color scheming to make a visually appealing and informative 
  color scheme
v0.4.0 - Added an option to visualize "Bivarate UV Measurement x Melanoma 
  Rate"
v0.3.1 - Updated the layout of the home page to make more sense 
v0.3.0 - Updated the home page to include a short summary of our project, some 
  images to show what melanoma is, images to show our resources for raw data, 
  and a brief description of how to navigate the website.
v0.2.5 - Updated the color scheme of the data to provide greater clarity on the 
  two melanoma single-variable visualizations.
v0.2.4 - Added a box that shows a description of the variable that is selected 
  when the user clicks on it.
v0.2.3 - Modified the color scheme to make data visualization clarity better
v0.2.2 - Modified the data cleanup file to further clean our data and make it 
  easier to map and read across multiple tables
v0.2.1 - Corrected Melanoma visualization to show empty data and missing 
  couinties.
v0.2.0 - Mapped Melanoma by County on the leaflet map
v0.1.1 - Added an interactive data table and interactive zoomable leaflet map 
  with no other features
v0.1.0 - Website frame created with three tabs - home, visualizations, data 
  explorer, completed data cleanup and proper cleaning of tables, 
  
This code has been tested to run without knowledgeable error on R v4.4.2 on 
November 10th, 2025. Newer versions of R are NOT TESTED and not recommended for 
this program. To ensure full feature availability, please use this version of R.

This code requires the following open-source packages to be installed alongside 
this code in R:

- shiny
- markdown
- cowplot
- leaflet
- reactable
- biscale
- ggplot2
- dplyr
- tigris
- sf

Please ensure that these packages are correctly installed in your R environment. 

--

USAGE INSTRUCTIONS:

This web app can be run by launching the code in an r-studio session or in the 
R desktop app. 

--

CONTRIBUTION GUIDELINES AND BUG REPORTING:

To report bugs, please see the CONTACT INFORMATION section below. For 
contribution guidelines, please refer to the license information attached to 
this project

--

LICENSE INFORMATION:

This code is protected with a GPL 3.0 License which explicitly states that users
are allowed to distribute verbatim copies of this code and use it in other
projects, so long as the code is not modified and cited correctly. For more
information on the GPL 3.0 License, visit the GPL 3.0 ReadMe file in this
repository

--

ACKNOWLEDGEMENTS AND RESOURCES:

This project is the sole creation of the authors, as well as with assistance
from content generation resources, specifically Claude(R) and ChatGPT 5
Thinking.

Data for this web app comes from a variety of resources, including all of the
following: 

1. State Cancer Profiles - NCI & CDC
https://statecancerprofiles.cancer.gov/incidencerates/index.php?age=001&areatype=state&cancer=053&race=00&sex=0&sortOrder=asc&sortVariableName=rate&stage=999&stateFIPS=00&stream=top&type=incd&utm_campaign=sendto_localnewslettertest&utm_medium=email&year=0&utm_source=chatgpt.com

2. Cancer.gov - UV data by county, shows the intensity of UV (average) by county
https://gis.cancer.gov/tools/uv-exposure/uv-county.xlsx 

3. County Demographics - Corgis-edu.github.io
https://corgis-edu.github.io/corgis/datasets/csv/county_demographics/county_demographics.csv 

--

CONTACT INFORMATION:

(C) Patrick Murphy and Dylan Reher, 2025
Patrick Murphy: pmurphy@mail.wlu.edu
Dylan Reher: dreher@mail.wlu.edu




