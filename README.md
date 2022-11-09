# GEFAAR
A Generic Framework for the Analysis of Antimicrobial Resistance

## Requirements
To run GEFAAR, you need R (Version 4.1.0 or higher) and R Shiny.

##  Installation
To install GEFAAR, just download the repository. All required packages will be installed, all functions loaded automatically by the help of the global.R script.

## Running GEFAAR
GEFAAR is available as an R Shiny GUI. To run the software: 1) navigate to the folder in which the following files are stored: `global.R`, `ui.R`, `server.R`, `www/IMI.png`, `www/UKM.png` and `www/white.png`. 2) Execute `Run App`.


## Examplary use of GEFAAR

## Input
To execute any analyses with GEFAAR, in input file has to be 1) read in and 2) configured.

### Read in file

* `Upload input file` Select a tabular file for upload, containing information on antimicrobial resistance.
* `Separator input file` Select the field separator used in the input file (one of: Comma, Semincolon, Tab).

### Configure input

* `Select column containing information on...`
  * `...species` Name of the column containing information on the analyzed species.
  * `...clinic` Name of the column containing information on the clinic.
  * `...specimen` Name of the column containing information on the specimen, in which the species were detected.
  * `...date` Name of the column containing information on the date at which samples were taken.
    * `-> date format` Underlying date format used in the input file (one of: dd.mm.yy, dd.mm.yyyy, mm/dd/yy, mm/dd/yyyy, yy-mm-dd, yyyy-mm-dd)
  * `...first antimicrobial agent`Name of the column containing information on the first antimicrobial agent that was tested. All subsequent columns are assumed to contain information on antimicrobial agents as well. The following coding of resistance information is required: 'S' for susceptible, 'I' for susceptible increased exposure, 'R' for resistant, '-' for not analyzed (according to EUCAST).
      

### Example

Select exemplary input file 'Example/Input/Examplary_input.txt' and separator `Tab`. Execute `Read in file`.

Select column 'Species' for `...species`, 'Clinic' for `...clinic`, 'Specimen' for `...specimen`, 'Date' for `...date`, 'yyyy-mm-dd' for `-> date format` and 'Ampicillin' for `...first antimicrobial agent`. Execute `Configure input`.


## Pathogen statistics

As soon as an input file has been uploaded and configured, pathogen statistics can be determined.

### Details

* `Year` Select the year for which data shall be analyzed (default: first available year)
* `Specimen` Select all or a specific specimen for which pathogen statistics shall be determined (default: all).
* `Clinic` Select all or a specific clinic for which pathogen statistics shall be determined (default: all).
* `Cut-off min. 30 cases` Select if cut-off of at least 30 cases shall be applied (as in WHO GLASS Report) (default: yes).

Input options are interactively updated, dependent on the uploaded input file (e.g. only those years can be selected that are available in the previously defined date-column).

### Output

An interactive version of the patoghen statistics is available in the right panel of the shiny interface, tab `Pathogen statistics`. The analysis can be exported as CSV, Excel or PDF (=Print) file. 


### Example

Keep default parameters (`Year` 2021, `Specimen` All, `Clinic` All, `Cut-off min 30 cases` Yes) and execute `Start analysis`. The interactive pathogen statistics are generated. Export the results as Excel file. The output file to be expected is available in 'Example/Output/GEFAAR_Pathogen_Statistics.xlsx'.



## Resistance statistics

As soon as an input file has been uploaded and configured, resistance statistics can be determined.

### Details

* `Year` Select the year for which data shall be analyzed (default: first available year)
* `Specimen` Select all or a specific specimen for which resistance statistics shall be determined (default: all).
* `Clinic` Select all or a specific clinic for which resistance statistics shall be determined (default: all).
* `Species` Select all or a specific species for which resistance statistics shall be determined (default: all).

Input options are interactively updated, dependent on the uploaded input file (e.g. only those years can be selected that are available in the previously defined date-column).

### Output

The resistance statistics consist of the `Data sheet antimicrobial agents` and the `Figures antimicrobial agents`. An interactive version is available in the right panel of the shiny interface, tab `Resistance statistics`. The analysis can be exported as xlsx file by executing `xlsx export`.


### Example

Keep default parameters (`Year` 2021, `Specimen` All, `Clinic` All, `Species` All) and execute `Start analysis`. The interactive resistance statistics are generated. Execute `xlsx export`to export the results as Excel file. The output file to be expected is available in 'Example/Output/GEFAAR_Resistance_Statistics_2021_Specimen_All_Species_All_Clinic_All.xlsx'.


## Cluster analyses

As soon as an input file has been uploaded and configured, cluster analyses can be conducted. Two versions are available: `Interactive` and `Export`.

### Details

* `File name of the PDF export` Select standard naming (GEFAAR_Resistance-Cluster-Analyses_<date>) or define an individual name (default: standard; only EXPORT).
* `Year` Select the year for which data shall be analyzed (default: first available year).
* `Specimen` Select all or a specific specimen for which cluster analyses shall be conducted (default: all).
* `Analysis is independently conducted` Select whether an analysis 'per species' or 'per clinic' shall be conducted (default: per species; radio buttons for INTERACTIVE, check boxes for EXPORT).

#### Per species
* `Analyze all species? (min. 30 cases)` Select whether all or a selected set of species shall be analyzed. If 'No' is chosen, all available species are automatically displayed and an individual selection can be made (default: yes).

* `Visualization heatmap` Select analyses by heatmap to be conducted.
  * Data ordered by 1) clinic, 2) resistance
  * Data ordered by 1) clinic, 2) date
  * Hierarchical clustering
* `Visualization UMAP` Select analyses by UMAP to be conducted.
  * Plot with colored clinics
  * Plot with colored clusters 
    * Additional heatmap: Data ordered by UMAP clusters
    * Additional heatmap: Data ordered by clinics 
    
#### Per clinic
* `Analyze all clinics? (min. 30 cases)` Select whether all or a selected set of clinics shall be analyzed. If 'No' is chosen, all available clinics are automatically displayed and an individual selection can be made (default: yes).

* `Visualization heatmap` Select analyses by heatmap to be conducted.
  * Data ordered by species
  * Hierarchical clustering
  
Input options are interactively updated, dependent on the uploaded input file (e.g. only those years can be selected that are available in the previously defined date-column).


### Output

An interactive version of the cluster analyses is available in the right panel of the shiny interface, tab `Cluster analyses`. The analysis can be exported as PDF file, by switching to the export version of the cluster analyses.


### Example

Select version `Export`. Keep default parameters (`File name` Standard,`Year` 2021, `Specimen` All). Select 'per species' and 'per clinic' and check all possible analysis options. Execute `Download`. A PDF report, containing all resistance cluster analyses, is automatically generated. The output file to be expected is available in 'Example/Output/GEFAAR_Resistance-Cluster-Analyses_2022-11-09.pdf'.

As an alternative: select version `Interactive`. Keep default parameters (`File name` Standard,`Year` 2021, `Specimen` All). Select either 'per species' or 'per clinic' and check all possible analysis options. Execute `Start analysis`. 




## Reporting errors / Requesting features

If you should experience any errors using GEFAAR, or you are looking for some additional features, feel free to open an issue or to contact Sarah Sandmann ( sarah.sandmann@uni-muenster.de ).


