Analytical Paleobiology - Summer 2024 - Gastropoda Project 
Alissa Watson - 22 July 2024

Download data from PBDB with certain filters
	1. Go to PBDB -> Data -> Download
	2. Select by taxonomy -> Taxon -> "gastropoda"
	3. Select by geological context -> Environment -> Siliclastic
	4. Choose output options -> check "ecospace","stratigraphy", "geological context", "location", "stratigraphy ext.", "time binning", "genus", and "abundance" -> 		
         uncheck Include metadata at the beginning of the output
	5. Data was collected on 15 July 2024 at 14:37.

Start R script

# Set working database
# Read in data file 
	1. look at the data -> 59807 observations of 58 variables
# Extract only columns necessary
	1. Occurence_no, collection_no, early_interval, late_interval, max_ma, min_ma, reference_no, genus, formation, zone, environment, and time_bins
# Find diversity of each time bin
# Get preloaded divDyn data (for each time bin)
# Categorize data by time intervals and time bins
# Fixed Ordovician stages modifying code provided by Adam Kocsis
	1. Load data using: https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData
	2. Source code: https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R
# Plot raw overall occurrences of data
	1. Separate based on 3 different environments
# Get data ready for sqs tests
	1. Subset data based on 3 different environments
	2. Set seed to be able to reproduce results (95532)
# Use uncorrected sqs to test different parameters (for all 3 depositional environments)
	1. Look at different q levels (0.3 and 0.5)
	2. Look at failed tests for both q levels
# Prepare for plotting 
	1. Merge sqs data acquired with time scale
# Create basic plots
	1. Use range-through (RT) diversity to compare different q levels
	2. Use range-through (RT) diversity to compare different q levels with Failed tests
	3. Use range-through (RT) diversity to look at 0.3 q level
	4. Use range-through (RT) diversity to look at 0.3 q level with Failed tests
	5. Look to see if Pull-of-the-Recent is effecting the graph
		a. only look at time bins [1:94] 
	6. Use corrected sampled-in-bin (CSIB) diversity to compare different q levels
	7. Use corrected sampled-in-bin (CSIB) diversity to compare different q levels with Failed Tests
	8. Look at just Carnian pluvial episode of RT diversity with 0.3 q level
		a. only look at time bins [43:69]
	9. Look at just Carnian pluvial episode of RT diversity with 0.3 q level with Failed Tests
	10. Look at just Carnian pluvial episode of CSIB diversity with 0.3 q level


Colors used
1. Paralic - #8ea8c3
2. Deltaic - #819e57
3. Non-deltaic - #0b2545
