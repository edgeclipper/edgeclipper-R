# edgeclipper-R
R version of Edgeclipper software

Python version of Edgeclipper software can be found [here][1]

## Functions

### openReport Files

opens and reads files

### generateFileNames

filename extraction - creates file names

### getNodes

gets the nodes from a file with node names 

### getNetworks

gets all of the networks with no parsing

### getNetsAll

gets all of the networks from the BANJO files in the same working directory

### reorderNetworks

writes each network only if it matches the score regex

### rewriteScoreFile

parses the network/score file and acts as a wrapper for the *reorderNetworks* function

### grabScores

gets the parsed score information and returns the scores

### getMaxScore

gets the max score

### bvalCalculator

calculates the b-values

### percent_appear

generates the decisions for whether and edge shoud be included or not

**Major assumption**: an edge must be present with frequency = 1.0 to include it

### bvalConsensus

generates consensus network given the b-values and acts as a wrapper for the *percent_appear* function

### readEdgeInformation

generates the formatted information for the c-value output

### bvalWrapper

main wrapper for b-value computation and output

### cvalWrapper

main wrapper for c-value computation and output


[1]: https://github.com/drhodges/pyclipper