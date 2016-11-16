#Major steps:
### Main function ###
# global variables
# Put actual directory when available
timestamp = "20101105124824";
maindir = "F:\\SomeDirectory\\";
scriptDir = maindir + "scripts\\";
workingDir = maindir + "reportFiles\\";
outputDir = maindir + "outputFiles\\";

# 2) Next get list of nodes
# Note: may need additional function to reverse the index->gene to gene->index
file_nodes = paste0(timestamp, ".probelist.txt")
nodes = getNodes(file_nodes)

# 3) Next get set of networks from the BANJO analyses
# directory = diry + 'testReports\\';
# Note: this section should be adjusted for your particular file naming strategy!
filenames = generateFileNames(timestamp, 0:20)

# 4) generate the condensed network information file with sorted scores
netfile1 = "tempNetFile.txt"
netfile2 = "tempNetFile2.txt"

# 5 rewrite the networks into a parsable format
network.scores = getNetsAll(filenames, netfile1, os, re)

# Next reformat the testNetOut temporary file so that networks are ordered by score
#scores.uniq <- network.scores[3].keys()
scores.uniq = networks.scores[3]
scores.uniq = sort(scores.uniq)
rewriteScoresFile(netfile1, netfile2, scores.uniq, re)

# 6 get b-value information
outputBvals = bvalCalculator(netfile2,e)

# 7 write b-value info and corresponding consensus networks to file
edgePercent <- 1.0
bval.file <- "bvalEdgeInfo.txt"
bvalWrapper(outputBvals, bval.file, netfile2, edgePercent)

# finally, generate c-values for edges and store to file
cval.file <- "cvalEdgeInfo.txt"
cvalWrapper(bval.file, cval.file, nodes)



