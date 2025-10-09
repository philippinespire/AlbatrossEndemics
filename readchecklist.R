## First, find Albatross holotypes

lines <- readLines("Balisco and Liao 2025 checklist.txt") # read in the Appendix

out = data.frame(species = character(0), albatross = logical(0)) # hold results

GETALBSTAT <- FALSE # set a flag
for(i in 1:length(lines)){
    if(grepl("^\\([0-9]+\\)", lines[i])){ # continue until hit a species
		sppname <- sapply(strsplit(lines[i], "\\s+"), `[`, 2) # note that these are missing a space between genus and species
		outline = data.frame(species =  sppname)
		GETALBSTAT <- TRUE # start looking for whether or not the holotype was found by the Albatross
	}
	if(GETALBSTAT){
		if(grepl("Holotype", lines[i])){ # continue until hit a holotype
		    if(grepl('Albatross', lines[i])){ # if Albatross holotype
		        outline$albatross =  TRUE
    			out = rbind(out, outline) # append to results
		    } else {
		        GETALBSTAT <- FALSE # stop looking for albatross status if holotype line didn't mention Albatross    
		    }
		}
	}
}

head(out)
nrow(out) # 239 species described from the Albatross collection. Some are junior synonyms.



## Second, find Philippine endemics
status <- read.csv('zookeys-1246-001_article-145752__-s001.csv', skip =1) # read in Table S1

status$sppname <- sapply(strsplit(status$Original.ScientificName, "\\s+"), function(y) paste(head(y, 2), collapse = " ")) # get only first two words

ve_lines <- which(status$Status == "VE") # get lines with Philippines endemics (marked "VE")
endemics <- status[ve_lines, c("Original.ScientificName", "Status", "ValidAs.Remarks", "sppname")]

nrow(endemics) # 221 endemics



## Third, find Philippine endemics found by the Albatross
validrows <- endemics$ValidAs.Remarks == "" # find lines (T/F) without "valid as" entries. Having an entry implies the original name is a junior synonym, and not valid.
endemicspp <- gsub(" ", "", endemics$sppname) # set up a list endemics to match against. No spaces since the Albatross holotypes is missing spaces.

pattern <- paste(out$species, collapse = "|") # long search pattern of all Albatross holotypes
Albatross_endemic_lines <- grepl(pattern, endemicspp, ignore.case = TRUE) & validrows # find entries in endemicspp that are Albatross holotypes and valid as senior synonyms

sum(Albatross_endemic_lines) # finds 14 species
Albatross_endemics <- endemics$sppname[Albatross_endemic_lines] # list of the species

## Write out
write.csv(endemics[Albatross_endemic_lines, c("Original.ScientificName", "ValidAs.Remarks", "sppname")], file = "Albatross_endemics.csv", row.names = FALSE)


## Bonus: find senior synonyms described by the Albatross (remove junior synonmyms)
validspptomatch <- gsub(" ", "", status$sppname[status$ValidAs.Remarks == ""])
pattern <- paste(out$species, collapse = "|") # long search pattern of all Albatross holotypes
Albatross_valid_lines <- grepl(pattern, validspptomatch, ignore.case = TRUE) # find entries in validspptomatch that are Albatross holotypes

sum(Albatross_valid_lines) # finds 117 species
