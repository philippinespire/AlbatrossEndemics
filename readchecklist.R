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
nrow(out) # 239 species found the Albatross



## Second, find Philippine endemics
status <- read.csv('zookeys-1246-001_article-145752__-s001.csv', skip =1) # read in Table S1

status$sppname <- status$Original.ScientificName # step one to pick a valid name
replacename <- which(status$ValidAs.Remarks != "") # find lines with "valid as" entries
status$sppname[replacename] <- status$ValidAs.Remarks[replacename] # replace valid name with "valid as" entry
status$sppname_short <- sapply(strsplit(status$sppname, "\\s+"), function(y) paste(head(y, 2), collapse = " ")) # get only first two words
status$sppname_short_oneword <- gsub(" ", "", status$sppname_short) # remove the space

ve_lines <- which(status$Status == "VE") # get lines with Philippines endemics (marked "VE")
endemics <- status[ve_lines, c("Original.ScientificName", "Status", "ValidAs.Remarks", "sppname", "sppname_short", "sppname_short_oneword")]

nrow(endemics) # 221 endemics



## Third, find Philippine endemics found by the Albatross
endemics2[] <- lapply(endemics, function(x) { # remove all spaces, since the list of Albatross holotypes is missing spaces
    if (is.character(x)) gsub(" ", "", x) else x
})

pattern <- paste(out$species, collapse = "|") # long search pattern of all Albatross holotypes
Albatross_endemic_lines <- apply(endemics2, 1, function(row) any(grepl(pattern, row, ignore.case = TRUE))) # find lines in endemics2 that are Albatross holotypes

sum(Albatross_endemic_lines) # finds 28 species
Albatross_endemics2 <- endemics$sppname_short[Albatross_endemic_lines] # list of the species, using their valid name

## Write out
write.csv(endemics[Albatross_endemic_lines, c("Original.ScientificName", "ValidAs.Remarks", "sppname_short")], file = "Albatross_endemics.csv", row.names = FALSE)
