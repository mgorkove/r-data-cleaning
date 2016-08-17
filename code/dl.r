library("stringdist")
library("data.table")

file = "allSandagTest2.csv" #dirty data
col = 8 #column with agency names
h = TRUE #has header or not 


aCol = fread(file ,sep = ",", header = h,select = col )
freqTable = table(aCol) #map of agency name:frequency
map = c() #map of name: most common spelling of it
names = unique(aCol)
for (i in 1:nrow(names)) { 
  currentName = toString(names[i])
  mostCommon = currentName
  variousSpellings = c(currentName)
  
  for (j in i+1:nrow(names)) {
    if (j <= nrow(names) ){
      compared = toString(names[j])
      cnLower = sapply(currentName, tolower) #current name to lowercase
      cLower = sapply(compared, tolower) #compared to lowercase
      
      if (stringdist(c(cnLower),c(cLower)) <= 2) {
        variousSpellings = c(variousSpellings, compared)
        if (freqTable[compared] > freqTable[currentName]) mostCommon = compared
        names = names[-j] #removes compared 
        j = j - 1 
      }
    }
  }
  for (s in variousSpellings) {
    map[s] = mostCommon
    #print (paste(s, mostCommon, sep = ": ")) #for debugging -- prints map
  }
}
#fixed some of the errors:/ -----
map["MTS"] = "Metropolitan Transit System"
map["MTDB"] = "MTDB"
map["Governorâ€™s\nAppointee"] = "Governor's Appointee"
map["Governorâ€™s Appointee"] = "Governor's Appointee"
map["Regional Planning Stakeholders Working Group(SWG)"] = "Regional Planning Stakeholders Working Group"
map["Caltrans, District 12"] = "Caltrans, District 12"
map["North County"] = "North County"
map["City of San Diego â€“ A"] = "City of San Diego - A"
map["City of San Diego - A"] = "City of San Diego - A"
map["County of San Diego - A"] = "County of San Diego - A"
map["Regional Planning \"\"Technical Working\nGroup (TWG)\"\""] = "Regional Planning Technical Working Group (TWG)"
map["El Cajon CDC"] = "El Cajon CDC" 
map["SPECIAL SANDAG BOARD OF DIRECTORS\\xe2\\x80\\x99 MEETING"] = "SPECIAL SANDAG BOARD OF DIRECTORS MEETING"
map["Tribal Chairmen\\xe2\\x80\\x99s"] = "Southern California Tribal Chairmen's Association"
map["Southern California Tribal\nChairmenâ€™s Association"] = "Southern California Tribal Chairmen's Association"
map["Southern California Tribal Chairmenâ€™s Association"] = "Southern California Tribal Chairmen's Association"
map["Southern California\nTribal Chairmenâ€™s Association"] = "Southern California Tribal Chairmen's Association"
map["Southern California Tribal"] = "Southern California Tribal Chairmen's Association"
map["Southern California Tribal Chairmen\xe2\x80\x99"] = "Southern California Tribal Chairmen's Association"
map["San Diego - A"] = "San Diego - A"
map["Southern California"] = "Southern California Tribal Chairmen's Association"
map["Southern CA Tribal Chairmenâ€™s Association (SCTCA)"] = "Southern California Tribal Chairmen's Association"
map["SAN DIEGO SHERIFFâ€™S\nDEPARTMENT"] = "SAN DIEGO SHERIFF'S DEPARTMENT"
map["City of Lemon"] = "City of Lemon Grove"
map["SD County Water"] = "SD County Water Authority"
#-----

newCol = c()
for (i in 1:nrow(col3)) {
  value = toString(col3[i])
  newCol = c(newCol, map[value])
}
wholeFile = fread(file ,sep = ",", header = h,select = c(1:8) ) #read cols 1-8 of original file
combined = data.frame(wholeFile, JURISDICTION_EDITED = newCol)
outputFilename = "FinalSandagData-v2.csv"
write.csv(combined, file = outputFilename)
