Added for predator update 02/22/2022 (or better 22 02 2022)

The updated dataset is attached. All of the variable names are the same (and represent the same info as before!), and the prey list hasn’t changed. I did change a few things based on our conversations:

 

    Dataset runs through 2021 now since the 2021 diet data are available. We weren’t quite done in mid-Jan, and I didn’t want to send you a partial dataset back then.
    I eliminated both smooth dogfish and winter skate diet info from the datasets.
    The predators included are as follows:
        Summer Flounder 21-70 cm
        Silver Hake 21-76 cm
        Weakfish 26-50 cm
        Atlantic Cod 81-150 cm (we actually had some!)
        Bluefish 3 – 118 cm
        Striped Bass 31 – 120 cm
        Spanish Mackerel 10 – 33.5 cm (everything we had)
        Spotted Sea Trout 15.5 – 34 cm (again, everything we had)
        Spiny Dogfish 36 – 117 cm
        Goosefish 5 – 124 cm


The ‘NEAMAP_Mean stomach weights_Bluefish prey.csv’ file provides tow-level data, and I believe is similar to the input file that you have been using for the Center’s data in VAST. A few things on that file:

 

For each tow (row), I restricted the predators (and their stomachs) to the list that we talked about the other day, but I added goosefish, which we do catch, and filtered the piscivores using the Garrison and Link length cut-offs. Once I had filtered the predators, I filtered the prey types in their stomachs to include only those that we discussed on Friday (minus those that we decided to ditch from the original list, like decapods, unidentified categories, etc).  So then:

 

Sumbluepreywt – is the total weight, in grams, of all bluefish prey types encountered across all eligible piscivore stomachs evaluated from that tow

 

Nbfpreyspp – represents the total number of bluefish prey categories (species) encountered in those stomachs.

 

Npiscspp – the number of piscivore species from which stomach samples were examined on that tow (post-filtering for piscivores of interest and sizes of interest within those species)

 

Nstomtot – number of stomachs examined on that tow (again, post-filtering)

 

Meanbluepreywt – sumbluepreywt/nstomtot

 

Meanpisclen.simple – the simple arithmetic mean of the lengths of the individual piscivores that were evaluated for diet in that tow (so, for example, in the first row, the mean length of the 10 fish whose stomachs are represented by that row).

 

Meanpisclen.weighted – like you all, we have a two stage cluster sampling design that we implement to select fishes for diet analysis from each catch. So each fish that we examine for diet represents some number of fish in the total catch in that tow, which is given by an expansion factor. I therefore also calculated mean length as (sum(expansion * pisclenth)/sum(expansion)), to basically weight the length measurements by the abundance of those piscivores in the catches.

 

Remaining variables are water depth in meters (depthm), latitude and longitude at which the tow occurred (lat & lon), and bottom water temperature, bottom salinity, and bottom dissolved oxygen concentration at each side (bWT, bSA, bDO).

 

I want to describe the first row of data, just to make sure that you and I calculated our quantities in the same way (if not, I can adjust to match yours). So the first tow is identified as NM20070901001, which took place in Fall 2007. The total weight of bluefish prey (from our list) in that tow was 15.176g, and was represented by just one species (Etrumeus teres). In fact, it was 2 round herring encountered in the stomach of a single summer flounder at that tow. Overall, however, we looked at the stomachs of 10 fish that are considered piscivores (post filtering) – 6 summer flounder, 1 smooth dogfish, and 3 winter skates made the cut. Because of that, the number of piscivorous species in that tow was 3, and the total number of stomachs was 10. The mean bluefish prey weight was then 15.176/10 = 1.5176. By calculating the average length of those 10 piscivores, I got 53.78174, and the weighted average of the lengths of those piscivores was 55.85268, which was the sum of the products of their lengths and expansion factors, divided by the sum of their expansion factors. Also note, if we made a tow where we caught piscivores of interest and examined their stomachs, and did not find any prey of interest, I did include those tows in this dataset (i.e., included zero observation). Those are denoted by 0’s in the ‘sumbluepreywt’, ‘nbfpreyspp’, and ‘meanbluepreywt’ columns.

 

Again, I just wanted to (hopefully) clearly outline the above paragraph, so that we could verify (or not) that we did our math the same way.

 

The second file is the individual-level data that we discussed. In this case, each row represents the total weight of a given prey type/species encountered in the stomach of an individual piscivore in a given tow. If a given piscivore individual only consumed of one of the prey types of interest, it is represented by a single row in this dataset (e.g., first row of data in the dataset). If the piscivore consumed two different prey types of interest, it would get two rows of data. For example, row 14 & 15 show that summer flounder (vimscode = 3) specimid number 6 collected at site NM20070901003 consumed both Anchoa mitchilli and Loligo pealeii. The totalpreywt variable gives the total weight (in g) of each prey type in the individual predator’s stomach. The length of the piscivore is provided (pisclen) as is the expansion factor (expansion) to get those data to the ‘catch level’. The covariates to the right of ‘expansion’ are the same as included in the tow-level file discussed above.

 

I’m not sure that this individual file is exactly what we might need, but I figured I’d send it as a first cut. Specifically, any station/piscivore combinations where a prey of interest was not encountered in the stomachs are not included in this dataset (i.e., doesn’t account for ‘0 observations’0). I’m wondering, if we want to try to apply these models at the prey species level, if we might want to follow the format of the ‘mean stomach weights’ file discussed above, but instead of aggregating across prey types, we make a separate dataset for each prey type of interest. I think that is what Ng did for the Atlantic herring paper (?).

 