# nbs_synthesis
R-script that compiles and analyses an excel based meta-dataset of nature based solution outcomes.

The dataset was producted in 2023 for a rapid assessment of the economic implications of adopting field-scale NbS practices in non-high income countries. Central to our research approach was an emphasis on profit, moving away from the often-cited gross revenue metrics. Profit, which factors in the costs, gives us an unfiltered view of the farmer's genuine earnings, thereby offering a more realistic understanding of the financial dynamics of NbS. This analysis is informed by 181 studies.

The data can be found in the ```./data/NBS_extraction_table.xlsx``` excel file.

The analysis script is ```./nbs_analysis.R```.

To understand this analysis please read the working paper "Steward P, Joshi N, Kacha G, Ombewa B, Mumo E, Muller L, Youngberg B,Magnan N, and Rosensotck T. 2023. Economic benefits and costs of nature-based solutions in low- and middle-income countries". Working Paper. Alliance of Bioversity-CIAT. Rome". A copy of this paper can be found in the main project directory.

Analysis results are saved in folders ```data\TV_merged``` or ```data\TV_not_merged```.  For results in the ```TV_merged``` total and variable cost are renamed to "Cost" and are therefore analysed together. Where total and variable costs are both reported by a study, only total cost is retained.  For results in the ```TV_not_merged```  folder total and variable costs are considered separately. Within the ```data\TV_merged``` or ```data\TV_not_merged``` folders are subfolders named to reflect the maximum percentage of negative outcomes allowed before a combination of NbS practice x economic outcome is rejected from the dataset. For example ```data\TV_merged\Neg_7.5perc``` the final subfolder means that up to 7.5% negative outcomes were allowed per NbS practice x economic outcome.

The controls for the above analysis parameters can be found in analysis script ```./nbs_analysis.R``` on L249: ```max_neg<-7.5 # What is the maximum perc of neg values allowed per practice x outcome combination?``` and L250: ```combine_TV<-T # combine total and variable costs?```

A further control ```do_combos``` is important (L248: ```do_combos<-F # Combine practices or analysis raw? ```).  
The ```do_combos``` control determines whether practices are considered: 
  1) "raw" where the practice names used to group data for analysis are unchanged, in this case outcomes for "raw" results are given the name "Bundles" (because individual bundles of practices are analysed, and not merged); or 
  2) combined; for all individual practices considered, all observations containing a specific practice are subset and the practice name set to that practice, then data are recombined. For example, if the practice is ```reduced tillage``` then we use ```grep("reduced tillage",practice)``` to search for all observations containing the practice. This fidns observations with the practice alone ```reduced tillage``` or in combination with other practices, e.g.m ```reduced_fertizer+reduced_tillage```. The practice name for all the practices is then set to ```reduced tillage```, so ```reduced_fertizer+reduced_tillage``` becomes ```reduced tillage```. This process is repeated for the all the practices we consider in the analysis and the results recombined into a single table.




The World Bank supported this work. 
