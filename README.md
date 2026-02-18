# pristid2_maps

This repo contains R code re: the analyses for the manuscript: 

> Biskis, V. N., Schoeman, D., Townsend, K. Holmes, B. J., & Wueringer, B. E. (2026). [Size structure and mortality risk in protected sawfish populations.]. In Prep.

Additional output is outlined in the completed PhD thesis:

> Biskis, V. N., Schoeman, D., Townsend, K. Holmes, B. J., & Wueringer, B. E. (2026). [Ch. 3: Putting sawfishes back on the map...] In: Threatened sawfishes of Australia: Spatial ecology and species composition through time. PhD Dissertation. *University of the Sunshine Coast*, (https://doi.org/10.25907/01001)

------------------------------------------------------------------------

## Author: Nikki Biskis

PhD Candidate UniSC | SARA
Date: 2025-12-10

------------------------------------------------------------------------

## Objectives

*RQ1*  -- Have sawfish distributions or species composition changed over time?

> 01 = range | 02 = species comp

*RQ2*  -- Is the probability of interaction linked more closely with environmental variables - or recreational fisher effort?

> 03 = life history stages | 04 = enviro | 05 = hs modelling (removed)

*RQ3*  -- What are the key anthropomorphic drivers of shifting baselines (TL) and negative interaction (mortality)?

> 06 = total lenghth | 07 = mortality

*RQ4*  -- How does risk analyses and maps compare between citizen science and commercial data?

> 08 = risk maps

## Contents

pristid2_maps

    └── scripts             <- code used in the analysis 
    └── helpers             <- source code written by author VNB   
        ├── functions       <- stats functions written by author VNB 
    └── figs                <- .png files of figures in main text   
        ├── supps           <- additional visuals for the manuscript appendix
        ├── thesis_vs       <- figures that appear in the thesis, but not the published journal article

## Notes

All code was initially written in March 2024 - 2025, with edits prior to thesis submission in June 2025.
Changes in figure output following thesis review in December 2025.

The following modifications were done clarity/reproducibility:

1. Separated scripts and made multiple source codes
     - This is noted at the beginning of each script
     - Repetitive lines -> created a function 
  
2. Made sure all scripts run independently following an R restart
  
3. Where a large object created (ggplot), saved as rds for later loading

4. Pulled out majority of enviro code that will repurposed for future SDMs

## Questions & feedback?

Please submit an issue, or email your questions to Nikki Biskis: nikkibiskis@gmail.com

---