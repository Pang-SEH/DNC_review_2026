# Demographic Niche Concept, Review Article, supporting R scripts
R scripts used to inform two figures in the review article  
Title: "From niche theory to demographic realities: the demographic niche concept for understanding range-wide population dynamics"  
Study: XXX  
DOI: XXX  
COMPADRE & COMADRE dataset: https://compadre-db.org/

## 1_DNCrev_vsp_simple_sim_dn_overlaps.R
A simple simulation of a virtual, hypothetical annual plant species with two varying vital rates. Simulations visualise how demographic niche boundaries might be identified and how their overlaps are required, but not always sufficient, for persistence (depending on how boundaries of the demographic niche are derived). Response curves in Figure 1a (top panel) were adapted from these simulations; not an exact illustration.

## 2_DNCrev_demo_data_accumulation_code.R
Script detailing how meta data from the COMPADRE and COMADRE datasets were extracted, cleaned, and counted (brute force calculations). Only basic data verifications were used to determine the relevant data (e.g., simple spatial thinning of data points). Code for creating Figure 8 is included.
