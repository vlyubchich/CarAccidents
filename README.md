The main file is `CarAccidents_report.Rmd` that generates the report `CarAccidents_report.pdf`.
The RMD document: 
- grabs data from the folders `dataraw/` and `dataderived/`
- uses formatting LaTeX code from `preamble.tex` and potentially references from `ref_trafficCAUS.bib`
- generates and uses the file with BibTeX references for R packages `refPackages.bib`
- uses `springer-basicVL.csl` to format the references

Note that some of the outputs have been precomputed to speed up the report generation.
For example, `code/data_USA.R` loads and compiles most of the US data, then saves the data in the folder `dataderived/`;
`code/model_USA.R` runs heavy [random forest] models and even some plots, then saves the outputs in the folder `dataderived/`.
