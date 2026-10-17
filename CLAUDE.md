This is an R package to assist epidemiologists with writing phenotype reports from the information in a Strategus (github.com/ohdsi/strategus) results folder. 

Follow all coding style for R and SQL: https://ohdsi.github.io/Hades/codeStyle.html

We will start simple, building functions to summarize one module at a time. 

When planning, ask me questions (one question at a time) about my intent until we come to a shared understanding. Tell me your suggestions also. 

The first step will be building a function to simulate a Strategus results folder, then another function to write it to disk. 

After that we will implement functions for database information, cohort counts, and then other Strategus modules. 

This package will ultimately be submitted to CRAN, so let's try to follow their policies. 
