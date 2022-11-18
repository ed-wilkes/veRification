guideImprecision <- function() {
  HTML(
    "<b>Data input tab:</b><br>

        <p style='margin-left: 20px'>
        <b>Data input:</b><br>
       - Using the <b>[Browse]</b> button, find and select the .csv/.xls/.xlsx file containing your data<br>
       - You can analyse multiple levels of QC within the same analysis<br>
       - Indicate with the checkbox whether the first row of your data contains the column names<br>
       - Enter the name of your analyte (this can be left blank if desired)<br>
       - <b>NB:</b> you can collapse any of the boxes within the <b>[-]</b> button in the top right of each box<br>
       <br>

       <b>Your data:</b><br>
       - You can view and search through your data once it has loaded<br>
       - Click on any rows which you wish to exclude from further analysis (this can be reversed by clicking on them again)<br>
       <br>

       <b>Column selection:</b><br>
       - Select the columns within your data as indicated<br>
       - Tick the checkbox if you wish to test your assay's imprecision against the manufacturer's claims<br>
       <br>

       <b>Manufacturer's claims:</b><br>
       - This box will only appear if the manufacturer's claim checkbox has been ticked
       - Input the manufacturer's total CV claim for each level of QC within your data<br>
       </p>

    <b>Plots tab:</b><br>
    - You can view the data for each level of QC within the <b>[Plots]</b> tab<br>
    - You can interact with the plots (hover, zoom, export, etc.) using the mouse<br>
    - You can export the plots as .png image files by clicking on the camera icon<br>
    <br>

    <b>Summary statistics tab:</b><br>
    - You can view the Bayesian modelling results within the <b>[Summary statistics]</b> tab<br>
    - A separate model is fitted to each level of QC present within the data<br>
    - The repeatability, intermediate precision, and total laboratory CV (%) are provided for each QC level<br>
    - If present, the posterior distribution of the total CV is tested against the manufacturer's claim with the given credible interval<br>
    <br>

    <b>Bayesian model results tab:</b><br>
    - The MCMC traces and full posterior distributions for each model's parameters can be viewed under the <b>[Bayesian model results]</b> tab<br>
    <br>
    "
  )
}
