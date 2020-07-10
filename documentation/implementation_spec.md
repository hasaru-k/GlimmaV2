# GlimmaV2 Spec
## Code Structure
```
.
├── DESCRIPTION
├── GlimmaV2.Rproj
├── NAMESPACE
├── R
│   ├── glimmaMDS.R----------------------------> main R data processing file for glimmaMDS()
│   └── glimmaXY.R-----------------------------> main R data processing file for glimmaXY(), glimmaMA()
├── README.md
└── inst
    └── htmlwidgets
        ├── glimmaMDS.js-----------------------> main frontend interface for glimmaMDS()
        ├── glimmaMDS.yaml---------------------> project dependencies for glimmaMDS()
        ├── glimmaXY.js------------------------> main frontend interface for glimmaMA(), glimmaXY()
        ├── glimmaXY.yaml----------------------> project dependencies for glimmaMA(), glimmaXY()
        └── lib
            ├── GlimmaV2
            │   ├── MDSSpecs.js----------------> contains createMDSSpec(), createEigenSpec()
            │   ├── XYSpecs.js-----------------> contains createXYSpec()
            │   └── saveVega.js----------------> contains addSave()
            ├── vega
            │   ├── vega.js--------------------> main vega library
            │   ├── vega.min.js
            │   └── vega_plots.css-------------> main stylesheet; consider separating out
            └── vega-tooltip-------------------> tooltip library
                ├── vega-tooltip.css
                └── vega-tooltip.js
```
## HTML Layout
GlimmaV2 is rendered within a parent widget HTML element in which it spawns the two different hierarchies given below, depending on which plot is being displayed. Both the XY plot and the MDS plot share a general structure to improve modularity: a plotContainer sandboxes the rendered Vega plots, and a controlContainer contains interactive components such as dropdowns, tables and save buttons which apply changes to the plot. The main differrence in structure between the MDS and XY layout is that the MDS plot lacks the datatable.

### XY/MA Plot
![XY plot](xy_layout.png "MDS Plot")
- the controlContainer is dominated by the datatable
- the datatable itself contains controls for saving the table (CSV/Excel), resetting selections and searching the data
- CSS grid will be used to render xyView and expView plots on the same line to avoid another dependency on bootstrap
### MDS Plot
![MDS plot](mds_layout.png "MDS Plot")
- an empty block element (not shown) is used to structure the vega binds in controlContainer into separate lines
- simpler and much more lightweight than the XY plot without the datatable

## XY Plot Interaction
![interaction](interaction_statemach.png)
