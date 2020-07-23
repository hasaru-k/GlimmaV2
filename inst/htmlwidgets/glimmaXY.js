HTMLWidgets.widget({

  name: 'glimmaXY',

  type: 'output',

  factory: function(el, width, height) {

    // create general layout elements
    var plotContainer = document.createElement("div");
    var controlContainer = document.createElement("div");
    plotContainer.setAttribute("class", "plotContainer");
    controlContainer.setAttribute("class", "controlContainer");

    var widget = document.getElementById(el.id);
    widget.appendChild(plotContainer);
    widget.appendChild(controlContainer);

    return {

      renderValue: function(x) {
        
        console.log(x);
        var handler = new vegaTooltip.Handler();

        // create container elements
        var xyContainer = document.createElement("div");
        xyContainer.setAttribute("class", "xyContainerSingle");
        plotContainer.appendChild(xyContainer);

        var xyTable = HTMLWidgets.dataframeToD3(x.data.table)
        var xySpec = createXYSpec(x.data, xyTable, width, height);
        xyView = new vega.View(vega.parse(xySpec), {
          renderer: 'svg',
          container: xyContainer,
          bind: controlContainer,
          hover: true
        });
        xyView.tooltip(handler.call);
        xyView.runAsync();



        // add expression plot if necessary
        var countsMatrix = null;
        var expressionView = null;
        if (x.data.counts != -1)
        {
          var expressionContainer = document.createElement("div");
          expressionContainer.setAttribute("class", "expressionContainer");
          plotContainer.appendChild(expressionContainer);
          xyContainer.setAttribute("class", "xyContainer");
          countsMatrix = HTMLWidgets.dataframeToD3(x.data.counts);
          var expressionSpec = createExpressionSpec(width, height, x.data.expCols);
          var expressionView = new vega.View(vega.parse(expressionSpec), {
            renderer: 'canvas',
            container: expressionContainer,
            hover: true
          });
          expressionView.tooltip(handler.call);
          expressionView.runAsync();

        }
        
        // add datatable, and generate interaction
        setupXYInteraction(xyView, xyTable, countsMatrix, expressionView, controlContainer, x, height);
        // add XY plot save button
        addSave(controlContainer, xyView, text="Save (XY)");
        if (expressionView) addSave(controlContainer, expressionView, text="Save (EXP)");

      },

      resize: function(width, height) 
      {
        console.log("resize called, width=" + width + ",height=" + height);
      }

    };
  }
});


function setupXYInteraction(xyView, xyTable, countsMatrix, expressionView, controlContainer, x, height)
{
  // setup the datatable
  var datatableEl = document.createElement("TABLE");
  datatableEl.setAttribute("class", "dataTable");
  controlContainer.appendChild(datatableEl);
  
  var selected = [];
  var graphMode = false;

  $(document).ready(function() 
  {

    var datatable = $(datatableEl).DataTable({
        data: xyTable,
        columns: x.data.cols.map(el => ({"data": el, "title": el})),
        rowId: "gene",
        dom: '<"geneDisplay">Bfrtip',
        buttons: [  
                    { 
                      action: () => saveSubsetClick(selected, xyTable, countsMatrix),
                      text: 'Save (All)',
                      attr: {class: 'save-button saveSubset'}
                    }
                  ],
        scrollY: (height*0.4).toString() + "px",
        scrollX: false,
        orderClasses: false,
        stripeClasses: ['stripe1','stripe2']
    });

    // reset graph and table selections
    datatable.button().add(0, 
      {
        action: function ( e, dt, button, config ) 
        {
          graphMode = false;
          console.log(datatable.rows())
          /* clear datatable rows and search filter */
          datatable.rows('.selected').nodes().to$().removeClass('selected');
          datatable.search('').columns().search('').draw();                      
          selected = [];
          selectedUpdateHandler(selected, controlContainer);
          /* clear XY plot */
          xyView.data("selected_points", selected);
          xyView.runAsync();
          /* clear expression plot */
          clearExpressionPlot(expressionView);
        },
        text: 'Clear',
        attr: {class: 'save-button'}
      });

    // map table selections onto the graph (clearing graph selections each time)
    datatable.on( 'click', 'tr', function () 
      {
        // not possible while in graph mode
        if (graphMode) return;
        $(this).toggleClass('selected');
        let selectedRows = datatable.rows('.selected').data();
        let i;
        selected = [];
        for (i = 0; i < selectedRows.length; i++) selected.push(selectedRows[i]);
        xyView.data("selected_points", selected);
        xyView.runAsync();
        selectedUpdateHandler(selected, controlContainer);

        /* expression plot */
        let index = datatable.row(this).index();
        let selectEvent = $(this).hasClass('selected');
        expressionUpdateHandler(expressionView, countsMatrix, x, selectEvent, selected, xyTable[index]);
      }
    );

    // map graph selections onto the table (clearing table selections each time)
    xyView.addSignalListener('click', 
      function(name, value) 
      {
        var datum = value[0];

        if (!graphMode)
        {
          graphMode = true;
          datatable.rows('.selected').nodes().to$().removeClass('selected');
          selected = [];
        }

        var loc = containsGene(selected, datum);
        selected = loc >= 0 ? remove(selected, loc) : selected.concat(datum);
        selectedUpdateHandler(selected, controlContainer);
        xyView.data("selected_points", selected);
        xyView.runAsync();

        // edge case: deselecting last point
        if (selected.length == 0)
        {
          graphMode = false;
        }

        datatable.search('').columns().search('').draw();
        var regex_search = selected.map(x => '^' + x.gene + '$').join('|');
        datatable.columns(0).search(regex_search, regex=true, smart=false).draw();

        let selectEvent = loc < 0;
        expressionUpdateHandler(expressionView, countsMatrix, x, selectEvent, selected, datum);
      }

    );
    
  });

}

function expressionUpdateHandler(expressionView, countsMatrix, x, selectEvent, selectedPoints, xyRow)
{
  if (!expressionView) return;
  if (selectEvent)
  {
    let countsRow = countsMatrix[xyRow.index];
    processExpression(countsRow, x.data.groups, expressionView, xyRow.gene);
  }
  /* if we deselected the point, check if anything else is selected */
  else
  {
    if (selectedPoints.length > 0)
    {
      let last = selectedPoints[selectedPoints.length-1];
      let countsRow = countsMatrix[last.index];
      processExpression(countsRow, x.data.groups, expressionView, last.gene);
    }
    else
    {
      clearExpressionPlot(expressionView);
    }
  }
}

function clearExpressionPlot(expressionView)
{
  if (!expressionView) return;
  expressionView.data("table", []);
  expressionView.signal("title_signal", "");
  expressionView.runAsync();
}

function processExpression(countsRow, groupsData, expressionView, gene)
{
  console.log(groupsData);
  let groups = groupsData.group;
  let samples = groupsData.sample;
  let result = [];
  for (col in countsRow) 
  {
    if (!samples.includes(col)) continue;
    let curr = {};
    let group = groups[samples.indexOf(col)];
    curr["group"] = group;
    curr["sample"] = col;
    curr["count"] = countsRow[col];
    result.push(curr);
  }
  console.log(result);
  expressionView.data("table", result);
  expressionView.signal("title_signal", "Gene " + gene.toString());
  expressionView.runAsync();
}

function containsGene(arr, datum)
{
  let loc = -1;
  let i;
  for (i = 0; i < arr.length; i++)
  {
    if (arr[i]['gene'] === datum['gene']) loc = i;
  }
  return loc;
}


function selectedUpdateHandler(selected, controlContainer)
{
  /* update gene display */
  var geneDisplay = controlContainer.getElementsByClassName("geneDisplay")[0];
  let htmlString = selected.map(x => `<span>${x.gene}</span>`).join("");
  $(geneDisplay).html(htmlString);

  /* update save btn */
  var saveSubsetButton = controlContainer.getElementsByClassName("saveSubset")[0];
  let saveString = selected.length > 0 ? `Save (${selected.length})` : "Save (All)";
  $(saveSubsetButton).html(saveString);
}

function remove(arr, index)
{
  let new_arr = arr.slice(0, index).concat(arr.slice(index+1))
  return new_arr;
}