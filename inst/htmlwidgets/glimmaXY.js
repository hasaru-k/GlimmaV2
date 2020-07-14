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
          renderer: 'canvas',
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
          countsMatrix = HTMLWidgets.dataframeToD3(x.data.counts)
          /* TODO: add expressionView located in expressionContainer */
          var expressionSpec = createExpressionSpec(width, height);
          var expressionView = new vega.View(vega.parse(expressionSpec), {
            renderer: 'canvas',
            container: expressionContainer,
            hover: true
          });
          expressionView.runAsync();
        }
        
        // add datatable, and generate interaction
        setupXYInteraction(xyView, xyTable, countsMatrix, expressionView, controlContainer, x);
        // add XY plot save button
        addSave(controlContainer, xyView);

      },

      resize: function(width, height) 
      {
        console.log("resize called, width=" + width + ",height=" + height);
      }

    };
  }
});



function setupXYInteraction(xyView, xyTable, countsMatrix, expressionView, widget, x)
{
  // setup the datatable
  var datatableEl = document.createElement("TABLE");
  datatableEl.setAttribute("class", "dataTable");
  widget.appendChild(datatableEl);
  var xyColumnsInfo = [];
  x.data.cols.forEach(x => xyColumnsInfo.push({"data": x, "title": x}));
  
  var graphMode = false;
  $(document).ready(function() 
  {

    var datatable = $(datatableEl).DataTable({
        data: xyTable,
        columns: xyColumnsInfo,
        rowId: "index",
        dom: 'Bfrtip',
        buttons: ['csv', 'excel'],
        scrollY:        "180px",
        scrollX:        false,
        orderClasses: false,
        'stripeClasses':['stripe1','stripe2']
    });

    var selected = [];

    // reset graph and table selections
    datatable.button().add(0, 
      {
        action: function ( e, dt, button, config ) 
        {
          graphMode = false;
          console.log(datatable.rows())
          datatable.rows('.selected').nodes().to$().removeClass('selected');
          datatable.search('').columns().search('').draw();                      
          selected = [];
          xyView.data("selected_points", selected);
          xyView.runAsync();
        },
        text: 'Reset'
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

        /* expression plot */
        let index = Number($(this).context.firstChild.innerHTML);
        let countsRow = countsMatrix[index];
        let selectEvent = $(this).hasClass('selected');
        expressionUpdateHandler(expressionView, countsMatrix, x, selectEvent, selected, countsRow);
      }
    );
    
    // map graph selections onto the table (clearing table selections each time)
    xyView.addSignalListener('click', 
      function(name, value) 
      {
        var datum = value[0];
        if (datum == null) return;

        // switch to graph mode if in table mode
        if (!graphMode)
        {
          graphMode = true;
          datatable.rows('.selected').nodes().to$().removeClass('selected');
          selected = [];
        }

        // check if datum is in selected
        // if it is, remove it; otherwise, add it
        var loc = contains(selected, datum);
        loc >= 0 ?
          selected = selected.slice(0, loc).concat(selected.slice(loc+1)) 
          : selected.push(datum);

        // highlight selected points
        xyView.data("selected_points", selected);
        xyView.runAsync();

        // edge case: deselect last point
        if (selected.length == 0) graphMode = false;

        // update table filter based on selected
        if (!datatable) return;
        datatable.search('').columns().search('').draw();
        // filter using a regex string: union over indices in selected
        var regex_search = selected.map(x => '^' + x["index"] + '$').join('|');
        datatable.columns(0).search(regex_search, regex=true, smart=false).draw();

        /* expression plot */
        let selectEvent = loc < 0;
        let countsRow = countsMatrix[datum.index];
        expressionUpdateHandler(expressionView, countsMatrix, x, selectEvent, selected, countsRow);

      }

    );
    
  });


}

function expressionUpdateHandler(expressionView, countsMatrix, x, selectEvent, selectedPoints, countsRow)
{
  if (!expressionView) return;
  if (selectEvent)
  {
    processExpression(countsRow, x.data.groups.group, x.data.groups.sample, expressionView);
  }
  /* if we deselected the point, check if anything else is selected */
  else
  {
    if (selectedPoints.length > 0)
    {
      let newIndex = selectedPoints[selectedPoints.length-1].index;
      processExpression(countsMatrix[newIndex], x.data.groups.group, x.data.groups.sample, expressionView);
    }
    else
    {
      expressionView.data("table", []);
      expressionView.signal("title_signal", "");
      expressionView.runAsync();
    }
  }
}

function processExpression(countsRow, groups, samples, expressionView)
{
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
  expressionView.signal("title_signal", "Gene " + countsRow.gene);
  expressionView.runAsync();
}

function contains(arr, datum)
{
  let loc = -1;
  let i;
  for (i = 0; i < arr.length; i++)
  {
    if (arr[i]['index'] === datum['index']) loc = i;
  }
  return loc;
}