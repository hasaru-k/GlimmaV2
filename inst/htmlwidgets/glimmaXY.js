HTMLWidgets.widget({

  name: 'glimmaXY',

  type: 'output',

  factory: function(el, width, height) 
  {

    var plotContainer = document.createElement("div");
    var controlContainer = document.createElement("div");
    plotContainer.setAttribute("class", "plotContainer");
    controlContainer.setAttribute("class", "controlContainer");

    var widget = document.getElementById(el.id);
    widget.appendChild(plotContainer);
    widget.appendChild(controlContainer);

    return {

      renderValue: function(x) 
      {
        
        console.log(x);
        var handler = new vegaTooltip.Handler();

        // create container elements
        var xyContainer = document.createElement("div");
        xyContainer.setAttribute("class", "xyContainerSingle");
        plotContainer.appendChild(xyContainer);

        var xyTable = HTMLWidgets.dataframeToD3(x.data.table)
        var xySpec = createXYSpec(x.data, xyTable, width, height);
        var xyView = new vega.View(vega.parse(xySpec), {
          renderer: 'svg',
          container: xyContainer,
          bind: controlContainer,
          hover: true
        });
        xyView.tooltip(handler.call);
        xyView.runAsync();

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

        var data =
        {
          xyView: xyView,
          expressionView: expressionView,
          xyTable: xyTable,
          countsMatrix: countsMatrix,
          controlContainer: controlContainer,
          height: height,
          cols: x.data.cols,
          groups: x.data.groups
        };

        setupXYInteraction(data);
        addSavePlotButton(controlContainer, xyView, text="Save (XY)");
        if (expressionView)
        {
          addSavePlotButton(controlContainer, expressionView, text="Save (EXP)");
        }

      },

      resize: function(width, height) 
      {}

    };
  }
});


function setupXYInteraction(data)
{
  var state = {selected: [], graphMode: false };
  var datatableEl = document.createElement("TABLE");
  datatableEl.setAttribute("class", "dataTable");
  data.controlContainer.appendChild(datatableEl);

  $(document).ready(function() 
  {
    var datatable = $(datatableEl).DataTable(
      {
        data: data.xyTable,
        columns: data.cols.map(el => ({"data": el, "title": el})),
        rowId: "gene",
        dom: '<"geneDisplay">Bfrtip',
        buttons: [  
                    {
                      text: 'Clear',
                      action: () => clearTableListener(datatable, state, data),
                      attr: {class: 'save-button'}
                    },
                    { 
                      text: 'Save (All)',
                      action: () => saveTableClickListener(state, data),
                      attr: {class: 'save-button saveSubset'}
                    }
                  ],
        scrollY: (data.height*0.4).toString() + "px",
        scrollX: false,
        orderClasses: false,
        stripeClasses: ['stripe1','stripe2']
      });

    datatable.on('click', 'tr', function() { tableClickListener(datatable, state, data, $(this)) } );
    data.xyView.addSignalListener('click', function(name, value) { XYSignalListener(datatable, state, value[0], data) } );
  });
}

function clearTableListener(datatable, state, data)
{
  state.graphMode = false;
  state.selected = [];
  selectedUpdateHandler(state, data.controlContainer);
  datatable.rows('.selected').nodes().to$().removeClass('selected');
  datatable.search('').columns().search('').draw();       
  data.xyView.data("selected_points", state.selected);
  data.xyView.runAsync();
  clearExpressionPlot(data.expressionView);
  console.log(state);
}


function tableClickListener(datatable, state, data, row)
{
  if (state.graphMode)
  {
    return;
  } 
  row.toggleClass('selected');
  let selectedRows = datatable.rows('.selected').data();
  state.selected = [];
  for (let i = 0; i < selectedRows.length; i++)
  {
    state.selected.push(selectedRows[i]);
  } 
  data.xyView.data("selected_points", state.selected);
  data.xyView.runAsync();
  selectedUpdateHandler(state, data.controlContainer);

  let index = datatable.row(row).index();
  let selectEvent = row.hasClass('selected');
  expressionUpdateHandler(data, selectEvent, state, data.xyTable[index]);
  console.log(state);
}


function XYSignalListener(datatable, state, datum, data)
{
  if (datum == null) return;
  if (!state.graphMode)
  {
    state.graphMode = true;
    datatable.rows('.selected').nodes().to$().removeClass('selected');
    state.selected = [];
  }

  var loc = containsGene(state.selected, datum);
  state.selected = loc >= 0 ? remove(state.selected, loc) : state.selected.concat(datum);
  selectedUpdateHandler(state, data.controlContainer);
  data.xyView.data("selected_points", state.selected);
  data.xyView.runAsync();

  // edge case: deselecting last point
  if (state.selected.length == 0)
  {
    state.graphMode = false;
  }

  datatable.search('').columns().search('').draw();
  var regex_search = state.selected.map(x => '^' + x.gene + '$').join('|');
  datatable.columns(0).search(regex_search, regex=true, smart=false).draw();

  let selectEvent = loc < 0;
  expressionUpdateHandler(data, selectEvent, state, datum);
}


function expressionUpdateHandler(data, selectEvent, state, xyRow)
{
  if (!data.expressionView)
  {
    return;
  }
  if (selectEvent)
  {
    let countsRow = data.countsMatrix[xyRow.index];
    updateExpressionPlot(countsRow, data, xyRow.gene);
  }
  /* if we deselected the point, check if anything else is selected */
  else
  {
    if (state.selected.length > 0)
    {
      let last = state.selected[state.selected.length-1];
      let countsRow = data.countsMatrix[last.index];
      updateExpressionPlot(countsRow, data, last.gene);
    }
    else
    {
      clearExpressionPlot(data.expressionView);
    }
  }
}


function clearExpressionPlot(expressionView)
{
  if (!expressionView)
  {
    return;
  }
  expressionView.data("table", []);
  expressionView.signal("title_signal", "");
  expressionView.runAsync();
}


function updateExpressionPlot(countsRow, data, gene)
{
  let groups = data.groups.group;
  let samples = data.groups.sample;
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
  data.expressionView.data("table", result);
  data.expressionView.signal("title_signal", "Gene " + gene.toString());
  data.expressionView.runAsync();
}


function containsGene(arr, datum)
{
  let loc = -1;
  let i;
  for (i = 0; i < arr.length; i++)
  {
    if (arr[i]['gene'] === datum['gene'])
    {
      loc = i;
      break;
    } 
  }
  return loc;
}


function selectedUpdateHandler(state, controlContainer)
{
  /* update gene display */
  var geneDisplay = controlContainer.getElementsByClassName("geneDisplay")[0];
  let htmlString = state.selected.map(x => `<span>${x.gene}</span>`).join("");
  $(geneDisplay).html(htmlString);

  /* update save btn */
  var saveSubsetButton = controlContainer.getElementsByClassName("saveSubset")[0];
  let saveString = state.selected.length > 0 ? `Save (${state.selected.length})` : "Save (All)";
  $(saveSubsetButton).html(saveString);
}


function remove(arr, index)
{
  let new_arr = arr.slice(0, index).concat(arr.slice(index+1))
  return new_arr;
}