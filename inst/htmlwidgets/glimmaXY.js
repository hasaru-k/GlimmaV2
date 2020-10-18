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
        var expressionContainer = null;
        if (x.data.counts != -1)
        {
          expressionContainer = document.createElement("div");
          expressionContainer.setAttribute("class", "expressionContainer");
          plotContainer.appendChild(expressionContainer);
          xyContainer.setAttribute("class", "xyContainer");
          countsMatrix = HTMLWidgets.dataframeToD3(x.data.counts);
          var expressionSpec = createExpressionSpec(width, height, x.data.expCols, x.data.sampleColours, x.data.samples);
          var expressionView = new vega.View(vega.parse(expressionSpec), {
            renderer: 'svg',
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
          groups: x.data.groups,
          levels: x.data.levels,
          expressionContainer: expressionContainer
        };

        setupXYInteraction(data);
        addSavePlotButton(controlContainer, xyView, expressionView, "Save Plot");
        if (expressionView) {
          addAxisMessage(data);
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
        buttons: {
          dom: {
            buttonContainer: {
              tag: 'div',
              className: 'buttonContainer'
            }
          },
          buttons: [
                    {
                      text: 'Clear (0)',
                      action: () => clearTableListener(datatable, state, data),
                      attr: {class: 'save-button clearSubset'}
                    },
                    { 
                      text: 'Save Data',
                      action: () => showDataDropdown(),
                      attr: {class: 'save-button saveSubset'}
                    }
                  ]
                },
        scrollY: (data.height*0.4).toString() + "px",
        scrollX: false,
        orderClasses: false,
        stripeClasses: ['stripe1','stripe2']
      });

    datatable.on('click', 'tr', function() { tableClickListener(datatable, state, data, $(this)) } );
    data.xyView.addSignalListener('click', function(name, value) { XYSignalListener(datatable, state, value[0], data) } );

    $(document.getElementsByClassName("saveSubset")[0]).html(`Save Data`);
    addSaveDataElement(state, data, `Save All`, `Save (0)`);
  });
}


function showDataDropdown() {
  let dataDropdown = document.getElementsByClassName("dataDropdown")[0];
  dropdownOnClick(dataDropdown);
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
  clearExpressionPlot(data);
  console.log(state);
}


function tableClickListener(datatable, state, data, row)
{
  if (state.graphMode)
  {
    return;
  } 
  row.toggleClass('selected');
  let datum = datatable.row(row).data();
  var loc = containsGene(state.selected, datum);
  state.selected = loc >= 0 ? remove(state.selected, loc) : state.selected.concat(datum);
  selectedUpdateHandler(state, data.controlContainer);
  data.xyView.data("selected_points", state.selected);
  data.xyView.runAsync();
  let selectEvent = row.hasClass('selected');
  expressionUpdateHandler(data, selectEvent, state, datum);
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
      clearExpressionPlot(data);
    }
  }
}


function clearExpressionPlot(data)
{
  if (!data.expressionView)
  {
    return;
  }
  data.expressionView.data("table", []);
  data.expressionView.signal("title_signal", "");
  data.expressionView.signal("max_count", 0);
  data.expressionView.runAsync();
  updateAxisMessage(data);
}


function updateExpressionPlot(countsRow, data, gene)
{
  let groups = data.groups.group;
  let samples = data.groups.sample;
  let levels = data.levels;
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
  if (levels != null) {
    result.sort((a, b) => levels.indexOf(a.group) - levels.indexOf(b.group));
  }
  data.expressionView.data("table", result);
  data.expressionView.signal("title_signal", "Gene " + gene.toString());
  data.expressionView.signal("max_count", Math.max(...result.map(x => x.count)));
  data.expressionView.runAsync();
  updateAxisMessage(data);
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
  var saveSubsetButton = controlContainer.getElementsByClassName("saveSelectButton")[0];
  $(saveSubsetButton).html(`Save (${state.selected.length})`);

  /* update clear btn */
  var clearSubsetButton = controlContainer.getElementsByClassName("clearSubset")[0];
  $(clearSubsetButton).html(`Clear (${state.selected.length})`);
}

function remove(arr, index)
{
  let new_arr = arr.slice(0, index).concat(arr.slice(index+1))
  return new_arr;
}


function addAxisMessage(data)
{
  var bindings = data.expressionContainer.getElementsByClassName("vega-bindings")[0];
  var alertBox = document.createElement("div");
  alertBox.setAttribute("class", "alertBox invisible");
  data.expressionView.addSignalListener('max_y_axis', 
    function(name, value) { updateAxisMessage(data) });
  bindings.appendChild(alertBox);
}


function updateAxisMessage(data)
{
  var alertBox = data.expressionContainer.getElementsByClassName("alertBox")[0];
  let maxCount = data.expressionView.signal("max_count");
  let userValue = data.expressionView.signal("max_y_axis");
  if (userValue == null || userValue == "" || Number(userValue) >= maxCount)
  {
    alertBox.setAttribute("class", "alertBox invisible");
  }
  else
  {
    alertBox.innerHTML = `Max count value is ${maxCount}`;
    alertBox.setAttribute("class", "alertBox danger");
  }
}
