HTMLWidgets.widget({

  name: 'glimmaMDS',

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
        var mdsContainer = document.createElement("div");
        var eigenContainer = document.createElement("div");
        mdsContainer.setAttribute("class", "mdsContainer");
        eigenContainer.setAttribute("class", "eigenContainer");

        plotContainer.appendChild(mdsContainer);
        plotContainer.appendChild(eigenContainer);

        processDataMDS(x);
        var mdsData = HTMLWidgets.dataframeToD3(x.data.mdsData);
        var eigenData = HTMLWidgets.dataframeToD3(x.data.eigenData);

        /* NB: the createXXSpec functions are defined in lib/GlimmaSpecs */
        var mdsSpec = createMDSSpec(mdsData, x.data.dimlist,
                                      x.data.features,
                                      width, height, x.data.continuousColour);

        var mdsView = new vega.View(vega.parse(mdsSpec), {
          renderer: 'svg',
          container: mdsContainer,
          bind: controlContainer,
          hover: true
        });

        mdsView.tooltip(handler.call);
        mdsView.runAsync();

        var eigenSpec = createEigenSpec(eigenData, width, height);
        eigenView = new vega.View(vega.parse(eigenSpec), {
          renderer: 'svg',
          container: eigenContainer,
          hover: true
        });
        eigenView.runAsync();
        linkPlotsMDS(mdsView, eigenView);

        addColourMessage(x.data, mdsView, controlContainer);

        addBlockElement(controlContainer);
        addSavePlotButton(controlContainer, mdsView, eigenView, text="Save Plot",
                          summaryText="MDS", expressionText="VAR");

        reformatElementsMDS(controlContainer);
      },

      resize: function(width, height)
      {}

    };
  }
});

function addBlockElement(controlContainer)
{
  var blockElement = document.createElement("DIV");
  blockElement.setAttribute("class", "display-block");
  controlContainer.appendChild(blockElement);
}


function processDataMDS(x)
{
  /* if there's only a single feature in an R vector,
    it does not become an array after data transformation to JS */
  if (!Array.isArray(x.data.features["numeric"]))
  {
    x.data.features["numeric"] = [ x.data.features["numeric"] ];
  }
  if (!Array.isArray(x.data.features["discrete"]))
  {
    x.data.features["discrete"] = [ x.data.features["discrete"] ];
  }
  // sort to get "-", " -" features loaded first
  x.data.features["numeric"].sort();
  x.data.features["discrete"].sort();
}

function linkPlotsMDS(mdsView, eigenView)
{

  // highlight variance plot when we change a signal in the MDS plot
  mdsView.addSignalListener('x_axis', function(name, value) {
    var externalSelectValue = parseInt(value.substring(3));
    eigenView.signal("external_select_x", externalSelectValue);
    eigenView.runAsync();
  });

  mdsView.addSignalListener('y_axis', function(name, value) {
    var externalSelectValue = parseInt(value.substring(3));
    eigenView.signal("external_select_y", externalSelectValue);
    eigenView.runAsync();
  });

}

/* arranges vega-bind elements so that x_dim, y_dim are on their
   own line */
function reformatElementsMDS(controlContainer)
{
  var binds = controlContainer.getElementsByClassName("vega-bind");
  for (var i = 0; i < binds.length; i++)
  {
    // the separator input signal is a dummy invisible signal after x_axis, y_axis
    if (i == 2) binds[i].className += " separator_signal";
    // it is used to display all signals after x_axis, y_axis to display on a different line
    else if (i > 2) binds[i].className += " signal";
  }
}

function addColourMessage(data, view, container)
{
  var alertBox = document.createElement("div");
  alertBox.setAttribute("class", "alertBox invisible");
  // update the warning box when colourscheme signal changes
  view.addSignalListener('colourscheme',
    function(name, value) { updateColourMessage(data, container, view, value) });
  // update warning box when the colour_by signal changes
  view.addSignalListener('colour_by',
    function(name, value) {
      updateColourMessage(data, container, view, view.signal('colourscheme'));
    });
  container.appendChild(alertBox);
}

function updateColourMessage(data, container, view, value)
{
  var alertBox = container.getElementsByClassName("alertBox")[0];
  let schemeCount = vega.scheme(value).length;
  let colourBy = view.signal("colour_by");
  let colourCount = [...new Set(data.mdsData[colourBy])].length;
  alertBox.setAttribute("class", "alertBox invisible");

  if (data.continuousColour) return;
  if (value == "plasma" || value == "viridis") return;
  if (colourBy == "-") return;

  if (schemeCount < colourCount) {
    alertBox.innerHTML = `Warning: not enough distinct colours. ${colourCount} supported.`;

    alertBox.setAttribute("class", "alertBox warning");
  } else {
    alertBox.setAttribute("class", "alertBox invisible");
  }
}
