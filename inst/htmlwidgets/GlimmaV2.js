HTMLWidgets.widget({

  name: 'GlimmaV2',

  type: 'output',

  factory: function(el, width, height) {

    // create general layout elements
    var plotContainer = document.createElement("div");
    var controlContainer = document.createElement("div");
    plotContainer.setAttribute("id", "plotContainer");
    controlContainer.setAttribute("id", "controlContainer");

    var widget = document.getElementById(el.id);
    widget.appendChild(plotContainer);
    widget.appendChild(controlContainer);

    return {

      renderValue: function(x) {
        
        var handler = new vegaTooltip.Handler();

        if (x.plotType === "MDS")
        {
          // create tooltip handler

          // create container elements
          var mdsContainer = document.createElement("div");
          var eigenContainer = document.createElement("div");
          mdsContainer.setAttribute("id", "mdsContainer");
          eigenContainer.setAttribute("id", "eigenContainer");
          
          plotContainer.appendChild(mdsContainer);
          plotContainer.appendChild(eigenContainer);

          console.log(x);
          processDataMDS(x);
          var mdsData = HTMLWidgets.dataframeToD3(x.data.mdsData);
          var eigenData = HTMLWidgets.dataframeToD3(x.data.eigenData);
          
          /* NB: the createXXSpec functions are defined in lib/GlimmaSpecs */
          var mdsSpec = createMDSSpec(mdsData, x.data.dimlist, 
                                        x.data.features,
                                        width, height, x.data.continuous_colour);
          
          mdsView = new vega.View(vega.parse(mdsSpec), {
            renderer: 'canvas',
            container: '#' + mdsContainer.getAttribute("id"),
            bind: '#' + controlContainer.getAttribute("id"),
            hover: true
          });
  
          mdsView.tooltip(handler.call);
          mdsView.runAsync();
  
          var eigenSpec = createEigenSpec(eigenData, width, height);
          eigenView = new vega.View(vega.parse(eigenSpec), {
            renderer: 'canvas',
            container: '#' + eigenContainer.getAttribute("id"),
            hover: true
          });
          eigenView.runAsync();
          linkPlotsMDS();
          addBlockElement(controlContainer);
          addSave(controlContainer, mdsView, text="Save MDS");
          addSave(controlContainer, eigenView, text="Save VAR");

          reformatElementsMDS();
        }

        if (x.plotType === "XY")
        {
          console.log("XY plot created.");
          console.log(x);

          // create container elements
          var xyContainer = document.createElement("div");
          xyContainer.setAttribute("id", "xyContainer");
          plotContainer.appendChild(xyContainer);
          var xyData = HTMLWidgets.dataframeToD3(x.data.table);
          console.log(xyData);
          var xySpec = createXYSpec(xyData, width, height, 
                                      x.data.x, x.data.y, x.data.cols,
                                      x.data.status_colours, x.data.title);
          xyView = new vega.View(vega.parse(xySpec), {
            renderer: 'canvas',
            container: '#' + xyContainer.getAttribute("id"),
            bind: '#' + controlContainer.getAttribute("id"),
            hover: true
          });
          xyView.tooltip(handler.call);
          xyView.runAsync();

          // add datatable, and generate interaction
          setupXYInteraction(xyView, xyData, controlContainer, x);
          
          // add XY plot save button
          addSave(controlContainer, xyView);

        }

      },

      resize: function(width, height) 
      {
        console.log("resize called, width=" + width + ",height=" + height);
      }

    };
  }
});

function addBlockElement(controlContainer)
{
  var blockElement = document.createElement("DIV");
  blockElement.setAttribute("class", "display-block");
  controlContainer.appendChild(blockElement);
}

function setupXYInteraction(xyView, xyData, widget, x)
{
  // setup the datatable
  var datatableEl = document.createElement("TABLE");
  datatableEl.setAttribute("id", "dataTable");
  datatableEl.setAttribute("class", "dataTable");
  widget.appendChild(datatableEl);
  var xyColumnsInfo = [];
  x.data.cols.forEach(x => xyColumnsInfo.push({"data": x, "title": x}));

  graphMode = false;
  $(document).ready(function() 
  {

    datatable = $("#" + datatableEl.getAttribute("id")).DataTable({
        data: xyData,
        columns: xyColumnsInfo,
        rowId: "index",
        dom: 'Bfrtip',
        buttons: ['csv', 'excel'],
        scrollY:        "180px",
        scrollX:        false,
        orderClasses: false,
        'stripeClasses':['stripe1','stripe2']
    });
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
    $("#" + datatableEl.getAttribute("id") + ' tbody').on( 'click', 'tr', function () 
      {
        if (graphMode) return;
        $(this).toggleClass('selected');
        let selected_rows = datatable.rows('.selected').data();
        let i;
        selected = [];
        for (i = 0; i < selected_rows.length; i++) selected.push(selected_rows[i]);
        xyView.data("selected_points", selected);
        xyView.runAsync();
      }
    );
    
  });

  selected = [];
  // map graph selections onto the table (clearing table selections each time)
  xyView.addSignalListener('click', function(name, value) {
    var datum = value[0];
    if (datum == null) return;
    if (!graphMode)
    {
      graphMode = true;
      datatable.rows('.selected').nodes().to$().removeClass('selected');
      selected = [];
    }
    // check if datum is in selected
    // if it is, remove it; otherwise, add it
    var loc = contains(selected, datum);
    console.log(loc);
    loc >= 0 ?
      selected = selected.slice(0, loc).concat(selected.slice(loc+1)) 
      : selected.push(datum);
    xyView.data("selected_points", selected);
    xyView.runAsync();
    
    // filter table
    if (!datatable) return;
    datatable.search('').columns().search('').draw();
    // search using a regex string: union over indices in selected
    var regex_search = selected.map(x => '^' + x["index"] + '$').join('|');
    datatable.columns(0).search(regex_search, regex=true, smart=false).draw();

  });

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

function linkPlotsMDS()
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

function addSave(controlContainer, view_obj, text="Save Plot")
{
  // set up button elements
  var dropdownDiv = document.createElement("div");
  dropdownDiv.setAttribute("class", "dropdown");

  var dropdownButton = document.createElement("button");
  dropdownButton.setAttribute("class", "save-button");
  dropdownButton.innerHTML = text;

  var dropdownContent = document.createElement("div");
  dropdownContent.setAttribute("class", "dropdown-content");
  dropdownContent.setAttribute("id", "dropdown-content");

  var pngSaveBtn = document.createElement("a");
  pngSaveBtn.setAttribute("href", "#")
  pngSaveBtn.innerText = "PNG";
  pngSaveBtn.onclick = function() {
    view_obj.toImageURL('png', scaleFactor=3).then(function (url) {
      var link = document.createElement('a');
      link.setAttribute('href', url);
      link.setAttribute('target', '_blank');
      link.setAttribute('download', 'vega-export.png');
      link.dispatchEvent(new MouseEvent('click'));
    });
  };
  
  var svgSaveBtn = document.createElement("a");
  svgSaveBtn.setAttribute("href", "#");
  svgSaveBtn.innerText = "SVG";
  svgSaveBtn.onclick = function() {
    view_obj.toImageURL('svg', scaleFactor=3).then(function (url) {
      var link = document.createElement('a');
      link.setAttribute('href', url);
      link.setAttribute('target', '_blank');
      link.setAttribute('download', 'vega-export.svg');
      link.dispatchEvent(new MouseEvent('click'));
    });
  };

  // add elements to container
  dropdownDiv.appendChild(dropdownButton);
  dropdownDiv.appendChild(dropdownContent);

  dropdownContent.appendChild(pngSaveBtn);
  dropdownContent.appendChild(svgSaveBtn);

  // set up dropdown action
  dropdownButton.onclick = function() {
      var dropdowns = document.getElementsByClassName("dropdown-content");
      for (const dropdown_i of dropdowns) {
        if (dropdown_i.classList.contains("show")) {
          dropdown_i.classList.remove("show");
        }
      }
    dropdownContent.classList.toggle("show");
  };

  controlContainer.appendChild(dropdownDiv);

  // set up dropdown hide when clicking elsewhere
  // global window.dropdownHide so this event is only added once
  if (!window.dropdownHide) {
    function hideDropdowns(event) {
      if (!event.target.matches(".save-button")) {
        var dropdowns = document.getElementsByClassName("dropdown-content");

        for (const dropdown_i of dropdowns) {
          if (dropdown_i.classList.contains("show")) {
            dropdown_i.classList.remove("show");
          }
        }
      }
    }

    window.addEventListener("click", hideDropdowns);

    window.dropdownHide = true;
  }
}

function addSVGSave(controlContainer, view_obj, text="Save (SVG)")
{
  var downloadButton = document.createElement("BUTTON");
  downloadButton.setAttribute("class", "save-button");
  downloadButton.innerHTML = text;
  downloadButton.onclick =
  function changeContent() {
    view_obj.toImageURL('svg', scaleFactor=3).then(function (url) {
      var link = document.createElement('a');
      link.setAttribute('href', url);
      link.setAttribute('target', '_blank');
      link.setAttribute('download', 'vega-export.svg');
      link.dispatchEvent(new MouseEvent('click'));
    }).catch(function (error) { /* error handling */ });
  }
  controlContainer.appendChild(downloadButton);
}

function reformatElementsMDS(controlContainer)
{
  binds = document.getElementsByClassName("vega-bind");
  for (var i = 0; i < binds.length; i++)
  {
    // the separator input signal is a dummy invisible signal after x_axis, y_axis
    if (i == 2) binds[i].className += " separator_signal";
    // it is used to display all signals after x_axis, y_axis to display on a different line
    else if (i > 2) binds[i].className += " signal";
  }
}