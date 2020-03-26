HTMLWidgets.widget({

  name: 'GlimmaV2',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance
 
    return {

      renderValue: function(x) {

        console.log(x);
        
        var plotContainer = document.createElement("div");
        var mdsContainer = document.createElement("div");
        var eigenContainer = document.createElement("div");
        var controlContainer = document.createElement("div");
        mdsContainer.setAttribute("id", "mdsContainer");
        eigenContainer.setAttribute("id", "eigenContainer");
        plotContainer.setAttribute("id", "plotContainer");
        controlContainer.setAttribute("id", "controlContainer");

        var widget = document.getElementById(el.id);
        widget.appendChild(plotContainer);
        widget.appendChild(controlContainer);

        plotContainer.appendChild(mdsContainer);
        plotContainer.appendChild(eigenContainer);

        mdsData = HTMLWidgets.dataframeToD3(x.data.mdsData);
        eigenData = HTMLWidgets.dataframeToD3(x.data.eigenData);

        // TODO: extract this data from dataframe
        dimList = ["dim1", "dim2", "dim3", "dim4", "dim5", "dim6"];
        sampleList = ["lane", "genotype"];

        var mdsSpec = createMDSSpec(dimList, sampleList, mdsData, width, height);
        var mdsView = new vega.View(vega.parse(mdsSpec), {
          renderer: 'canvas',  // renderer (canvas or svg)
          container: '#' + mdsContainer.getAttribute("id"),
          bind: '#' + controlContainer.getAttribute("id"),
          hover: true       // enable hover processing
        });
        mdsView.runAsync();

        var eigenSpec = createEigenSpec(eigenData, width, height);
        console.log(eigenSpec);
        var eigenView = new vega.View(vega.parse(eigenSpec), {
          renderer: 'canvas',  // renderer (canvas or svg)
          container: '#' + eigenContainer.getAttribute("id"),
          hover: true       // enable hover processing
        });
        eigenView.runAsync();

        // save to PNG button for MDS plot
        var downloadButton = document.createElement("BUTTON");
        downloadButton.setAttribute("id", "savePNGBtn");
        downloadButton.innerHTML = "Save to PNG";
        downloadButton.onclick =
          function changeContent() {
            mdsView.toImageURL('png').then(function (url) {
              var link = document.createElement('a');
              link.setAttribute('href', url);
              link.setAttribute('target', '_blank');
              link.setAttribute('download', 'vega-export.png');
              link.dispatchEvent(new MouseEvent('click'));
            }).catch(function (error) { /* error handling */ });
          }
        controlContainer.appendChild(downloadButton);

        // modify the vega-bound control elements
        bindNames = document.getElementsByClassName("vega-bind-name");
        for (i = 0; i < bindNames.length; i++) {
          bindNames[i].innerHTML += ":";
        }
        // color_by input on next line
        binds = document.getElementsByClassName("vega-bind");
        binds[2].className += " display-block";


      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});

// parametrise graph encoding for MDS plot
function createMDSSpec(dimList, sampleList, mdsData, width, height) 
{
  return {
    "$schema": "https://vega.github.io/schema/vega/v5.json",
    "description": "Testing ground for GlimmaV2",
    "width": width * 0.5,
    "height": height * 0.6,
    "padding": 0,
    "signals":
      [
        {
          "name": "x_axis",
          "value": "dim1",
          "bind": { "input": "select", "options": dimList }
        },
        {
          "name": "y_axis",
          "value": "dim2",
          "bind": { "input": "select", "options": dimList }
        },
        {
          "name": "colour_by",
          "value": "lane",
          "bind": { "input": "select", "options": sampleList }
        },
      ],
    "data": 
      [
        {
          "name": "source",
          "values": mdsData,
          "transform": [{
            "type": "formula",
            "expr": "datum.x",
            "as": "tooltip"
          }]
        }
      ],
    "scales": [
      {
        "name": "x",
        "type": "linear",
        "round": true,
        "nice": true,
        "zero": true,
        "domain": { "data": "source", "field": { "signal": "x_axis" } },
        "range": "width"
      },
      {
        "name": "y",
        "type": "linear",
        "round": true,
        "nice": true,
        "zero": true,
        "domain": { "data": "source", "field": { "signal": "y_axis" } },
        "range": "height"
      },
      {
        "name": "size",
        "type": "linear",
        "round": true,
        "nice": false,
        "zero": true,
        "domain": { "data": "source", "field": "libsize" },
        "range": [4, 361]
      },
      {
        "name": "color",
        "type": "ordinal",
        "domain": { "data": "source", "field": { "signal": "colour_by" } },
        "range": { "scheme": "category10" }
      }
    ],

    "axes": [
      {
        "scale": "x",
        "grid": true,
        "domain": false,
        "orient": "bottom",
        "tickCount": 5,
        "title": { "signal": "x_axis" }
      },
      {
        "scale": "y",
        "grid": true,
        "domain": false,
        "orient": "left",
        "titlePadding": 5,
        "title": { "signal": "y_axis" }
      }
    ],

    "legends": [
      {
        "size": "size",
        "title": "Library Size",
        "format": "s",
        "symbolStrokeColor": "#4682b4",
        "symbolStrokeWidth": 2,
        "symbolOpacity": 0.7,
        "symbolType": "circle"
      },
      {
        "fill": "color",
        "title": "Colour",
        "symbolStrokeColor": "#4682b4",
        "symbolStrokeWidth": 2,
        "symbolOpacity": 0.7,
        "symbolType": "circle"
      }
    ],

    "marks": [
      {
        "name": "marks",
        "type": "symbol",
        "from": { "data": "source" },
        "enter": {
          "tooltip": { "field": "tooltip" }
        },
        "encode": {
          "update": {
            "x": { "scale": "x", "field": { "signal": "x_axis" } },
            "y": { "scale": "y", "field": { "signal": "y_axis" } },
            "size": { "scale": "size", "field": "libsize" },
            "shape": { "value": "circle" },
            "strokeWidth": { "value": 2 },
            "opacity": { "value": 0.7 },
            "stroke": { "value": "#4682b4" },
            "fill": { "scale": "color", "field": { "signal": "colour_by" } },
            "tooltip": { "field": "tooltip" }
          }
        }
      }
    ]
  };
}

// parametrise graph encoding for variance plot
function createEigenSpec(eigenData, width, height) 
{
  return {
    "$schema": "https://vega.github.io/schema/vega/v5.json",
    "description": "A basic bar chart example, with value labels shown upon mouse hover.",
    "width": width * 0.5,
    "height": height * 0.6,
    "padding": 5,
  
    "data": [
      {
        "name": "table",
        "values": eigenData
      }
    ],
  
    "signals": [
      {
        "name": "tooltip",
        "value": {},
        "on": [
          {"events": "rect:mouseover", "update": "datum"},
          {"events": "rect:mouseout",  "update": "{}"}
        ]
      }
    ],
  
    "scales": [
      {
        "name": "xscale",
        "type": "band",
        "domain": {"data": "table", "field": "name"},
        "range": "width",
        "padding": 0.05,
        "round": true
      },
      {
        "name": "yscale",
        "domain": {"data": "table", "field": "eigen"},
        "range": "height"
      }
    ],
  
    "axes": [
      { "orient": "bottom", "scale": "xscale" },
      { "orient": "left", "scale": "yscale" }
    ],
  
    "marks": [
      {
        "type": "rect",
        "from": {"data":"table"},
        "encode": {
          "enter": {
            "x": {"scale": "xscale", "field": "name"},
            "width": {"scale": "xscale", "band": 1},
            "y": {"scale": "yscale", "field": "eigen"},
            "y2": {"scale": "yscale", "value": 0}
          },
          "update": {
            "fill": {"value": "steelblue"}
          },
          "hover": {
            "fill": {"value": "red"}
          }
        }
      },
      {
        "type": "text",
        "encode": {
          "enter": {
            "align": {"value": "center"},
            "baseline": {"value": "bottom"},
            "fill": {"value": "#333"}
          },
          "update": {
            "x": {"scale": "xscale", "signal": "tooltip.name", "band": 0.5},
            "y": {"scale": "yscale", "signal": "tooltip.eigen", "offset": -2},
            "text": {"signal": "tooltip.eigen"},
            "fillOpacity": [
              {"test": "datum === tooltip", "value": 0},
              {"value": 1}
            ]
          }
        }
      }
    ]
  };
}