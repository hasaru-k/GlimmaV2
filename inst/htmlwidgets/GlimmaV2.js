HTMLWidgets.widget({

  name: 'GlimmaV2',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance
 
    return {

      renderValue: function(x) {

        console.log(x);

        var widget = document.getElementById(el.id);
        var plotContainer = document.createElement("div");
        plotContainer.setAttribute("id", "plotContainer");
        var controlContainer = document.createElement("div");
        controlContainer.setAttribute("id", "controlContainer");
        widget.appendChild(plotContainer);
        widget.appendChild(controlContainer);


        var view;
        mdsData = HTMLWidgets.dataframeToD3(x.data.mdsData);

        // TODO: extract this data from dataframe
        dimList = ["dim1", "dim2", "dim3", "dim4", "dim5", "dim6"];
        sampleList = ["lane", "genotype"];

        render(createVegaSpec(dimList, sampleList, mdsData,
                width, height));

        // plotContainer as the container to the view
        function render(spec) {
          view = new vega.View(vega.parse(spec), {
            renderer: 'canvas',  // renderer (canvas or svg)
            container: '#' + plotContainer.getAttribute("id"),
            bind: '#' + controlContainer.getAttribute("id"),
            hover: true       // enable hover processing
          });
          return view.runAsync();
        }
        
        // save to PNG button
        var downloadButton = document.createElement("BUTTON");
        downloadButton.setAttribute("id", "savePNGBtn");
        downloadButton.innerHTML = "Save to PNG";
        downloadButton.onclick =
          function changeContent() {
            view.toImageURL('png').then(function (url) {
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

// parametrise a vega graph encoding
function createVegaSpec(dimList, sampleList, mdsData, width, height) {
  return {
    "$schema": "https://vega.github.io/schema/vega/v5.json",
    "description": "Testing ground for GlimmaV2",
    "width": width * 0.9,
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