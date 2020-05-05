
// parametrise graph encoding for MDS plot
function createXYSpec(xyData, width, height, x, y, cols) 
{

  // generate tooltip object for embedding in spec
  var tooltipString = "{";
  cols.forEach(x => tooltipString += `'${x}':datum['${x}'],`);
  tooltipString += "}";
  var tooltip = { "signal" : tooltipString };

  return {
    "$schema": "https://vega.github.io/schema/vega/v5.json",
    "description": "Testing ground for GlimmaV2",
    "width": width * 0.8,
    "height": height * 0.5,
    "padding": 10,
    "title": {
      "text": "MA Plot"
    },
    "signals":
      [
        {
          "name": "click", "value": null,
          "on": [ {"events": "mousedown", "update": "[datum, now()]" } ]
        }
      ],
    "data": 
      [
        {
          "name": "source",
          "values": xyData,
          "transform": [{
            "type": "formula",
            "expr": "datum.x",
            "as": "tooltip"
          }]
        },
        { "name": "selected_points" }
      ],
    "scales": [
      {
        "name": "x",
        "type": "linear",
        "round": true,
        "nice": true,
        "zero": true,
        "domain": { "data": "source", "field": x },
        "range": "width"
      },
      {
        "name": "y",
        "type": "linear",
        "round": true,
        "nice": true,
        "zero": true,
        "domain": { "data": "source", "field": y },
        "range": "height"
      },
      {
        "name": "colour_scale",
        "type": "ordinal",
        "domain": [-1, 0, 1],
        "range": ["dodgerblue", "lightslategray", "firebrick"]
      }
    ],
    "axes" : [
      {
        "scale": "x",
        "grid": true,
        "domain": false,
        "orient": "bottom",
        "tickCount": 5,
        "title": x
      },
      {
        "scale": "y",
        "grid": true,
        "domain": false,
        "orient": "left",
        "titlePadding": 5,
        "title": y
      }
    ],
    "marks": [
      {
        "name": "marks",
        "type": "symbol",
        "from": { "data": "source" },
        "encode": {
          "update": {
            "x": { "scale": "x", "field": x },
            "y": { "scale": "y", "field": y },
            "shape": "circle",
            "size" : 2,
            "strokeWidth": { "value": 1 },
            "opacity": { "value": 0.6 },
            "fill": { "scale": "colour_scale", "field": "colour" },
            "tooltip": tooltip
          }
        }
      },
      {
        "name": "selected_marks",
        "type": "symbol",
        "from": { "data": "selected_points" },
        "encode": {
          "update": {
            "x": { "scale": "x", "field": x },
            "y": { "scale": "y", "field": y },
            "shape": "circle",
            "size" : 2,
            "fill" : {"value" : "darkorange"},
            "strokeWidth": { "value": 1.5 },
            "opacity": { "value": 1 },
            "tooltip": tooltip
          }
        }
      },
    ]
  };
}