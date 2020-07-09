
// parametrise graph encoding for MDS plot
function createXYSpec(xyData, xyTable, width, height)
{

  // generate tooltip object for embedding in spec
  var tooltipString = "{";
  xyData.cols.forEach(x => tooltipString += `'${x}':datum['${x}'],`);
  tooltipString += "}";
  var tooltip = { "signal" : tooltipString };

  return {
    "$schema": "https://vega.github.io/schema/vega/v5.json",
    "description": "Testing ground for GlimmaV2",
    "width": xyData.counts == -1 ? (width*0.9) : (width * 0.45),
    "height": height * 0.35,
    "padding": 0,
    "title": {
      "text": xyData.title
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
          "values": xyTable,
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
        "domain": { "data": "source", "field": xyData.x },
        "range": "width"
      },
      {
        "name": "y",
        "type": "linear",
        "round": true,
        "nice": true,
        "zero": true,
        "domain": { "data": "source", "field": xyData.y },
        "range": "height"
      },
      {
        "name": "colour_scale",
        "type": "ordinal",
        "domain": [-1, 0, 1],
        "range": xyData.status_colours
      }
    ],
    "axes" : [
      {
        "scale": "x",
        "grid": true,
        "domain": false,
        "orient": "bottom",
        "tickCount": 5,
        "title": xyData.x
      },
      {
        "scale": "y",
        "grid": true,
        "domain": false,
        "orient": "left",
        "titlePadding": 5,
        "title": xyData.y
      }
    ],
    "marks": [
      {
        "name": "marks",
        "type": "symbol",
        "from": { "data": "source" },
        "encode": {
          "update": {
            "x": { "scale": "x", "field": xyData.x },
            "y": { "scale": "y", "field": xyData.y },
            "shape": "circle",
            "size" : 50,
            "strokeWidth": 0,
            "opacity": { "value": 0.6 },
            "fill": { "scale": "colour_scale", "field": "status" },
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
            "x": { "scale": "x", "field": xyData.x },
            "y": { "scale": "y", "field": xyData.y },
            "shape": "circle",
            "size" : 1,
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