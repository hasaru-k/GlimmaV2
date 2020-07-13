function createExpressionSpec(width, height)
{
    return {
        "$schema": "https://vega.github.io/schema/vega/v5.json",
        "width": width*0.45,
        "height": height*0.35,
        "padding": 5,
        "data": [ {"name": "table"} ],
        "scales": [
            {
                "name": "x",
                "type": "band",
                "padding":1,
                "domain": {"data": "table", "field": "group"},
                "range": "width"
            },
            {
                "name": "y",
                "domain": {"data": "table", "field": "count"},
                "range": "height"
            }],
        "axes": [
            {"scale": "x", "orient": "bottom", "title": "group"},
            {
                "scale": "y",
                "grid": true,
                "orient": "left",
                "titlePadding": 5,
                "title": "count"
            }],
        "marks": 
        [{
                "name": "marks",
                "type": "symbol",
                "from": {"data": "table"},
                "encode": {
                "update": {
                    "x": {"scale": "x", "field": "group"},
                    "y": {"scale": "y", "field": "count"},
                    "shape": {"value": "circle"},
                    "strokeWidth": {"value": 2},
                    "opacity": {"value": 0.5},
                    "stroke": {"value": "#4682b4"},
                    "fill": {"value": "transparent"}
                }
                }
            }]
    };

}