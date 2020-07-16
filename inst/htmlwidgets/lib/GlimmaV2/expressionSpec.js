function createExpressionSpec(width, height, expColumns)
{
    /* must match counts term in processExpression */
    expColumns.push("count");
    let tooltip = makeVegaTooltip(expColumns);
    return {
        "$schema": "https://vega.github.io/schema/vega/v5.json",
        "width": width*0.40,
        "height": height*0.35,
        "padding": 0,
        "autosize": {"type": "fit", "resize": true},
        "title": { "text": {"signal": "title_signal" }},
        "signals": [ {"name": "title_signal", "value": "" } ],
        "data": [ {"name": "table"} ],
        "scales": 
        [
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
            },
            {
                "name": "color",
                "type": "ordinal",
                "domain": { "data": "table", "field": "sample" },
                "range": { "scheme": "category20" }
            },
        ],
        "axes": 
        [
            {"scale": "x", "orient": "bottom", "title": "group"},
            {
                "scale": "y",
                "grid": true,
                "orient": "left",
                "titlePadding": 5,
                "title": "count"
            }
        ],
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
                        "fill": { "scale": "color", "field": "sample" },
                        "strokeWidth": {"value": 1},
                        "opacity": {"value": 0.6},
                        "size": {"value": 100},
                        "stroke": {"value": "#575757"},
                        "tooltip": tooltip
                    }
                }
            }]
    };

}