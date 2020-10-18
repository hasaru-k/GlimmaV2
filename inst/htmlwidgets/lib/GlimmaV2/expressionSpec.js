function createExpressionSpec(width, height, expColumns, sampleColours, samples)
{
    /* must match counts term in processExpression */
    expColumns.push("count");
    let tooltip = makeVegaTooltip(expColumns);
    return {
        "$schema": "https://vega.github.io/schema/vega/v5.json",
        "width": width*0.40,
        "height": height*0.35,
        "padding": {"left": 0, "top": 0, "right": 0, "bottom": 10},
        "autosize": {"type": "fit", "resize": true},
        "title": { "text": {"signal": "title_signal" }},
        "signals": 
                [ 
                    {
                        "name": "title_signal", 
                        "value": "" 
                    },
                    {
                        "name": "max_y_axis", 
                        "value": null,
                        "bind": { 
                                  "input": "number",
                                  "class": "max_y_axis"
                                }
                    },
                    {
                        "name": "max_count",
                        "value": 0,
                    },
                    {
                        "name": "max_y",
                        "update": " (max_y_axis < max_count) ? null : max_y_axis"
                    }
                ],
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
                "range": "height",
                "domainMax": {"signal": "max_y"}
            },
            {
                "name": "color",
                "type": "ordinal",
                "domain": sampleColours == -1 ? { "data": "table", "field": "group" } : samples,
                "range": sampleColours == -1 ? { "scheme": "category10" } : sampleColours
            },
        ],
        "axes": 
        [
            {
                "scale": "x",
                "orient": "bottom",
                "title": "group",
                "labelAngle": -45,
                "labelAlign": "right",
                "labelOffset": -3  
            },
            {
                "scale": "y",
                "grid": true,
                "orient": "left",
                "titlePadding": 5,
                "title": "expression"
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
                        "fill": { "scale": "color", "field": sampleColours == -1 ? "group" : "sample" },
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