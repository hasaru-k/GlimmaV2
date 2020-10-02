function addSavePlotButton(controlContainer, xy_obj, exp_obj, text="Save Plot") 
{
  // set up button elements
  var dropdownDiv = document.createElement("div");
  dropdownDiv.setAttribute("class", "dropdown");

  var dropdownButton = document.createElement("button");
  dropdownButton.setAttribute("class", "save-button");
  dropdownButton.innerHTML = text;

  var dropdownContent = document.createElement("div");
  dropdownContent.setAttribute("class", "dropdown-content");
  
  var pngSummaryBtn = addSaveButtonElement(xy_obj, text="Summary plot (PNG)", type='png');
  var svgSummaryBtn = addSaveButtonElement(xy_obj, text="Summary plot (SVG)", type='png');
  
  // add elements to container
  dropdownDiv.appendChild(dropdownButton);
  dropdownDiv.appendChild(dropdownContent);

  dropdownContent.appendChild(pngSummaryBtn);
  dropdownContent.appendChild(svgSummaryBtn);

  // add the expression buttons if expression plot is active
  if (exp_obj) {
    var pngExpressionBtn = addSaveButtonElement(exp_obj, text="Expression plot (PNG)", type='png');
    var svgExpressionBtn = addSaveButtonElement(exp_obj, text="Expression plot (SVG)", type='svg');
  
    dropdownContent.appendChild(pngExpressionBtn);
    dropdownContent.appendChild(svgExpressionBtn);
  }

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

function addSaveButtonElement(view_obj, text, type) {
  // create a save button element for the save dropdown
  var saveButton = document.createElement("a");
  saveButton.setAttribute("href", "#")
  saveButton.innerText = text;
  saveButton.onclick = function() {
    view_obj.toImageURL(type, scaleFactor=3).then(function (url) {
      var link = document.createElement('a');
      link.setAttribute('href', url);
      link.setAttribute('target', '_blank');
      link.setAttribute('download', 'vega-export.' + type);
      link.dispatchEvent(new MouseEvent('click'));
    });
  };
  return saveButton;
}

function saveTableClickListener(state, data)
{
  if (state.selected.length == 0)
  {
    if (confirm(`This will save the table and counts data for all ${data.xyTable.length} genes.`)) 
    {
      /* only include counts if it is provided */
      let arr = data.countsMatrix==null ? 
        data.xyTable : data.xyTable.map( x => $.extend(x, data.countsMatrix[x.index]) );
      saveJSONArrayToCSV(arr);
    }
  }
  else
  {
    let concatData = data.countsMatrix==null ?
      state.selected : state.selected.map( x => $.extend(x, data.countsMatrix[x.index]) );
    saveJSONArrayToCSV(concatData);
  }
}


function saveJSONArrayToCSV(jsonArray)
{
  let csvData = JSONArrayToCSV(jsonArray);
  var blob = new Blob([csvData], { type: "text/csv;charset=utf-8" });
  saveAs(blob, "glimmaTable.csv");
}


/* credit: https://stackoverflow.com/questions/8847766/how-to-convert-json-to-csv-format-and-store-in-a-variable */
function JSONArrayToCSV(array)
{
  var fields = Object.keys(array[0])
  var replacer = function(key, value) { return value === null ? '' : value } 
  var csv = array.map(function(row){
    return fields.map(function(fieldName){
      return JSON.stringify(row[fieldName], replacer)
    }).join(',')
  })
  csv.unshift(fields.join(',')) // add header column
  csv = csv.join('\r\n');
  return csv;
}