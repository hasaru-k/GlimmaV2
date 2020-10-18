function addSavePlotButton(controlContainer, xy_obj, exp_obj=null, 
  text="Save Plot", summaryText="Summary plot", expressionText="Expression plot") 
{
  // set up button elements
  var dropdownDiv = document.createElement("div");
  dropdownDiv.setAttribute("class", "dropdown");

  var dropdownButton = document.createElement("button");
  dropdownButton.setAttribute("class", "save-button");
  dropdownButton.innerHTML = text;

  var dropdownContent = document.createElement("div");
  dropdownContent.setAttribute("class", "dropdown-content");
  
  var pngSummaryBtn = addSaveButtonElement(xy_obj, text=summaryText+" (PNG)", type='png');
  var svgSummaryBtn = addSaveButtonElement(xy_obj, text=summaryText+" (SVG)", type='png');
  
  // add elements to container
  dropdownDiv.appendChild(dropdownButton);
  dropdownDiv.appendChild(dropdownContent);

  dropdownContent.appendChild(pngSummaryBtn);
  dropdownContent.appendChild(svgSummaryBtn);

  // add the expression buttons if expression plot is active
  if (exp_obj) {
    var pngExpressionBtn = addSaveButtonElement(exp_obj, text=expressionText+" (PNG)", type='png');
    var svgExpressionBtn = addSaveButtonElement(exp_obj, text=expressionText+" (SVG)", type='svg');
  
    dropdownContent.appendChild(pngExpressionBtn);
    dropdownContent.appendChild(svgExpressionBtn);
  }

  // set up dropdown action
  dropdownButton.onclick = function() {
    dropdownOnClick(dropdownContent);
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

function dropdownOnClick(dropdownContent) {
  var dropdowns = document.getElementsByClassName("dropdown-content");
  for (const dropdown_i of dropdowns){
    if (dropdown_i.classList.contains("show")) {
      dropdown_i.classList.remove("show");
    }
  }
  dropdownContent.classList.toggle("show");
}

function addSaveButtonElement(view_obj, text, type) {
  // create a save button element for the save dropdown
  var saveButton = document.createElement("a");
  saveButton.setAttribute("href", "#");
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

function addSaveDataElement(state, data, saveAllText, saveSelectText) {
  buttonContainer = document.getElementsByClassName("saveSubset")[0].parentElement;

  var dropdownDiv = document.createElement("div");
  dropdownDiv.setAttribute("class", "dropdown");

  var dropdownContent = document.createElement("div");
  dropdownContent.setAttribute("class", "dropdown-content dataDropdown");

  var saveSelectBtn = document.createElement("a");
  saveSelectBtn.setAttribute("href", "#");
  saveSelectBtn.setAttribute("class", "saveSelectButton");
  saveSelectBtn.innerText = saveSelectText;
  saveSelectBtn.onclick = function() {
    saveTableClickListener(state, data, false);
  };

  var saveAllBtn = document.createElement("a");
  saveAllBtn.setAttribute("href", "#");
  saveAllBtn.innerText = saveAllText;
  saveAllBtn.onclick = function() {
    saveTableClickListener(state, data, true);
  };

  dropdownContent.appendChild(saveSelectBtn);
  dropdownContent.appendChild(saveAllBtn);
  buttonContainer.appendChild(dropdownContent);
} 

function saveTableClickListener(state, data, save_all)
{
  if (save_all)
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