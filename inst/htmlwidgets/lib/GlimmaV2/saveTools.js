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

function saveSubsetClick(selected, xyTable, countsMatrix)
{
  if (selected.length == 0)
  {
    if (confirm(`This will save the table and counts data for all ${xyTable.length} genes.`)) 
    {
      /* only include counts if it is provided */
      let data = countsMatrix==null ? 
        xyTable : xyTable.map( x => $.extend(x, countsMatrix[x.index]) );
      saveJSONArrayToCSV(data);
    }
  }
  else
  {
    let concatData = countsMatrix==null ?
      selected : selected.map( x => $.extend(x, countsMatrix[x.index]) );
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