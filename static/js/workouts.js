google.load('visualization', '1.0', {'packages':['corechart']});

function changeLookback() {
    var lookbackBox = document.getElementById("lookback_days");
    var newUrl = window.location.pathname + "?lookback_days=" + lookbackBox.value;
    alert(newUrl);
    window.location = newUrl;
}

function xyChart(kind, div_name, dates, ys, labels) {
    var data = new google.visualization.DataTable();
    data.addColumn("date", "Date");
    
    for (var c = 0; c < labels.length; c++) {
        data.addColumn("number", labels[c]);
    }

    for (var r = 0; r < dates.length; r++) {
        var row = [dates[r]];
        for (var c = 0; c < ys.length; c++) {
            row.push(ys[c][r]);
        }
        data.addRow(row);
    }

    var options = {
    };

    var ctors = {
        'Line': google.visualization.LineChart,
        'Scatter': google.visualization.ScatterChart
    };
    
    var chart = new ctors[kind](
        document.getElementById(div_name));

    chart.draw(data, options);
}
