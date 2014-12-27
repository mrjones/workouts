google.load('visualization', '1.0', {'packages':['corechart']});

function xyChart(kind, div_name, title, dates, ys) {
    var data = new google.visualization.DataTable();
    data.addColumn("date", "Date");
    
    for (var c = 0; c < ys.length; c++) {
        data.addColumn("number", title);
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
