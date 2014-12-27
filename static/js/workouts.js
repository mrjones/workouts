google.load('visualization', '1.0', {'packages':['corechart']});

function xyChart(kind, div_name, title, dates, ys) {
    var data = new google.visualization.DataTable();
    data.addColumn("date", "Date");
    data.addColumn("number", title);

    for (var i = 0; i < ys.length; i++) {
        data.addRow([dates[i], ys[i]]);
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
