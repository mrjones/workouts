google.load('visualization', '1.0', {'packages':['corechart']});

function mpwChart(mileage, dates) {
    var data = new google.visualization.DataTable();
    data.addColumn("date", "Date");
    data.addColumn("number", "Miles (last 7)");

    for (var i = 0; i < mileage.length; i++) {
        data.addRow([dates[i], mileage[i]]);
    }

    var options = {
    };
    
    var chart = new google.visualization.LineChart(
        document.getElementById('chart_div'));

    chart.draw(data, options);
}

function foo() {
    // Create the data table.
    var data = new google.visualization.DataTable();
    data.addColumn('string', 'Topping');
    data.addColumn('number', 'Slices');
    data.addRows([
        ['Double Pepperoni', 3],
        ['Triple Pepperoni', 1],
        ['Plain', 1],
        ['Sopresatta', 1],
        ['Pepperoni', 2]
    ]);

    // Set chart options
    var options = {'title':'How Much Pizza I Ate Last Night',
                   'width':400,
                   'height':300};

    // Instantiate and draw our chart, passing in some options.
    var chart = new google.visualization.PieChart(document.getElementById('chart_div'));
    chart.draw(data, options);
}
