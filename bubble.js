

// !preview r2d3 data =data, d3_version = 4

// Based on https://bl.ocks.org/mbostock/4063269

// Initialization


svg.attr("font-family", "Arial Black")
  .attr("font-size", "25")
  .attr("text-anchor", "middle");
    
var svgSize = 1600;
var pack = d3.pack()
  .size([svgSize+500, svgSize])
  .padding(30);
    
var format = d3.format(",d");

var group = svg.append("g");

// Resize
r2d3.onResize(function(width, height) {
  var minSize = Math.min(width, height);
  var scale = minSize / svgSize;
  
  group.attr("transform", function(d) {
    return "" +
      "translate(" + (width - minSize) / 2 + "," + (height - minSize) / 2 + ")," +
      "scale(" + scale + "," + scale + ")";
  });
});

// Rendering
r2d3.onRender(function(data, svg, width, height, options) {

  group.selectAll("g").remove();

  var root = d3.hierarchy({children: data})
    .sum(function(d) { return d.value; })
    .each(function(d) {
      if (short = d.data.short) {
        var short, i = short.lastIndexOf(".");
        d.short = short;
      }
      if (id = d.data.id) {
        var id, i = id.lastIndexOf(".");
        d.id = id;
      }
    });
  var dacol = ["No data","Data science","Causal inference","data collection","data analysis","database Management","nonSQL databases","Relational databases","Data wrangling", "Data Imputation","Data Mining","Missing Data","Machine Learning","Deep learning","Network science","Programming", "SQL","Python","Data clean-up","QGIS","PCRaster","R statistical software","Rmarkdown","clustering","linear algebra","mathematical concepts","regression","gradient descent","feedforward neural networks","logistic regression","Bayesian statistics","Statistical models","Stochastic Modeling","Exploratory Data Analysis","JASP","MplusÂ","Haskell", "Visual Studio","Big data","Statistics","Visualizations","SPSS","Complex systems","Text mining","Artificial Intelligence","Dataverzameling", "dataverwerking", "data analyse","Statistiek"]
  var myColor = d3.scaleOrdinal().domain(dacol)
  .range(["#FFFFFF","#faedcd","#3859ad","#6f996c","#a0b587","#6d577a","#ccb793","#dcceb6","#ece5d8","#aca0b0","#c4bcc7",
"#ddd8de","#b4d3f6","#f258ea","#f687f0","#fab6f6","#A8D1E7","#B3DBD8","#FEE5E0","#FFBFC5","#F6BD60",
"#F7EDE2","#F7EDE2","#F5CAC3","#84A59D","#F28482","#82A3AC",'#A9C7C5',"#BFD8D2","#E2CFC9","#D1ACA5",
"#D2D0F5","#D7C6D8","#F2E7F8","#C0B6FF","#FAF7FB","#EEE8E4","#F3DDD4","#F1C6B8","#E1A292","#ECF8F8",
"#EEE4E1","#E7D8C9","#E6BEAE","#B9B0CF","#e8f0ff","#e8f0f0"])
 //var color = d3.scaleOrdinal(d3.schemeCategory20c)

  
  var node = group.selectAll(".node")
    .data(pack(root).leaves())
    .enter().append("g")
      .attr("package", "node")
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

  node.append("circle")
      .transition()
      .ease(d3.easeExp)
      .attr("id", function(d) { return d.id; })
      .transition()
      .duration(400)
      .ease(d3.easeExp)
      .attr("r", function(d) { return d.r; })
      .style("fill", function(d){return myColor(d.id) });
     // .style("fill", function(d) { return color(d.id); });
  node.on("click", function(d) {
        var message = d.id;
        Shiny.onInputChange("click_event",message)
    });
      
      
  node.append("clipPath")
        .attr("id", function(d) { return "clip-" + d.id; })
        .append("use")
        .attr("xlink:href", function(d) { return "#" + d.id; });
      
  node.append("text")
    .attr("clip-path", function(d) { return "url(#clip-" + d.id + ")"; })
    .selectAll("tspan")
    .data(function(d) { return d.short.split(/(?=[A-Z][^A-Z])/g); })
    .enter()
    .append("tspan")
      .transition().duration(400)
      .attr("x", 0)
      .attr("y", function(d, i, nodes) { return 13 + (i - nodes.length / 2 - 0.5) * 12; })
      .transition()
      .duration(400)
      .text(function(d) { return d; });

  node.append("title")
      .text(function(d) { return d.id + "\n Total of: " + format(d.value)+" courses"+"\n Click on me to see details"; });
  r2d3.resize(width, height);
});