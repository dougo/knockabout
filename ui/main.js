svgNS = "http://www.w3.org/2000/svg";
xlinkNS = "http://www.w3.org/1999/xlink";
board = document.getElementById("board");

// Use a node by name.
function useNode(name) {
  var node = document.createElementNS(svgNS, "use");
  node.setAttributeNS(xlinkNS, "href", "#" + name);
  return node;
}

// Add an item to the board.
function addItem(item, row, col) {
  var x = 200 + 265*col;
  var y = 1100 + 153*(2*row - col);
  item.setAttribute("transform", "translate(" + x + "," + y + ")");
  board.appendChild(item);
  return item;
}

function addSpace(row, col) {
  addItem(useNode("space"), row, col);
}

for (col = 0; col < 13; col++) {
  for (row = Math.max(0, col - 6); row < Math.min(col + 7, 13); row++) {
    addSpace(row, col);
  }
}


function dieShapePoints(size) {
  switch (size) {
  case 4: return "   0,-153  132,76   -132,76";           // triangle
  case 6: return "-100,-100  100,-100  100,100 -100,100"; // square
  case 8: return "   0,-153  100,0       0,153 -100,0";   // diamond
  }
}

function makeDieShape(owner, size) {
  var shape = document.createElementNS(svgNS, "polygon");
  shape.setAttribute("points", dieShapePoints(size));
  if (owner == 0) {
    shape.setAttribute("fill", "yellow");
    shape.setAttribute("stroke", "black");
    shape.setAttribute("stroke-width", 3);
  }
  return shape;
}

function makeDieText(owner, face) {
  var text = document.createElementNS(svgNS, "text");
  text.setAttribute("text-anchor", "middle");
  text.setAttribute("font-family", "sans-serif");
  text.setAttribute("font-weight", "bold");
  text.setAttribute("font-size", 120);
  text.setAttribute("y", 45);
  if (owner == 1) text.setAttribute("fill", "white");
  text.appendChild(document.createTextNode(face));
  return text;
}

function makeDie(owner, row, col, size, face) {
  var die = document.createElementNS(svgNS, "g");
  var shape = makeDieShape(owner, size);
  die.appendChild(shape);
  var text = makeDieText(owner, face);
  die.appendChild(text);
  addItem(die, row, col);
}

makeDie(0, 4, 3, 4, 1);
makeDie(0, 5, 5, 4, 1);
makeDie(0, 6, 7, 4, 1);
makeDie(0, 7, 9, 4, 1);
makeDie(0, 3, 4, 6, 2);
makeDie(0, 4, 6, 6, 2);
makeDie(0, 5, 8, 6, 2);
makeDie(0, 2, 5, 8, 3);
makeDie(0, 3, 7, 8, 3);

makeDie(1, 5, 3, 4, 1);
makeDie(1, 6, 5, 4, 1);
makeDie(1, 7, 7, 4, 1);
makeDie(1, 8, 9, 4, 1);
makeDie(1, 7, 4, 6, 2);
makeDie(1, 8, 6, 6, 2);
makeDie(1, 9, 8, 6, 2);
makeDie(1, 9, 5, 8, 3);
makeDie(1, 10, 7, 8, 3);
