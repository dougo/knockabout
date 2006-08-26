svgNS = "http://www.w3.org/2000/svg";
xlinkNS = "http://www.w3.org/1999/xlink";
PaleGreen3 = "#7CCD7C";
SQRT3 = Math.sqrt(3);
SQRT5 = Math.sqrt(5);

// Hexagon centered at the origin, height = 2, width = sqrt(5)/2 + 1,
// each edge is sqrt(5)/2.
hexPoints =
  SQRT5/4        + ",-1 " +	// northeast corner
  (SQRT5/4+1/2)  + ",0 " +	// east
  SQRT5/4        + ",1 " +	// southeast
  -SQRT5/4       + ",1 " +	// southwest
  -(SQRT5/4+1/2) + ",0 " +	// west
  -SQRT5/4       + ",-1";	// northwest

// Die shapes that fit into the height-2 hexagon centered at the origin.
trianglePoints = "0,-0.85 0.7,0.5 -0.7,0.5";
squarePoints = "-0.65,-0.65 0.65,-0.65 0.65,0.65 -0.65,0.65";
diamondPoints = "0,-0.9 0.7,0 0,0.9 -0.7,0";

function makeGroup(parent) {
  return parent.appendChild(document.createElementNS(svgNS, "g"));
}

function makeHex() {
  var hex = document.createElementNS(svgNS, "polygon");
  hex.setAttribute("points", hexPoints);
  return hex;
}

function moveXY(node, x, y) {
  node.setAttribute("transform", "translate(" + x + "," + y + ")");
  return node;
}

function moveRC(node, row, col) {
  return moveXY(node, (SQRT5/4+1.05)*col, 2*row - col);
  // TO DO: why the .05??
}

function isGutterRC(row, col) {
  return (row == 0 || row == 12 || col == 0 || col == 12 ||
	  row-col == 6 || row-col == -6);
}

function makeBoard(parent) {
  var board = makeGroup(parent);
  moveXY(board, 6, 10);
  var gutter = makeHex();
  gutter.setAttribute("transform", "translate(10,6) rotate(30) scale(11)");
  // TO DO: why is the translate necessary?  Shouldn't it inherit from board?
  // And why are the coordinates reversed?!
  // And shouldn't the scale only need to be like 7?
  gutter.setAttribute("stroke", "black");
  gutter.setAttribute("stroke-width", 0.04);
  gutter.setAttribute("fill-opacity", 0);
  board.appendChild(gutter);

  for (col = 0; col < 13; col++) {
    for (row = Math.max(0, col - 6); row < Math.min(col + 7, 13); row++) {
      var hex = makeHex();
      hex.setAttribute("fill", PaleGreen3);
      hex.setAttribute("stroke", "black");
      hex.setAttribute("stroke-width", 0.1);
      moveRC(hex, row, col);
      if (isGutterRC(row, col))
	// Gutter space, shrink to half size.
	hex.setAttribute("transform", hex.getAttribute("transform")
			 + "scale(0.5)");
      board.appendChild(hex);
    }
  }

  return board;
}

function dieShapePoints(size) {
  switch (size) {
  case 4: return trianglePoints;
  case 6: return squarePoints;
  case 8: return diamondPoints;
  }
}

function makeDieShape(owner, size) {
  var shape = document.createElementNS(svgNS, "polygon");
  shape.setAttribute("points", dieShapePoints(size));
  if (owner == 0) {
    shape.setAttribute("fill", "yellow");
    shape.setAttribute("stroke", "black");
  }
  shape.setAttribute("stroke-width", 0.1);
  return shape;
}

function makeDieText(owner, face) {
  var text = document.createElementNS(svgNS, "text");
  text.setAttribute("text-anchor", "middle");
  text.setAttribute("font-family", "sans-serif");
  text.setAttribute("font-weight", "bold");
  text.setAttribute("font-size", 0.9);
  text.setAttribute("y", 0.3);
  // TO DO: figure out text color from background color,
  // i.e. light on dark or dark on light
  if (owner == 1) text.setAttribute("fill", "white");
  text.textContent = face;
  return text;
}

function makeDice(parent) {
  var dice = makeGroup(parent);

  function makeDie(owner, row, col, size, face) {
    var die = document.createElementNS(svgNS, "g");
    die.appendChild(makeDieShape(owner, size));
    die.appendChild(makeDieText(owner, face));
    moveRC(die, row, col);
    return dice.appendChild(die);
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

  return dice;
}

root = document.documentElement;
board = makeBoard(root);
dice = makeDice(board);
